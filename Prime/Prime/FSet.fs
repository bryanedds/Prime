// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.
// Adapted by Prime as FSet to enable fast set comparisons conducive to an MVU context.

namespace Prime

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.Text
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Collections

// A functional language implementation of binary trees

[<NoEquality; NoComparison>]
[<AllowNullLiteral>]
type FSetTree<'T>(k: 'T, h: int) =
    member _.Height = h
    member _.Key = k
    new(k: 'T) = FSetTree(k, 1)

[<NoEquality; NoComparison>]
[<Sealed>]
[<AllowNullLiteral>]
type internal FSetTreeNode<'T>(v: 'T, left: FSetTree<'T>, right: FSetTree<'T>, h: int) =
    inherit FSetTree<'T>(v, h)
    member _.Left = left
    member _.Right = right

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSetTree =

    let empty = null

    let inline isEmpty (t: FSetTree<'T>) =
        isNull t

    let inline private asNode (value: FSetTree<'T>) : FSetTreeNode<'T> =
        value :?> FSetTreeNode<'T>

    let rec countAux (t: FSetTree<'T>) acc =
        if isEmpty t then
            acc
        else if t.Height = 1 then
            acc + 1
        else
            let tn = asNode t
            countAux tn.Left (countAux tn.Right (acc + 1))

    let count s =
        countAux s 0

#if TRACE_SETS_AND_MAPS
    let mutable traceCount = 0
    let mutable numOnes = 0
    let mutable numNodes = 0
    let mutable numAdds = 0
    let mutable numRemoves = 0
    let mutable numLookups = 0
    let mutable numUnions = 0
    let mutable totalSizeOnNodeCreation = 0.0
    let mutable totalSizeOnSetAdd = 0.0
    let mutable totalSizeOnSetLookup = 0.0

    let report () =
        traceCount <- traceCount + 1

        if traceCount % 10000 = 0 then
            System.Console.WriteLine(
                "#SetOne = {0}, #SetNode = {1}, #Add = {2}, #Remove = {3}, #Unions = {4}, #Lookups = {5}, avSetSizeOnNodeCreation = {6}, avSetSizeOnSetCreation = {7}, avSetSizeOnSetLookup = {8}",
                numOnes,
                numNodes,
                numAdds,
                numRemoves,
                numUnions,
                numLookups,
                (totalSizeOnNodeCreation / float (numNodes + numOnes)),
                (totalSizeOnSetAdd / float numAdds),
                (totalSizeOnSetLookup / float numLookups)
            )

    let FSetTree n =
        report ()
        numOnes <- numOnes + 1
        totalSizeOnNodeCreation <- totalSizeOnNodeCreation + 1.0
        FSetTree n

    let FSetTreeNode (x, l, r, h) =
        report ()
        numNodes <- numNodes + 1
        let n = FSetTreeNode(x, l, r, h)
        totalSizeOnNodeCreation <- totalSizeOnNodeCreation + float (count n)
        n
#endif

    let inline height (t: FSetTree<'T>) =
        if isEmpty t then 0 else t.Height

    [<Literal>]
    let private tolerance = 2

    let mk l k r : FSetTree<'T> =
        let hl = height l
        let hr = height r
        let m = if hl < hr then hr else hl

        if m = 0 then // m=0 ~ isEmpty l && isEmpty r
            FSetTree k
        else
            FSetTreeNode(k, l, r, m + 1) :> FSetTree<'T>

    let rebalance t1 v t2 =
        let t1h = height t1
        let t2h = height t2

        if t2h > t1h + tolerance then // right is heavier than left
            let t2' = asNode (t2)
            // one of the nodes must have height > height t1 + 1
            if height t2'.Left > t1h + 1 then // balance left: combination
                let t2l = asNode (t2'.Left)
                mk (mk t1 v t2l.Left) t2l.Key (mk t2l.Right t2'.Key t2'.Right)
            else // rotate left
                mk (mk t1 v t2'.Left) t2.Key t2'.Right
        else if t1h > t2h + tolerance then // left is heavier than right
            let t1' = asNode (t1)
            // one of the nodes must have height > height t2 + 1
            if height t1'.Right > t2h + 1 then
                // balance right: combination
                let t1r = asNode (t1'.Right)
                mk (mk t1'.Left t1.Key t1r.Left) t1r.Key (mk t1r.Right v t2)
            else
                mk t1'.Left t1'.Key (mk t1'.Right v t2)
        else
            mk t1 v t2

    let rec add (comparer: IComparer<'T>) k (t: FSetTree<'T>) : FSetTree<'T> =
        if isEmpty t then
            FSetTree k
        else
            let c = comparer.Compare(k, t.Key)

            if t.Height = 1 then
                // nb. no check for rebalance needed for small trees, also be sure to reuse node already allocated
                if c < 0 then
                    FSetTreeNode(k, empty, t, 2) :> FSetTree<'T>
                elif c = 0 then
                    t
                else
                    FSetTreeNode(k, t, empty, 2) :> FSetTree<'T>
            else
                let tn = asNode t

                if c < 0 then
                    rebalance (add comparer k tn.Left) tn.Key tn.Right
                elif c = 0 then
                    t
                else
                    rebalance tn.Left tn.Key (add comparer k tn.Right)

    let rec balance comparer (t1: FSetTree<'T>) k (t2: FSetTree<'T>) =
        // Given t1 < k < t2 where t1 and t2 are "balanced",
        // return a balanced tree for <t1, k, t2>.
        // Recall: balance means subtrees heights differ by at most "tolerance"
        if isEmpty t1 then
            add comparer k t2 // drop t1 = empty
        elif isEmpty t2 then
            add comparer k t1 // drop t2 = empty
        else if t1.Height = 1 then
            add comparer k (add comparer t1.Key t2)
        else
            let t1n = asNode t1

            if t2.Height = 1 then
                add comparer k (add comparer t2.Key t1)
            else
                let t2n = asNode t2
                // Have:  (t1l < k1 < t1r) < k < (t2l < k2 < t2r)
                // Either (a) h1, h2 differ by at most 2 - no rebalance needed.
                //        (b) h1 too small, i.e. h1+2 < h2
                //        (c) h2 too small, i.e. h2+2 < h1
                if t1n.Height + tolerance < t2n.Height then
                    // case: b, h1 too small
                    // push t1 into low side of t2, may increase height by 1 so rebalance
                    rebalance (balance comparer t1 k t2n.Left) t2n.Key t2n.Right
                elif t2n.Height + tolerance < t1n.Height then
                    // case: c, h2 too small
                    // push t2 into high side of t1, may increase height by 1 so rebalance
                    rebalance t1n.Left t1n.Key (balance comparer t1n.Right k t2)
                else
                    // case: a, h1 and h2 meet balance requirement
                    mk t1 k t2

    let rec split (comparer: IComparer<'T>) pivot (t: FSetTree<'T>) =
        // Given a pivot and a set t
        // Return { x in t s.t. x < pivot }, pivot in t?, { x in t s.t. x > pivot }
        if isEmpty t then
            empty, false, empty
        else if t.Height = 1 then
            let c = comparer.Compare(t.Key, pivot)

            if c < 0 then t, false, empty // singleton under pivot
            elif c = 0 then empty, true, empty // singleton is    pivot
            else empty, false, t // singleton over  pivot
        else
            let tn = asNode t
            let c = comparer.Compare(pivot, tn.Key)

            if c < 0 then // pivot t1
                let t11Lo, havePivot, t11Hi = split comparer pivot tn.Left
                t11Lo, havePivot, balance comparer t11Hi tn.Key tn.Right
            elif c = 0 then // pivot is k1
                tn.Left, true, tn.Right
            else // pivot t2
                let t12Lo, havePivot, t12Hi = split comparer pivot tn.Right
                balance comparer tn.Left tn.Key t12Lo, havePivot, t12Hi

    let rec spliceOutSuccessor (t: FSetTree<'T>) =
        if isEmpty t then
            failwith "internal error: FSet.spliceOutSuccessor"
        else if t.Height = 1 then
            t.Key, empty
        else
            let tn = asNode t

            if isEmpty tn.Left then
                tn.Key, tn.Right
            else
                let k3, l' = spliceOutSuccessor tn.Left in k3, mk l' tn.Key tn.Right

    let rec remove (comparer: IComparer<'T>) k (t: FSetTree<'T>) =
        if isEmpty t then
            t
        else
            let c = comparer.Compare(k, t.Key)

            if t.Height = 1 then
                if c = 0 then empty else t
            else
                let tn = asNode t

                if c < 0 then
                    rebalance (remove comparer k tn.Left) tn.Key tn.Right
                elif c = 0 then
                    if isEmpty tn.Left then
                        tn.Right
                    elif isEmpty tn.Right then
                        tn.Left
                    else
                        let sk, r' = spliceOutSuccessor tn.Right
                        mk tn.Left sk r'
                else
                    rebalance tn.Left tn.Key (remove comparer k tn.Right)

    let rec mem (comparer: IComparer<'T>) k (t: FSetTree<'T>) =
        if isEmpty t then
            false
        else
            let c = comparer.Compare(k, t.Key)

            if t.Height = 1 then
                (c = 0)
            else
                let tn = asNode t

                if c < 0 then mem comparer k tn.Left
                elif c = 0 then true
                else mem comparer k tn.Right

    let rec iter f (t: FSetTree<'T>) =
        if isEmpty t then
            ()
        else if t.Height = 1 then
            f t.Key
        else
            let tn = asNode t
            iter f tn.Left
            f tn.Key
            iter f tn.Right

    let rec foldBackOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (t: FSetTree<'T>) x =
        if isEmpty t then
            x
        else if t.Height = 1 then
            f.Invoke(t.Key, x)
        else
            let tn = asNode t
            foldBackOpt f tn.Left (f.Invoke(tn.Key, (foldBackOpt f tn.Right x)))

    let foldBack f m x =
        foldBackOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m x

    let rec foldOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) x (t: FSetTree<'T>) =
        if isEmpty t then
            x
        else if t.Height = 1 then
            f.Invoke(x, t.Key)
        else
            let tn = asNode t
            let x = foldOpt f x tn.Left in
            let x = f.Invoke(x, tn.Key)
            foldOpt f x tn.Right

    let fold f x m =
        foldOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) x m

    let rec forall f (t: FSetTree<'T>) =
        if isEmpty t then
            true
        else if t.Height = 1 then
            f t.Key
        else
            let tn = asNode t
            f tn.Key && forall f tn.Left && forall f tn.Right

    let rec exists f (t: FSetTree<'T>) =
        if isEmpty t then
            false
        else if t.Height = 1 then
            f t.Key
        else
            let tn = asNode t
            f tn.Key || exists f tn.Left || exists f tn.Right

    let subset comparer a b =
        forall (fun x -> mem comparer x b) a

    let properSubset comparer a b =
        forall (fun x -> mem comparer x b) a
        && exists (fun x -> not (mem comparer x a)) b

    let rec filterAux comparer f (t: FSetTree<'T>) acc =
        if isEmpty t then
            acc
        else if t.Height = 1 then
            if f t.Key then
                add comparer t.Key acc
            else
                acc
        else
            let tn = asNode t

            let acc =
                if f tn.Key then
                    add comparer tn.Key acc
                else
                    acc

            filterAux comparer f tn.Left (filterAux comparer f tn.Right acc)

    let filter comparer f s =
        filterAux comparer f s empty

    let rec diffAux comparer (t: FSetTree<'T>) acc =
        if isEmpty acc then
            acc
        else if isEmpty t then
            acc
        else if t.Height = 1 then
            remove comparer t.Key acc
        else
            let tn = asNode t
            diffAux comparer tn.Left (diffAux comparer tn.Right (remove comparer tn.Key acc))

    let diff comparer a b =
        diffAux comparer b a

    let rec union comparer (t1: FSetTree<'T>) (t2: FSetTree<'T>) =
        // Perf: tried bruteForce for low heights, but nothing significant
        if isEmpty t1 then
            t2
        elif isEmpty t2 then
            t1
        else if t1.Height = 1 then
            add comparer t1.Key t2
        else if t2.Height = 1 then
            add comparer t2.Key t1
        else
            let t1n = asNode t1
            let t2n = asNode t2 // (t1l < k < t1r) AND (t2l < k2 < t2r)
            // Divide and Conquer:
            //   Suppose t1 is largest.
            //   Split t2 using pivot k1 into lo and hi.
            //   Union disjoint subproblems and then combine.
            if t1n.Height > t2n.Height then
                let lo, _, hi = split comparer t1n.Key t2 in

                balance comparer (union comparer t1n.Left lo) t1n.Key (union comparer t1n.Right hi)
            else
                let lo, _, hi = split comparer t2n.Key t1 in

                balance comparer (union comparer t2n.Left lo) t2n.Key (union comparer t2n.Right hi)

    let rec intersectionAux comparer b (t: FSetTree<'T>) acc =
        if isEmpty t then
            acc
        else if t.Height = 1 then
            if mem comparer t.Key b then
                add comparer t.Key acc
            else
                acc
        else
            let tn = asNode t
            let acc = intersectionAux comparer b tn.Right acc

            let acc =
                if mem comparer tn.Key b then
                    add comparer tn.Key acc
                else
                    acc

            intersectionAux comparer b tn.Left acc

    let intersection comparer a b =
        intersectionAux comparer b a empty

    let partition1 comparer f k (acc1, acc2) =
        if f k then
            (add comparer k acc1, acc2)
        else
            (acc1, add comparer k acc2)

    let rec partitionAux comparer f (t: FSetTree<'T>) acc =
        if isEmpty t then
            acc
        else if t.Height = 1 then
            partition1 comparer f t.Key acc
        else
            let tn = asNode t
            let acc = partitionAux comparer f tn.Right acc
            let acc = partition1 comparer f tn.Key acc
            partitionAux comparer f tn.Left acc

    let partition comparer f s =
        partitionAux comparer f s (empty, empty)

    let rec minimumElementAux (t: FSetTree<'T>) n =
        if isEmpty t then
            n
        else if t.Height = 1 then
            t.Key
        else
            let tn = asNode t
            minimumElementAux tn.Left tn.Key

    and minimumElementOpt (t: FSetTree<'T>) =
        if isEmpty t then
            None
        else if t.Height = 1 then
            Some t.Key
        else
            let tn = asNode t
            Some(minimumElementAux tn.Left tn.Key)

    and maximumElementAux (t: FSetTree<'T>) n =
        if isEmpty t then
            n
        else if t.Height = 1 then
            t.Key
        else
            let tn = asNode t
            maximumElementAux tn.Right tn.Key

    and maximumElementOpt (t: FSetTree<'T>) =
        if isEmpty t then
            None
        else if t.Height = 1 then
            Some t.Key
        else
            let tn = asNode t
            Some(maximumElementAux tn.Right tn.Key)

    let minimumElement s =
        match minimumElementOpt s with
        | Some k -> k
        | None -> invalidArg "s" "FSet contains no elements."

    let maximumElement s =
        match maximumElementOpt s with
        | Some k -> k
        | None -> invalidArg "s" "FSet contains no elements."

    // Imperative left-to-right iterators.
    [<NoEquality; NoComparison>]
    type SetIterator<'T> when 'T: comparison =
        {
            mutable stack: FSetTree<'T> list // invariant: always collapseLHS result
            mutable started: bool // true when MoveNext has been called
        }

    // collapseLHS:
    // a) Always returns either [] or a list starting with SetOne.
    // b) The "fringe" of the set stack is unchanged.
    let rec collapseLHS (stack: FSetTree<'T> list) =
        match stack with
        | [] -> []
        | x :: rest ->
            if isEmpty x then
                collapseLHS rest
            else if x.Height = 1 then
                stack
            else
                let xn = asNode x
                collapseLHS (xn.Left :: FSetTree xn.Key :: xn.Right :: rest)

    let mkIterator s =
        {
            stack = collapseLHS [ s ]
            started = false
        }

    let notStarted () =
        raise (InvalidOperationException("Enumeration not started."))

    let alreadyFinished () =
        raise (InvalidOperationException("Enumeration already finished."))

    let current i =
        if i.started then
            match i.stack with
            | k :: _ -> k.Key
            | [] -> alreadyFinished ()
        else
            notStarted ()

    let unexpectedStackForMoveNext () =
        failwith "Please report error: FSet iterator, unexpected stack for moveNext"

    let unexpectedstateInSetTreeCompareStacks () =
        failwith "unexpected state in FSetTree.compareStacks"

    let rec moveNext i =
        if i.started then
            match i.stack with
            | [] -> false
            | t :: rest ->
                if t.Height = 1 then
                    i.stack <- collapseLHS rest
                    not i.stack.IsEmpty
                else
                    unexpectedStackForMoveNext ()
        else
            i.started <- true // The first call to MoveNext "starts" the enumeration.
            not i.stack.IsEmpty

    let mkIEnumerator s =
        let mutable i = mkIterator s

        { new IEnumerator<_> with
            member _.Current = current i
          interface IEnumerator with
              member _.Current = box (current i)

              member _.MoveNext() =
                  moveNext i

              member _.Reset() =
                  i <- mkIterator s
          interface System.IDisposable with
              member _.Dispose() =
                  ()
        }

    /// FSet comparison.  Note this can be expensive.
    let rec compareStacks (comparer: IComparer<'T>) (l1: FSetTree<'T> list) (l2: FSetTree<'T> list) : int =
        let cont () =
            match l1, l2 with
            | (x1 :: t1), _ when not (isEmpty x1) ->
                if x1.Height = 1 then
                    compareStacks comparer (empty :: FSetTree x1.Key :: t1) l2
                else
                    let x1n = asNode x1

                    compareStacks
                        comparer
                        (x1n.Left :: (FSetTreeNode(x1n.Key, empty, x1n.Right, 0) :> FSetTree<'T>) :: t1)
                        l2
            | _, (x2 :: t2) when not (isEmpty x2) ->
                if x2.Height = 1 then
                    compareStacks comparer l1 (empty :: FSetTree x2.Key :: t2)
                else
                    let x2n = asNode x2

                    compareStacks
                        comparer
                        l1
                        (x2n.Left :: (FSetTreeNode(x2n.Key, empty, x2n.Right, 0) :> FSetTree<'T>) :: t2)
            | _ -> unexpectedstateInSetTreeCompareStacks ()

        match l1, l2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | (x1 :: t1), (x2 :: t2) ->
            if isEmpty x1 then
                if isEmpty x2 then
                    compareStacks comparer t1 t2
                else
                    cont ()
            elif isEmpty x2 then
                cont ()
            else if x1.Height = 1 then
                if x2.Height = 1 then
                    let c = comparer.Compare(x1.Key, x2.Key)

                    if c <> 0 then
                        c
                    else
                        compareStacks comparer t1 t2
                else
                    let x2n = asNode x2

                    if isEmpty x2n.Left then
                        let c = comparer.Compare(x1.Key, x2n.Key)

                        if c <> 0 then
                            c
                        else
                            compareStacks comparer (empty :: t1) (x2n.Right :: t2)
                    else
                        cont ()
            else
                let x1n = asNode x1

                if isEmpty x1n.Left then
                    if x2.Height = 1 then
                        let c = comparer.Compare(x1n.Key, x2.Key)

                        if c <> 0 then
                            c
                        else
                            compareStacks comparer (x1n.Right :: t1) (empty :: t2)
                    else
                        let x2n = asNode x2

                        if isEmpty x2n.Left then
                            let c = comparer.Compare(x1n.Key, x2n.Key)

                            if c <> 0 then
                                c
                            else
                                compareStacks comparer (x1n.Right :: t1) (x2n.Right :: t2)
                        else
                            cont ()
                else
                    cont ()

    let compare comparer (t1: FSetTree<'T>) (t2: FSetTree<'T>) =
        if isEmpty t1 then
            if isEmpty t2 then 0 else -1
        else if isEmpty t2 then
            1
        else
            compareStacks comparer [ t1 ] [ t2 ]

    let choose s =
        minimumElement s

    let toList (t: FSetTree<'T>) =
        let rec loop (t': FSetTree<'T>) acc =
            if isEmpty t' then
                acc
            else if t'.Height = 1 then
                t'.Key :: acc
            else
                let tn = asNode t'
                loop tn.Left (tn.Key :: loop tn.Right acc)

        loop t []

    let copyToArray s (arr: _ array) i =
        let mutable j = i

        iter
            (fun x ->
                arr.[j] <- x
                j <- j + 1)
            s

    let toArray s =
        let n = (count s)
        let res = Array.zeroCreate n
        copyToArray s res 0
        res

    let rec mkFromEnumerator comparer acc (e: IEnumerator<_>) =
        if e.MoveNext() then
            mkFromEnumerator comparer (add comparer e.Current acc) e
        else
            acc

    let ofSeq comparer (c: IEnumerable<_>) =
        use ie = c.GetEnumerator()
        mkFromEnumerator comparer empty ie

    let ofArray comparer l =
        Array.fold (fun acc k -> add comparer k acc) empty l

    let rec equals (s: FSetTree<'T>) (t: FSetTree<'T>) =
        if obj.ReferenceEquals (s, t)
        then true
        else
            if s.Height <> t.Height then false
            else
                if s.Height = 1 && t.Height = 1
                then s.Key = t.Key
                else
                    let sNode = asNode s
                    let tNode = asNode t
                    sNode.Key = tNode.Key &&
                    equals sNode tNode

    let rec equalsComparer (comparer : IEqualityComparer) (s: FSetTree<'T>) (t: FSetTree<'T>) =
        if obj.ReferenceEquals (s, t)
        then true
        else
            if s.Height <> t.Height then false
            else
                if s.Height = 1 && t.Height = 1
                then comparer.Equals (s.Key, t.Key)
                else
                    let sNode = asNode s
                    let tNode = asNode t
                    comparer.Equals (sNode.Key, tNode.Key) &&
                    equalsComparer comparer sNode tNode

[<Sealed>]
[<CompiledName("FSharpFSet`1")>]
[<DebuggerTypeProxy(typedefof<SetDebugView<_>>)>]
[<DebuggerDisplay("Count = {Count}")>]
type FSet<[<EqualityConditionalOn>] 'T when 'T: comparison>(comparer: IComparer<'T>, tree: FSetTree<'T>) =

    [<System.NonSerialized>]
    // NOTE: This type is logically immutable. This field is only mutated during deserialization.
    let mutable comparer = comparer

    [<System.NonSerialized>]
    // NOTE: This type is logically immutable. This field is only mutated during deserialization.
    let mutable tree = tree

    // NOTE: This type is logically immutable. This field is only mutated during serialization and deserialization.
    // WARNING: The compiled name of this field may never be changed because it is part of the logical
    // WARNING: permanent serialization format for this type.
    let mutable serializedData = null

    static let anyToStringShowingNull (a : obj) =
        match a with
        | null -> "null"
        | x -> sprintf "%A" x

    // We use .NET generics per-instantiation static fields to avoid allocating a new object for each empty
    // set (it is just a lookup into a .NET table of type-instantiation-indexed static fields).

    static let empty: FSet<'T> =
        let comparer = LanguagePrimitives.FastGenericComparer<'T>
        FSet<'T>(comparer, FSetTree.empty)

    [<System.Runtime.Serialization.OnSerializingAttribute>]
    member _.OnSerializing(context: System.Runtime.Serialization.StreamingContext) =
        ignore context
        serializedData <- FSetTree.toArray tree

    // Do not set this to null, since concurrent threads may also be serializing the data
    //[<System.Runtime.Serialization.OnSerializedAttribute>]
    //member _.OnSerialized(context: System.Runtime.Serialization.StreamingContext) =
    //    serializedData <- null

    [<System.Runtime.Serialization.OnDeserializedAttribute>]
    member _.OnDeserialized(context: System.Runtime.Serialization.StreamingContext) =
        ignore context
        comparer <- LanguagePrimitives.FastGenericComparer<'T>
        tree <- FSetTree.ofArray comparer serializedData
        serializedData <- null

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal set.Comparer = comparer

    member internal set.Tree: FSetTree<'T> = tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    static member Empty: FSet<'T> = empty

    member s.Add value : FSet<'T> =
#if TRACE_SETS_AND_MAPS
        FSetTree.report ()
        FSetTree.numAdds <- FSetTree.numAdds + 1
        FSetTree.totalSizeOnSetAdd <- FSetTree.totalSizeOnSetAdd + float (FSetTree.count s.Tree)
#endif
        FSet<'T>(s.Comparer, FSetTree.add s.Comparer value s.Tree)

    member s.Remove value : FSet<'T> =
#if TRACE_SETS_AND_MAPS
        FSetTree.report ()
        FSetTree.numRemoves <- FSetTree.numRemoves + 1
#endif
        FSet<'T>(s.Comparer, FSetTree.remove s.Comparer value s.Tree)

    member s.Count = FSetTree.count s.Tree

    member s.Contains value =
#if TRACE_SETS_AND_MAPS
        FSetTree.report ()
        FSetTree.numLookups <- FSetTree.numLookups + 1
        FSetTree.totalSizeOnSetLookup <- FSetTree.totalSizeOnSetLookup + float (FSetTree.count s.Tree)
#endif
        FSetTree.mem s.Comparer value s.Tree

    member s.Iterate x =
        FSetTree.iter x s.Tree

    member s.Fold f z =
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
        FSetTree.fold (fun x z -> f.Invoke(z, x)) z s.Tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member s.IsEmpty = FSetTree.isEmpty s.Tree

    member s.Partition f : FSet<'T> * FSet<'T> =
        if FSetTree.isEmpty s.Tree then
            s, s
        else
            let t1, t2 = FSetTree.partition s.Comparer f s.Tree in FSet(s.Comparer, t1), FSet(s.Comparer, t2)

    member s.Filter f : FSet<'T> =
        if FSetTree.isEmpty s.Tree then
            s
        else
            FSet(s.Comparer, FSetTree.filter s.Comparer f s.Tree)

    member s.Map f : FSet<'U> =
        let comparer = LanguagePrimitives.FastGenericComparer<'U>
        FSet(comparer, FSetTree.fold (fun acc k -> FSetTree.add comparer (f k) acc) (FSetTree.empty) s.Tree)

    member s.Exists f =
        FSetTree.exists f s.Tree

    member s.ForAll f =
        FSetTree.forall f s.Tree

    static member (-)(set1: FSet<'T>, set2: FSet<'T>) =
        if FSetTree.isEmpty set1.Tree then
            set1 (* 0 - B = 0 *)
        else if FSetTree.isEmpty set2.Tree then
            set1 (* A - 0 = A *)
        else
            FSet(set1.Comparer, FSetTree.diff set1.Comparer set1.Tree set2.Tree)

    static member (+)(set1: FSet<'T>, set2: FSet<'T>) =
#if TRACE_SETS_AND_MAPS
        FSetTree.report ()
        FSetTree.numUnions <- FSetTree.numUnions + 1
#endif
        if FSetTree.isEmpty set2.Tree then
            set1 (* A U 0 = A *)
        else if FSetTree.isEmpty set1.Tree then
            set2 (* 0 U B = B *)
        else
            FSet(set1.Comparer, FSetTree.union set1.Comparer set1.Tree set2.Tree)

    static member Intersection(a: FSet<'T>, b: FSet<'T>) : FSet<'T> =
        if FSetTree.isEmpty b.Tree then
            b (* A INTER 0 = 0 *)
        else if FSetTree.isEmpty a.Tree then
            a (* 0 INTER B = 0 *)
        else
            FSet(a.Comparer, FSetTree.intersection a.Comparer a.Tree b.Tree)

    static member Union(sets: seq<FSet<'T>>) : FSet<'T> =
        Seq.fold (+) FSet<'T>.Empty sets

    static member Intersection(sets: seq<FSet<'T>>) : FSet<'T> =
        Seq.reduce (fun s1 s2 -> FSet.Intersection(s1, s2)) sets

    static member Equality(a: FSet<'T>, b: FSet<'T>) =
        (FSetTree.compare a.Comparer a.Tree b.Tree = 0)

    static member Compare(a: FSet<'T>, b: FSet<'T>) =
        FSetTree.compare a.Comparer a.Tree b.Tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.Choose = FSetTree.choose x.Tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.MinimumElement = FSetTree.minimumElement x.Tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.MaximumElement = FSetTree.maximumElement x.Tree

    member x.IsSubsetOf(otherSet: FSet<'T>) =
        FSetTree.subset x.Comparer x.Tree otherSet.Tree

    member x.IsSupersetOf(otherSet: FSet<'T>) =
        FSetTree.subset x.Comparer otherSet.Tree x.Tree

    member x.IsProperSubsetOf(otherSet: FSet<'T>) =
        FSetTree.properSubset x.Comparer x.Tree otherSet.Tree

    member x.IsProperSupersetOf(otherSet: FSet<'T>) =
        FSetTree.properSubset x.Comparer otherSet.Tree x.Tree

    member x.ToList() =
        FSetTree.toList x.Tree

    member x.ToArray() =
        FSetTree.toArray x.Tree

    member private this.ComputeHashCode() =
        let combineHash x y =
            (x <<< 1) + y + 631

        let mutable res = 0

        for x in this do
            res <- combineHash res (hash x)

        res

    override this.GetHashCode() =
        this.ComputeHashCode()

    override this.Equals that =
        if obj.ReferenceEquals (this, that)
        then true
        else
            match that with
            | :? FSet<'T> as that ->
                FSetTree.equals this.Tree that.Tree
            | _ -> false

    interface System.IComparable with
        member this.CompareTo(that: obj) =
            FSetTree.compare this.Comparer this.Tree ((that :?> FSet<'T>).Tree)

    interface IStructuralEquatable with
        member this.Equals(that, comparer) =
            if obj.ReferenceEquals (this, that)
            then true
            else
                match that with
                | :? FSet<'T> as that ->
                    FSetTree.equalsComparer comparer this.Tree that.Tree
                | _ -> false

        member this.GetHashCode(comparer) =
            let combineHash x y =
                (x <<< 1) + y + 631

            let mutable res = 0

            for x in this do
                res <- combineHash res (comparer.GetHashCode(x))

            res

    interface ICollection<'T> with
        member s.Add x =
            ignore x
            raise (new System.NotSupportedException("ReadOnlyCollection"))

        member s.Clear() =
            raise (new System.NotSupportedException("ReadOnlyCollection"))

        member s.Remove x =
            ignore x
            raise (new System.NotSupportedException("ReadOnlyCollection"))

        member s.Contains x =
            FSetTree.mem s.Comparer x s.Tree

        member s.CopyTo(arr, i) =
            FSetTree.copyToArray s.Tree arr i

        member s.IsReadOnly = true

        member s.Count = s.Count

    interface IReadOnlyCollection<'T> with
        member s.Count = s.Count

    interface IEnumerable<'T> with
        member s.GetEnumerator() =
            FSetTree.mkIEnumerator s.Tree

    interface IEnumerable with
        override s.GetEnumerator() =
            (FSetTree.mkIEnumerator s.Tree :> IEnumerator)

    static member Singleton(x: 'T) : FSet<'T> =
        FSet<'T>.Empty.Add x

    new(elements: seq<'T>) =
        let comparer = LanguagePrimitives.FastGenericComparer<'T>
        FSet(comparer, FSetTree.ofSeq comparer elements)

    static member Create(elements: seq<'T>) =
        FSet<'T>(elements)

    static member FromArray(arr: 'T array) : FSet<'T> =
        let comparer = LanguagePrimitives.FastGenericComparer<'T>
        FSet(comparer, FSetTree.ofArray comparer arr)

    override x.ToString() =
        match List.ofSeq (Seq.truncate 4 x) with
        | [] -> "set []"
        | [ h1 ] ->
            let txt1 = anyToStringShowingNull h1
            StringBuilder().Append("set [").Append(txt1).Append("]").ToString()
        | [ h1; h2 ] ->
            let txt1 = anyToStringShowingNull h1
            let txt2 = anyToStringShowingNull h2

            StringBuilder()
                .Append("set [")
                .Append(txt1)
                .Append("; ")
                .Append(txt2)
                .Append("]")
                .ToString()
        | [ h1; h2; h3 ] ->
            let txt1 = anyToStringShowingNull h1
            let txt2 = anyToStringShowingNull h2
            let txt3 = anyToStringShowingNull h3

            StringBuilder()
                .Append("set [")
                .Append(txt1)
                .Append("; ")
                .Append(txt2)
                .Append("; ")
                .Append(txt3)
                .Append("]")
                .ToString()
        | h1 :: h2 :: h3 :: _ ->
            let txt1 = anyToStringShowingNull h1
            let txt2 = anyToStringShowingNull h2
            let txt3 = anyToStringShowingNull h3

            StringBuilder()
                .Append("set [")
                .Append(txt1)
                .Append("; ")
                .Append(txt2)
                .Append("; ")
                .Append(txt3)
                .Append("; ... ]")
                .ToString()

and [<Sealed>] SetDebugView<'T when 'T: comparison>(v: FSet<'T>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items = v |> Seq.truncate 1000 |> Seq.toArray

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FSet =

    [<CompiledName("IsEmpty")>]
    let isEmpty (set: FSet<'T>) =
        set.IsEmpty

    [<CompiledName("Contains")>]
    let contains element (set: FSet<'T>) =
        set.Contains element

    [<CompiledName("Add")>]
    let add value (set: FSet<'T>) =
        set.Add value

    [<CompiledName("Singleton")>]
    let singleton value =
        FSet<'T>.Singleton value

    [<CompiledName("Remove")>]
    let remove value (set: FSet<'T>) =
        set.Remove value

    [<CompiledName("Union")>]
    let union (set1: FSet<'T>) (set2: FSet<'T>) =
        set1 + set2

    [<CompiledName("UnionMany")>]
    let unionMany sets =
        FSet.Union sets

    [<CompiledName("Intersect")>]
    let intersect (set1: FSet<'T>) (set2: FSet<'T>) =
        FSet<'T>.Intersection(set1, set2)

    [<CompiledName("IntersectMany")>]
    let intersectMany sets =
        FSet.Intersection sets

    [<CompiledName("Iterate")>]
    let iter action (set: FSet<'T>) =
        set.Iterate action

    [<CompiledName("Empty")>]
    let empty<'T when 'T: comparison> : FSet<'T> = FSet<'T>.Empty

    [<CompiledName("ForAll")>]
    let forall predicate (set: FSet<'T>) =
        set.ForAll predicate

    [<CompiledName("Exists")>]
    let exists predicate (set: FSet<'T>) =
        set.Exists predicate

    [<CompiledName("Filter")>]
    let filter predicate (set: FSet<'T>) =
        set.Filter predicate

    [<CompiledName("Partition")>]
    let partition predicate (set: FSet<'T>) =
        set.Partition predicate

    [<CompiledName("Fold")>]
    let fold<'T, 'State when 'T: comparison> folder (state: 'State) (set: FSet<'T>) =
        FSetTree.fold folder state set.Tree

    [<CompiledName("FoldBack")>]
    let foldBack<'T, 'State when 'T: comparison> folder (set: FSet<'T>) (state: 'State) =
        FSetTree.foldBack folder set.Tree state

    [<CompiledName("Map")>]
    let map mapping (set: FSet<'T>) =
        set.Map mapping

    [<CompiledName("Count")>]
    let count (set: FSet<'T>) =
        set.Count

    [<CompiledName("OfList")>]
    let ofList elements =
        FSet(List.toSeq elements)

    [<CompiledName("OfArray")>]
    let ofArray (array: 'T array) =
        FSet<'T>.FromArray array

    [<CompiledName("ToList")>]
    let toList (set: FSet<'T>) =
        set.ToList()

    [<CompiledName("ToArray")>]
    let toArray (set: FSet<'T>) =
        set.ToArray()

    [<CompiledName("ToSeq")>]
    let toSeq (set: FSet<'T>) =
        (set :> seq<'T>)

    [<CompiledName("OfSeq")>]
    let ofSeq (elements: seq<_>) =
        FSet elements

    [<CompiledName("Difference")>]
    let difference (set1: FSet<'T>) (set2: FSet<'T>) =
        set1 - set2

    [<CompiledName("IsSubset")>]
    let isSubset (set1: FSet<'T>) (set2: FSet<'T>) =
        FSetTree.subset set1.Comparer set1.Tree set2.Tree

    [<CompiledName("IsSuperset")>]
    let isSuperset (set1: FSet<'T>) (set2: FSet<'T>) =
        FSetTree.subset set1.Comparer set2.Tree set1.Tree

    [<CompiledName("IsProperSubset")>]
    let isProperSubset (set1: FSet<'T>) (set2: FSet<'T>) =
        FSetTree.properSubset set1.Comparer set1.Tree set2.Tree

    [<CompiledName("IsProperSuperset")>]
    let isProperSuperset (set1: FSet<'T>) (set2: FSet<'T>) =
        FSetTree.properSubset set1.Comparer set2.Tree set1.Tree

    [<CompiledName("MinElement")>]
    let minElement (set: FSet<'T>) =
        set.MinimumElement

    [<CompiledName("MaxElement")>]
    let maxElement (set: FSet<'T>) =
        set.MaximumElement
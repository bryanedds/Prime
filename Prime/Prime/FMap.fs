// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.
// Adapted by Prime as FMap to enable fast map comparisons conducive to an MVU context.

namespace Prime
open System
open System.Collections.Generic
open System.Collections
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Text
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

[<NoEquality; NoComparison>]
[<AllowNullLiteral>]
type FMapTree<'Key, 'Value>(k: 'Key, v: 'Value, h: int) =
    member _.Height = h
    member _.Key = k
    member _.Value = v
    new(k: 'Key, v: 'Value) = FMapTree(k, v, 1)

[<NoEquality; NoComparison>]
[<Sealed>]
[<AllowNullLiteral>]
type internal FMapTreeNode<'Key, 'Value>
    (k: 'Key, v: 'Value, left: FMapTree<'Key, 'Value>, right: FMapTree<'Key, 'Value>, h: int) =
    inherit FMapTree<'Key, 'Value>(k, v, h)
    member _.Left = left
    member _.Right = right

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FMapTree =

    let empty = null

    let inline isEmpty (m: FMapTree<'Key, 'Value>) =
        isNull m

    let inline internal asNode (value: FMapTree<'Key, 'Value>) : FMapTreeNode<'Key, 'Value> =
        value :?> FMapTreeNode<'Key, 'Value>

    let rec sizeAux acc (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            acc
        else if m.Height = 1 then
            acc + 1
        else
            let mn = asNode m
            sizeAux (sizeAux (acc + 1) mn.Left) mn.Right

    let size x =
        sizeAux 0 x

#if TRACE_SETS_AND_MAPS
    let mutable traceCount = 0
    let mutable numOnes = 0
    let mutable numNodes = 0
    let mutable numAdds = 0
    let mutable numRemoves = 0
    let mutable numLookups = 0
    let mutable numUnions = 0
    let mutable totalSizeOnNodeCreation = 0.0
    let mutable totalSizeOnMapAdd = 0.0
    let mutable totalSizeOnMapLookup = 0.0
    let mutable largestMapSize = 0
    let mutable largestMapStackTrace = Unchecked.defaultof<_>

    let report () =
        traceCount <- traceCount + 1

        if traceCount % 1000000 = 0 then
            Console.WriteLine(
                "#MapOne = {0}, #MapNode = {1}, #Add = {2}, #Remove = {3}, #Unions = {4}, #Lookups = {5}, avMapTreeSizeOnNodeCreation = {6}, avMapSizeOnCreation = {7}, avMapSizeOnLookup = {8}",
                numOnes,
                numNodes,
                numAdds,
                numRemoves,
                numUnions,
                numLookups,
                (totalSizeOnNodeCreation / float (numNodes + numOnes)),
                (totalSizeOnMapAdd / float numAdds),
                (totalSizeOnMapLookup / float numLookups)
            )

            Console.WriteLine("#largestMapSize = {0}, largestMapStackTrace = {1}", largestMapSize, largestMapStackTrace)

    let FMapTree (k, v) =
        report ()
        numOnes <- numOnes + 1
        totalSizeOnNodeCreation <- totalSizeOnNodeCreation + 1.0
        FMapTree(k, v)

    let FMapTreeNode (x, l, v, r, h) =
        report ()
        numNodes <- numNodes + 1
        let n = FMapTreeNode(x, l, v, r, h)
        totalSizeOnNodeCreation <- totalSizeOnNodeCreation + float (size n)
        n
#endif

    let inline height (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then 0 else m.Height

    [<Literal>]
    let tolerance = 2

    let mk l k v r : FMapTree<'Key, 'Value> =
        let hl = height l
        let hr = height r
        let m = if hl < hr then hr else hl

        if m = 0 then // m=0 ~ isEmpty l && isEmpty r
            FMapTree(k, v)
        else
            FMapTreeNode(k, v, l, r, m + 1) :> FMapTree<'Key, 'Value> // new map is higher by 1 than the highest

    let rebalance t1 (k: 'Key) (v: 'Value) t2 : FMapTree<'Key, 'Value> =
        let t1h = height t1
        let t2h = height t2

        if t2h > t1h + tolerance then (* right is heavier than left *)
            let t2' = asNode (t2)
            (* one of the nodes must have height > height t1 + 1 *)
            if height t2'.Left > t1h + 1 then (* balance left: combination *)
                let t2l = asNode (t2'.Left)
                mk (mk t1 k v t2l.Left) t2l.Key t2l.Value (mk t2l.Right t2'.Key t2'.Value t2'.Right)
            else (* rotate left *)
                mk (mk t1 k v t2'.Left) t2'.Key t2'.Value t2'.Right
        else if t1h > t2h + tolerance then (* left is heavier than right *)
            let t1' = asNode (t1)
            (* one of the nodes must have height > height t2 + 1 *)
            if height t1'.Right > t2h + 1 then
                (* balance right: combination *)
                let t1r = asNode (t1'.Right)
                mk (mk t1'.Left t1'.Key t1'.Value t1r.Left) t1r.Key t1r.Value (mk t1r.Right k v t2)
            else
                mk t1'.Left t1'.Key t1'.Value (mk t1'.Right k v t2)
        else
            mk t1 k v t2

    let rec add (comparer: IComparer<'Key>) k (v: 'Value) (m: FMapTree<'Key, 'Value>) : FMapTree<'Key, 'Value> =
        if isEmpty m then
            FMapTree(k, v)
        else
            let c = comparer.Compare(k, m.Key)

            if m.Height = 1 then
                if c < 0 then
                    FMapTreeNode(k, v, empty, m, 2) :> FMapTree<'Key, 'Value>
                elif c = 0 then
                    FMapTree(k, v)
                else
                    FMapTreeNode(k, v, m, empty, 2) :> FMapTree<'Key, 'Value>
            else
                let mn = asNode m

                if c < 0 then
                    rebalance (add comparer k v mn.Left) mn.Key mn.Value mn.Right
                elif c = 0 then
                    FMapTreeNode(k, v, mn.Left, mn.Right, mn.Height) :> FMapTree<'Key, 'Value>
                else
                    rebalance mn.Left mn.Key mn.Value (add comparer k v mn.Right)

    let rec tryGetValue (comparer: IComparer<'Key>) k (v: byref<'Value>) (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            false
        else
            let c = comparer.Compare(k, m.Key)

            if c = 0 then
                v <- m.Value
                true
            else if m.Height = 1 then
                false
            else
                let mn = asNode m
                tryGetValue comparer k &v (if c < 0 then mn.Left else mn.Right)

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwKeyNotFound () =
        raise (KeyNotFoundException())

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let find (comparer: IComparer<'Key>) k (m: FMapTree<'Key, 'Value>) =
        let mutable v = Unchecked.defaultof<'Value>

        if tryGetValue comparer k &v m then
            v
        else
            throwKeyNotFound ()

    let tryFind (comparer: IComparer<'Key>) k (m: FMapTree<'Key, 'Value>) =
        let mutable v = Unchecked.defaultof<'Value>

        if tryGetValue comparer k &v m then
            Some v
        else
            None

    let partition1 (comparer: IComparer<'Key>) (f: OptimizedClosures.FSharpFunc<_, _, _>) k v (acc1, acc2) =
        if f.Invoke(k, v) then
            (add comparer k v acc1, acc2)
        else
            (acc1, add comparer k v acc2)

    let rec partitionAux
        (comparer: IComparer<'Key>)
        (f: OptimizedClosures.FSharpFunc<_, _, _>)
        (m: FMapTree<'Key, 'Value>)
        acc
        =
        if isEmpty m then
            acc
        else if m.Height = 1 then
            partition1 comparer f m.Key m.Value acc
        else
            let mn = asNode m
            let acc = partitionAux comparer f mn.Right acc
            let acc = partition1 comparer f mn.Key mn.Value acc
            partitionAux comparer f mn.Left acc

    let partition (comparer: IComparer<'Key>) f m =
        partitionAux comparer (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m (empty, empty)

    let filter1 (comparer: IComparer<'Key>) (f: OptimizedClosures.FSharpFunc<_, _, _>) k v acc =
        if f.Invoke(k, v) then
            add comparer k v acc
        else
            acc

    let rec filterAux
        (comparer: IComparer<'Key>)
        (f: OptimizedClosures.FSharpFunc<_, _, _>)
        (m: FMapTree<'Key, 'Value>)
        acc
        =
        if isEmpty m then
            acc
        else if m.Height = 1 then
            filter1 comparer f m.Key m.Value acc
        else
            let mn = asNode m
            let acc = filterAux comparer f mn.Left acc
            let acc = filter1 comparer f mn.Key mn.Value acc
            filterAux comparer f mn.Right acc

    let filter (comparer: IComparer<'Key>) f m =
        filterAux comparer (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m empty

    let rec spliceOutSuccessor (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            failwith "internal error: FMap.spliceOutSuccessor"
        else if m.Height = 1 then
            m.Key, m.Value, empty
        else
            let mn = asNode m

            if isEmpty mn.Left then
                mn.Key, mn.Value, mn.Right
            else
                let k3, v3, l' = spliceOutSuccessor mn.Left in k3, v3, mk l' mn.Key mn.Value mn.Right

    let rec remove (comparer: IComparer<'Key>) k (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            empty
        else
            let c = comparer.Compare(k, m.Key)

            if m.Height = 1 then
                if c = 0 then empty else m
            else
                let mn = asNode m

                if c < 0 then
                    rebalance (remove comparer k mn.Left) mn.Key mn.Value mn.Right
                elif c = 0 then
                    if isEmpty mn.Left then
                        mn.Right
                    elif isEmpty mn.Right then
                        mn.Left
                    else
                        let sk, sv, r' = spliceOutSuccessor mn.Right
                        mk mn.Left sk sv r'
                else
                    rebalance mn.Left mn.Key mn.Value (remove comparer k mn.Right)

    let rec change
        (comparer: IComparer<'Key>)
        k
        (u: 'Value option -> 'Value option)
        (m: FMapTree<'Key, 'Value>)
        : FMapTree<'Key, 'Value> =
        if isEmpty m then
            match u None with
            | None -> m
            | Some v -> FMapTree(k, v)
        else if m.Height = 1 then
            let c = comparer.Compare(k, m.Key)

            if c < 0 then
                match u None with
                | None -> m
                | Some v -> FMapTreeNode(k, v, empty, m, 2) :> FMapTree<'Key, 'Value>
            elif c = 0 then
                match u (Some m.Value) with
                | None -> empty
                | Some v -> FMapTree(k, v)
            else
                match u None with
                | None -> m
                | Some v -> FMapTreeNode(k, v, m, empty, 2) :> FMapTree<'Key, 'Value>
        else
            let mn = asNode m
            let c = comparer.Compare(k, mn.Key)

            if c < 0 then
                rebalance (change comparer k u mn.Left) mn.Key mn.Value mn.Right
            elif c = 0 then
                match u (Some mn.Value) with
                | None ->
                    if isEmpty mn.Left then
                        mn.Right
                    elif isEmpty mn.Right then
                        mn.Left
                    else
                        let sk, sv, r' = spliceOutSuccessor mn.Right
                        mk mn.Left sk sv r'
                | Some v -> FMapTreeNode(k, v, mn.Left, mn.Right, mn.Height) :> FMapTree<'Key, 'Value>
            else
                rebalance mn.Left mn.Key mn.Value (change comparer k u mn.Right)

    let rec mem (comparer: IComparer<'Key>) k (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            false
        else
            let c = comparer.Compare(k, m.Key)

            if m.Height = 1 then
                c = 0
            else
                let mn = asNode m

                if c < 0 then
                    mem comparer k mn.Left
                else
                    (c = 0 || mem comparer k mn.Right)

    let rec iterOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            ()
        else if m.Height = 1 then
            f.Invoke(m.Key, m.Value)
        else
            let mn = asNode m
            iterOpt f mn.Left
            f.Invoke(mn.Key, mn.Value)
            iterOpt f mn.Right

    let iter f m =
        iterOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec tryPickOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            None
        else if m.Height = 1 then
            f.Invoke(m.Key, m.Value)
        else
            let mn = asNode m

            match tryPickOpt f mn.Left with
            | Some _ as res -> res
            | None ->
                match f.Invoke(mn.Key, mn.Value) with
                | Some _ as res -> res
                | None -> tryPickOpt f mn.Right

    let tryPick f m =
        tryPickOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec existsOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            false
        else if m.Height = 1 then
            f.Invoke(m.Key, m.Value)
        else
            let mn = asNode m
            existsOpt f mn.Left || f.Invoke(mn.Key, mn.Value) || existsOpt f mn.Right

    let exists f m =
        existsOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec forallOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            true
        else if m.Height = 1 then
            f.Invoke(m.Key, m.Value)
        else
            let mn = asNode m
            forallOpt f mn.Left && f.Invoke(mn.Key, mn.Value) && forallOpt f mn.Right

    let forall f m =
        forallOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec map (f: 'Value -> 'Result) (m: FMapTree<'Key, 'Value>) : FMapTree<'Key, 'Result> =
        if isEmpty m then
            empty
        else if m.Height = 1 then
            FMapTree(m.Key, f m.Value)
        else
            let mn = asNode m
            let l2 = map f mn.Left
            let v2 = f mn.Value
            let r2 = map f mn.Right
            FMapTreeNode(mn.Key, v2, l2, r2, mn.Height) :> FMapTree<'Key, 'Result>

    let rec mapiOpt (f: OptimizedClosures.FSharpFunc<'Key, 'Value, 'Result>) (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            empty
        else if m.Height = 1 then
            FMapTree(m.Key, f.Invoke(m.Key, m.Value))
        else
            let mn = asNode m
            let l2 = mapiOpt f mn.Left
            let v2 = f.Invoke(mn.Key, mn.Value)
            let r2 = mapiOpt f mn.Right
            FMapTreeNode(mn.Key, v2, l2, r2, mn.Height) :> FMapTree<'Key, 'Result>

    let mapi f m =
        mapiOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec foldBackOpt (f: OptimizedClosures.FSharpFunc<_, _, _, _>) (m: FMapTree<'Key, 'Value>) x =
        if isEmpty m then
            x
        else if m.Height = 1 then
            f.Invoke(m.Key, m.Value, x)
        else
            let mn = asNode m
            let x = foldBackOpt f mn.Right x
            let x = f.Invoke(mn.Key, mn.Value, x)
            foldBackOpt f mn.Left x

    let foldBack f m x =
        foldBackOpt (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt f) m x

    let rec foldOpt (f: OptimizedClosures.FSharpFunc<_, _, _, _>) x (m: FMapTree<'Key, 'Value>) =
        if isEmpty m then
            x
        else if m.Height = 1 then
            f.Invoke(x, m.Key, m.Value)
        else
            let mn = asNode m
            let x = foldOpt f x mn.Left
            let x = f.Invoke(x, mn.Key, mn.Value)
            foldOpt f x mn.Right

    let fold f x m =
        foldOpt (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt f) x m

    let foldSectionOpt
        (comparer: IComparer<'Key>)
        lo
        hi
        (f: OptimizedClosures.FSharpFunc<_, _, _, _>)
        (m: FMapTree<'Key, 'Value>)
        x
        =
        let rec foldFromTo (f: OptimizedClosures.FSharpFunc<_, _, _, _>) (m: FMapTree<'Key, 'Value>) x =
            if isEmpty m then
                x
            else if m.Height = 1 then
                let cLoKey = comparer.Compare(lo, m.Key)
                let cKeyHi = comparer.Compare(m.Key, hi)

                let x =
                    if cLoKey <= 0 && cKeyHi <= 0 then
                        f.Invoke(m.Key, m.Value, x)
                    else
                        x

                x
            else
                let mn = asNode m
                let cLoKey = comparer.Compare(lo, mn.Key)
                let cKeyHi = comparer.Compare(mn.Key, hi)

                let x =
                    if cLoKey < 0 then
                        foldFromTo f mn.Left x
                    else
                        x

                let x =
                    if cLoKey <= 0 && cKeyHi <= 0 then
                        f.Invoke(mn.Key, mn.Value, x)
                    else
                        x

                let x =
                    if cKeyHi < 0 then
                        foldFromTo f mn.Right x
                    else
                        x

                x

        if comparer.Compare(lo, hi) = 1 then
            x
        else
            foldFromTo f m x

    let foldSection (comparer: IComparer<'Key>) lo hi f m x =
        foldSectionOpt comparer lo hi (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt f) m x

    let toList (m: FMapTree<'Key, 'Value>) =
        let rec loop (m: FMapTree<'Key, 'Value>) acc =
            if isEmpty m then
                acc
            else if m.Height = 1 then
                (m.Key, m.Value) :: acc
            else
                let mn = asNode m
                loop mn.Left ((mn.Key, mn.Value) :: loop mn.Right acc)

        loop m []

    let toArray m =
        m |> toList |> Array.ofList

    let ofList comparer l =
        List.fold (fun acc (k, v) -> add comparer k v acc) empty l

    let rec mkFromEnumerator comparer acc (e: IEnumerator<_>) =
        if e.MoveNext() then
            let (x, y) = e.Current
            mkFromEnumerator comparer (add comparer x y acc) e
        else
            acc

    let ofArray comparer (arr: ('Key * 'Value) array) =
        let mutable res = empty

        for (x, y) in arr do
            res <- add comparer x y res

        res

    let ofSeq comparer (c: seq<'Key * 'T>) =
        match c with
        | :? (('Key * 'T) array) as xs -> ofArray comparer xs
        | :? (('Key * 'T) list) as xs -> ofList comparer xs
        | _ ->
            use ie = c.GetEnumerator()
            mkFromEnumerator comparer empty ie

    let copyToArray m (arr: _ array) i =
        let mutable j = i

        m
        |> iter (fun x y ->
            arr.[j] <- KeyValuePair(x, y)
            j <- j + 1)

    /// Imperative left-to-right iterators.
    [<NoEquality; NoComparison>]
    type FMapIterator<'Key, 'Value when 'Key: comparison> =
        {
            /// invariant: always collapseLHS result
            mutable stack: FMapTree<'Key, 'Value> list

            /// true when MoveNext has been called
            mutable started: bool
        }

    // collapseLHS:
    // a) Always returns either [] or a list starting with MapOne.
    // b) The "fringe" of the set stack is unchanged.
    let rec collapseLHS (stack: FMapTree<'Key, 'Value> list) =
        match stack with
        | [] -> []
        | m :: rest ->
            if isEmpty m then
                collapseLHS rest
            else if m.Height = 1 then
                stack
            else
                let mn = asNode m
                collapseLHS (mn.Left :: FMapTree(mn.Key, mn.Value) :: mn.Right :: rest)

    let mkIterator m =
        {
            stack = collapseLHS [ m ]
            started = false
        }

    let notStarted () =
        raise (InvalidOperationException("Enumeration not started"))

    let alreadyFinished () =
        raise (InvalidOperationException("Enumeration already finished."))

    let unexpectedStackForCurrent () =
        failwith "Please report error: FMap iterator, unexpected stack for current"

    let unexpectedStackForMoveNext () =
        failwith "Please report error: FMap iterator, unexpected stack for moveNext"

    let current i =
        if i.started then
            match i.stack with
            | [] -> alreadyFinished ()
            | m :: _ ->
                if m.Height = 1 then
                    KeyValuePair<_, _>(m.Key, m.Value)
                else
                    unexpectedStackForCurrent ()
        else
            notStarted ()

    let rec moveNext i =
        if i.started then
            match i.stack with
            | [] -> false
            | m :: rest ->
                if m.Height = 1 then
                    i.stack <- collapseLHS rest
                    not i.stack.IsEmpty
                else
                    unexpectedStackForMoveNext ()
        else
            i.started <- true (* The first call to MoveNext "starts" the enumeration. *)
            not i.stack.IsEmpty

    let mkIEnumerator m =
        let mutable i = mkIterator m

        { new IEnumerator<_> with
            member _.Current = current i
          interface System.Collections.IEnumerator with
              member _.Current = box (current i)

              member _.MoveNext() =
                  moveNext i

              member _.Reset() =
                  i <- mkIterator m
          interface System.IDisposable with
              member _.Dispose() =
                  ()
        }

    let rec leftmost m =
        if isEmpty m then
            throwKeyNotFound ()
        else if m.Height = 1 then
            (m.Key, m.Value)
        else
            let nd = asNode m

            if isNull nd.Left then
                (m.Key, m.Value)
            else
                leftmost nd.Left

    let rec rightmost m =
        if isEmpty m then
            throwKeyNotFound ()
        else if m.Height = 1 then
            (m.Key, m.Value)
        else
            let nd = asNode m

            if isNull nd.Right then
                (m.Key, m.Value)
            else
                rightmost nd.Right

    let rec equals (m: FMapTree<'Key, 'Value>) (n: FMapTree<'Key, 'Value>) =
        if obj.ReferenceEquals (m, n) then true
        elif isEmpty m && isEmpty n then true
        elif isEmpty m || isEmpty n then false
        else
            if m.Height <> n.Height then false
            else
                if m.Height = 1 && n.Height = 1 then
                    m.Key = n.Key &&
                    Unchecked.equals m.Value n.Value
                else
                    let mNode = asNode m
                    let nNode = asNode n
                    mNode.Key = nNode.Key &&
                    Unchecked.equals mNode.Value nNode.Value &&
                    equals mNode nNode

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module private FMapTreeNode =

    let rec equals (this : FMapTreeNode<'Key, 'Value>) (that : FMapTreeNode<'Key, 'Value>) = 
        if Object.ReferenceEquals (this, that) then true
        elif isNull this && isNull that then true
        elif isNull this || isNull that then false
        else
            if this.Height <> that.Height then false
            elif not (Unchecked.equals this.Key that.Key) then false
            elif not (Unchecked.equals this.Value that.Value) then false
            else
                let leftEquals = 
                    match this.Left with
                    | :? FMapTreeNode<'Key, 'Value> as thisLeftNode ->
                        match that.Left with
                        | :? FMapTreeNode<'Key, 'Value> as thatLeftNode -> equals thisLeftNode thatLeftNode
                        | _ -> false
                    | _ -> not (that.Left :? FMapTreeNode<'Key, 'Value>) && FMapTree.equals this.Left that.Left
                if leftEquals then
                    match this.Right with
                    | :? FMapTreeNode<'Key, 'Value> as thisRightNode ->
                        match that.Right with
                        | :? FMapTreeNode<'Key, 'Value> as thatRightNode -> equals thisRightNode thatRightNode
                        | _ -> false
                    | _ -> not (that.Right :? FMapTreeNode<'Key, 'Value>) && FMapTree.equals this.Right that.Right
                else false

/// Like F# Map but with fast equality.
[<System.Diagnostics.DebuggerTypeProxy(typedefof<FMapDebugView<_, _>>)>]
[<System.Diagnostics.DebuggerDisplay("Count = {Count}")>]
[<Sealed>]
[<CompiledName("FSharpFMap`2")>]
type FMap<[<EqualityConditionalOn>] 'Key, [<EqualityConditionalOn; ComparisonConditionalOn>] 'Value when 'Key: comparison>
    (comparer: IComparer<'Key>, tree: FMapTree<'Key, 'Value>) =

    [<System.NonSerialized>]
    // This type is logically immutable. This field is only mutated during deserialization.
    let mutable comparer = comparer

    [<System.NonSerialized>]
    // This type is logically immutable. This field is only mutated during deserialization.
    let mutable tree = tree

    // This type is logically immutable. This field is only mutated during serialization and deserialization.
    //
    // WARNING: The compiled name of this field may never be changed because it is part of the logical
    // WARNING: permanent serialization format for this type.
    let mutable serializedData = null

    static let anyToStringShowingNull (a : obj) =
        match a with
        | null -> "null"
        | x -> sprintf "%A" x

    // We use .NET generics per-instantiation static fields to avoid allocating a new object for each empty
    // set (it is just a lookup into a .NET table of type-instantiation-indexed static fields).
    static let empty =
        let comparer = LanguagePrimitives.FastGenericComparer<'Key>
        new FMap<'Key, 'Value>(comparer, FMapTree.empty)

    [<System.Runtime.Serialization.OnSerializingAttribute>]
    member _.OnSerializing(context: System.Runtime.Serialization.StreamingContext) =
        ignore context
        serializedData <- FMapTree.toArray tree |> Array.map (fun (k, v) -> KeyValuePair(k, v))

    // Do not set this to null, since concurrent threads may also be serializing the data
    //[<System.Runtime.Serialization.OnSerializedAttribute>]
    //member _.OnSerialized(context: System.Runtime.Serialization.StreamingContext) =
    //    serializedData <- null

    [<System.Runtime.Serialization.OnDeserializedAttribute>]
    member _.OnDeserialized(context: System.Runtime.Serialization.StreamingContext) =
        ignore context
        comparer <- LanguagePrimitives.FastGenericComparer<'Key>

        tree <-
            serializedData
            |> Array.map (fun kvp -> kvp.Key, kvp.Value)
            |> FMapTree.ofArray comparer

        serializedData <- null

    static member Empty: FMap<'Key, 'Value> = empty

    static member Create(ie: IEnumerable<_>) : FMap<'Key, 'Value> =
        let comparer = LanguagePrimitives.FastGenericComparer<'Key>
        new FMap<_, _>(comparer, FMapTree.ofSeq comparer ie)

    new(elements: seq<_>) =
        let comparer = LanguagePrimitives.FastGenericComparer<'Key>
        new FMap<_, _>(comparer, FMapTree.ofSeq comparer elements)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal m.Comparer = comparer

    //[<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal m.Tree = tree

    member m.Add(key, value) : FMap<'Key, 'Value> =
#if TRACE_SETS_AND_MAPS
        FMapTree.report ()
        FMapTree.numAdds <- FMapTree.numAdds + 1
        let size = FMapTree.size m.Tree + 1
        FMapTree.totalSizeOnMapAdd <- FMapTree.totalSizeOnMapAdd + float size

        if size > FMapTree.largestMapSize then
            FMapTree.largestMapSize <- size
            FMapTree.largestMapStackTrace <- System.Diagnostics.StackTrace().ToString()
#endif
        new FMap<'Key, 'Value>(comparer, FMapTree.add comparer key value tree)

    member m.Change(key, f) : FMap<'Key, 'Value> =
        new FMap<'Key, 'Value>(comparer, FMapTree.change comparer key f tree)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member m.IsEmpty = FMapTree.isEmpty tree

    member m.Item
        with get (key: 'Key) =
#if TRACE_SETS_AND_MAPS
            FMapTree.report ()
            FMapTree.numLookups <- FMapTree.numLookups + 1
            FMapTree.totalSizeOnMapLookup <- FMapTree.totalSizeOnMapLookup + float (FMapTree.size tree)
#endif
            FMapTree.find comparer key tree

    member m.TryPick f =
        FMapTree.tryPick f tree

    member m.Exists predicate =
        FMapTree.exists predicate tree

    member m.Filter predicate =
        new FMap<'Key, 'Value>(comparer, FMapTree.filter comparer predicate tree)

    member m.ForAll predicate =
        FMapTree.forall predicate tree

    member m.Fold f acc =
        FMapTree.foldBack f tree acc

    member m.FoldSection (lo: 'Key) (hi: 'Key) f (acc: 'z) =
        FMapTree.foldSection comparer lo hi f tree acc

    member m.Iterate f =
        FMapTree.iter f tree

    member m.MapRange(f: 'Value -> 'Result) =
        new FMap<'Key, 'Result>(comparer, FMapTree.map f tree)

    member m.Map f =
        new FMap<'Key, 'b>(comparer, FMapTree.mapi f tree)

    member m.Partition predicate : FMap<'Key, 'Value> * FMap<'Key, 'Value> =
        let r1, r2 = FMapTree.partition comparer predicate tree
        new FMap<'Key, 'Value>(comparer, r1), new FMap<'Key, 'Value>(comparer, r2)

    member m.Count = FMapTree.size tree

    member m.ContainsKey key =
#if TRACE_SETS_AND_MAPS
        FMapTree.report ()
        FMapTree.numLookups <- FMapTree.numLookups + 1
        FMapTree.totalSizeOnMapLookup <- FMapTree.totalSizeOnMapLookup + float (FMapTree.size tree)
#endif
        FMapTree.mem comparer key tree

    member m.Remove key =
        new FMap<'Key, 'Value>(comparer, FMapTree.remove comparer key tree)

    member m.TryGetValue(key, [<System.Runtime.InteropServices.Out>] value: byref<'Value>) =
        FMapTree.tryGetValue comparer key &value tree

    member m.TryFind key =
#if TRACE_SETS_AND_MAPS
        FMapTree.report ()
        FMapTree.numLookups <- FMapTree.numLookups + 1
        FMapTree.totalSizeOnMapLookup <- FMapTree.totalSizeOnMapLookup + float (FMapTree.size tree)
#endif
        FMapTree.tryFind comparer key tree

    member m.ToList() =
        FMapTree.toList tree

    member m.ToArray() =
        FMapTree.toArray tree

    member m.Keys = KeyCollection(m) :> ICollection<'Key>

    member m.Values = ValueCollection(m) :> ICollection<'Value>

    member m.MinKeyValue = FMapTree.leftmost tree
    member m.MaxKeyValue = FMapTree.rightmost tree

    static member ofList l : FMap<'Key, 'Value> =
        let comparer = LanguagePrimitives.FastGenericComparer<'Key>
        new FMap<_, _>(comparer, FMapTree.ofList comparer l)

    member this.ComputeHashCode() =
        let combineHash x y =
            (x <<< 1) + y + 631

        let mutable res = 0

        for (KeyValue(x, y)) in this do
            res <- combineHash res (hash x)
            res <- combineHash res (Unchecked.hash y)

        res

    override this.Equals that =
        if obj.ReferenceEquals (this, that) then true
        else
            match that with
            | :? FMap<'Key, 'Value> as that ->
                let thisIsNode = this.Tree :? FMapTreeNode<'Key, 'Value>
                let thatIsNode = that.Tree :? FMapTreeNode<'Key, 'Value>
                if thisIsNode && thatIsNode then
                    let thisNode = this.Tree :?> FMapTreeNode<'Key, 'Value>
                    let thatNode = that.Tree :?> FMapTreeNode<'Key, 'Value>
                    FMapTreeNode.equals thisNode thatNode
                elif not thisIsNode && not thatIsNode then
                    FMapTree.equals this.Tree that.Tree
                else false
            | _ -> false

    override this.GetHashCode() =
        this.ComputeHashCode()

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member _.GetEnumerator() =
            FMapTree.mkIEnumerator tree

    interface IEnumerable with
        member _.GetEnumerator() =
            (FMapTree.mkIEnumerator tree :> IEnumerator)

    interface IDictionary<'Key, 'Value> with
        member m.Item
            with get x = m.[x]
            and set _ _ = raise (NotSupportedException("FMap cannot be mutated."))

        member m.Keys = m.Keys

        member m.Values = m.Values

        member m.Add(_, _) =
            raise (NotSupportedException("FMap cannot be mutated."))

        member m.ContainsKey k =
            m.ContainsKey k

        member m.TryGetValue(k, r) =
            m.TryGetValue(k, &r)

        member m.Remove(_) =
            raise (NotSupportedException("FMap cannot be mutated."))

    interface ICollection<KeyValuePair<'Key, 'Value>> with
        member _.Add(_) =
            raise (NotSupportedException("FMap cannot be mutated."))

        member _.Clear() =
            raise (NotSupportedException("FMap cannot be mutated."))

        member _.Remove(_) =
            raise (NotSupportedException("FMap cannot be mutated."))

        member m.Contains x =
            m.ContainsKey x.Key && Unchecked.equals m.[x.Key] x.Value

        member _.CopyTo(arr, i) =
            FMapTree.copyToArray tree arr i

        member _.IsReadOnly = true

        member m.Count = m.Count

    interface System.IComparable with
        member m.CompareTo(obj: obj) =
            match obj with
            | :? FMap<'Key, 'Value> as m2 ->
                Seq.compareWith
                    (fun (kvp1: KeyValuePair<_, _>) (kvp2: KeyValuePair<_, _>) ->
                        let c = comparer.Compare(kvp1.Key, kvp2.Key) in

                        if c <> 0 then
                            c
                        else
                            Unchecked.compare kvp1.Value kvp2.Value)
                    m
                    m2
            | _ -> invalidArg "obj" "Not comparable."

    interface IReadOnlyCollection<KeyValuePair<'Key, 'Value>> with
        member m.Count = m.Count

    interface IReadOnlyDictionary<'Key, 'Value> with

        member m.Item
            with get key = m.[key]

        member m.Keys = m.Keys :> IEnumerable<'Key>

        member m.TryGetValue(key, value: byref<'Value>) =
            m.TryGetValue(key, &value)

        member m.Values = m.Values :> IEnumerable<'Value>

        member m.ContainsKey key =
            m.ContainsKey key

    override x.ToString() =
        match List.ofSeq (Seq.truncate 4 x) with
        | [] -> "fmap []"
        | [ KeyValue h1 ] ->
            let txt1 = anyToStringShowingNull h1
            StringBuilder().Append("map [").Append(txt1).Append("]").ToString()
        | [ KeyValue h1; KeyValue h2 ] ->
            let txt1 = anyToStringShowingNull h1
            let txt2 = anyToStringShowingNull h2

            StringBuilder()
                .Append("map [")
                .Append(txt1)
                .Append("; ")
                .Append(txt2)
                .Append("]")
                .ToString()
        | [ KeyValue h1; KeyValue h2; KeyValue h3 ] ->
            let txt1 = anyToStringShowingNull h1
            let txt2 = anyToStringShowingNull h2
            let txt3 = anyToStringShowingNull h3

            StringBuilder()
                .Append("map [")
                .Append(txt1)
                .Append("; ")
                .Append(txt2)
                .Append("; ")
                .Append(txt3)
                .Append("]")
                .ToString()
        | KeyValue h1 :: KeyValue h2 :: KeyValue h3 :: _ ->
            let txt1 = anyToStringShowingNull h1
            let txt2 = anyToStringShowingNull h2
            let txt3 = anyToStringShowingNull h3

            StringBuilder()
                .Append("map [")
                .Append(txt1)
                .Append("; ")
                .Append(txt2)
                .Append("; ")
                .Append(txt3)
                .Append("; ... ]")
                .ToString()

and [<Sealed>] FMapDebugView<'Key, 'Value when 'Key: comparison>(v: FMap<'Key, 'Value>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items =
        v |> Seq.truncate 10000 |> Seq.map KeyValuePairDebugFriendly |> Seq.toArray

and [<DebuggerDisplay("{keyValue.Value}", Name = "[{keyValue.Key}]", Type = "")>] KeyValuePairDebugFriendly<'Key, 'Value>
    (keyValue: KeyValuePair<'Key, 'Value>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.KeyValue = keyValue

and KeyCollection<'Key, 'Value when 'Key: comparison>(parent: FMap<'Key, 'Value>) =
    interface ICollection<'Key> with
        member _.Add(_) =
            raise (NotSupportedException("FMap cannot be mutated."))

        member _.Clear() =
            raise (NotSupportedException("FMap cannot be mutated."))

        member _.Remove(_) =
            raise (NotSupportedException("FMap cannot be mutated."))

        member _.Contains x =
            parent.ContainsKey x

        member _.CopyTo(arr, index) =
            if isNull arr then
                nullArg "arr"

            if index < 0 then
                invalidArg "index" "index must be positive"

            if index + parent.Count > arr.Length then
                invalidArg "index" "array is smaller than index plus the number of items to copy"

            let mutable i = index

            for item in parent do
                arr.[i] <- item.Key
                i <- i + 1

        member _.IsReadOnly = true

        member _.Count = parent.Count

    interface IEnumerable<'Key> with
        member _.GetEnumerator() =
            (seq {
                for item in parent do
                    item.Key
            })
                .GetEnumerator()

    interface IEnumerable with
        member _.GetEnumerator() =
            (seq {
                for item in parent do
                    item.Key
            })
                .GetEnumerator()
            :> IEnumerator

and ValueCollection<'Key, 'Value when 'Key: comparison>(parent: FMap<'Key, 'Value>) =
    interface ICollection<'Value> with
        member _.Add(_) =
            raise (NotSupportedException("FMap cannot be mutated."))

        member _.Clear() =
            raise (NotSupportedException("FMap cannot be mutated."))

        member _.Remove(_) =
            raise (NotSupportedException("FMap cannot be mutated."))

        member _.Contains x =
            parent.Exists(fun _ value -> Unchecked.equals value x)

        member _.CopyTo(arr, index) =
            if isNull arr then
                nullArg "arr"

            if index < 0 then
                invalidArg "index" "index must be positive"

            if index + parent.Count > arr.Length then
                invalidArg "index" "array is smaller than index plus the number of items to copy"

            let mutable i = index

            for item in parent do
                arr.[i] <- item.Value
                i <- i + 1

        member _.IsReadOnly = true

        member _.Count = parent.Count

    interface IEnumerable<'Value> with
        member _.GetEnumerator() =
            (seq {
                for item in parent do
                    item.Value
            })
                .GetEnumerator()

    interface IEnumerable with
        member _.GetEnumerator() =
            (seq {
                for item in parent do
                    item.Value
            })
                .GetEnumerator()
            :> IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FMap =

    [<CompiledName("IsEmpty")>]
    let isEmpty (table: FMap<_, _>) =
        table.IsEmpty

    [<CompiledName("Add")>]
    let add key value (table: FMap<_, _>) =
        table.Add(key, value)

    [<CompiledName("Change")>]
    let change key f (table: FMap<_, _>) =
        table.Change(key, f)

    [<CompiledName("Find")>]
    let find key (table: FMap<_, _>) =
        table.[key]

    [<CompiledName("TryFind")>]
    let tryFind key (table: FMap<_, _>) =
        table.TryFind key

    [<CompiledName("Remove")>]
    let remove key (table: FMap<_, _>) =
        table.Remove key

    [<CompiledName("ContainsKey")>]
    let containsKey key (table: FMap<_, _>) =
        table.ContainsKey key

    [<CompiledName("Iterate")>]
    let iter action (table: FMap<_, _>) =
        table.Iterate action

    [<CompiledName("TryPick")>]
    let tryPick chooser (table: FMap<_, _>) =
        table.TryPick chooser

    [<CompiledName("Pick")>]
    let pick chooser (table: FMap<_, _>) =
        match tryPick chooser table with
        | None -> raise (KeyNotFoundException())
        | Some res -> res

    [<CompiledName("Exists")>]
    let exists predicate (table: FMap<_, _>) =
        table.Exists predicate

    [<CompiledName("Filter")>]
    let filter predicate (table: FMap<_, _>) =
        table.Filter predicate

    [<CompiledName("Partition")>]
    let partition predicate (table: FMap<_, _>) =
        table.Partition predicate

    [<CompiledName("ForAll")>]
    let forall predicate (table: FMap<_, _>) =
        table.ForAll predicate

    [<CompiledName("Map")>]
    let map mapping (table: FMap<_, _>) =
        table.Map mapping

    [<CompiledName("Fold")>]
    let fold<'Key, 'T, 'State when 'Key: comparison> folder (state: 'State) (table: FMap<'Key, 'T>) =
        FMapTree.fold folder state table.Tree

    [<CompiledName("FoldBack")>]
    let foldBack<'Key, 'T, 'State when 'Key: comparison> folder (table: FMap<'Key, 'T>) (state: 'State) =
        FMapTree.foldBack folder table.Tree state

    [<CompiledName("ToSeq")>]
    let toSeq (table: FMap<_, _>) =
        table |> Seq.map (fun kvp -> kvp.Key, kvp.Value)

    [<CompiledName("FindKey")>]
    let findKey predicate (table: FMap<_, _>) =
        table
        |> Seq.pick (fun kvp ->
            let k = kvp.Key in

            if predicate k kvp.Value then
                Some k
            else
                None)

    [<CompiledName("TryFindKey")>]
    let tryFindKey predicate (table: FMap<_, _>) =
        table
        |> Seq.tryPick (fun kvp ->
            let k = kvp.Key in

            if predicate k kvp.Value then
                Some k
            else
                None)

    [<CompiledName("OfList")>]
    let ofList (elements: ('Key * 'Value) list) =
        FMap<_, _>.ofList elements

    [<CompiledName("OfSeq")>]
    let ofSeq elements =
        FMap<_, _>.Create elements

    [<CompiledName("OfArray")>]
    let ofArray (elements: ('Key * 'Value) array) =
        let comparer = LanguagePrimitives.FastGenericComparer<'Key>
        new FMap<_, _>(comparer, FMapTree.ofArray comparer elements)

    [<CompiledName("ToList")>]
    let toList (table: FMap<_, _>) =
        table.ToList()

    [<CompiledName("ToArray")>]
    let toArray (table: FMap<_, _>) =
        table.ToArray()

    [<CompiledName("Empty")>]
    let empty<'Key, 'Value when 'Key: comparison> = FMap<'Key, 'Value>.Empty

    [<CompiledName("Count")>]
    let count (table: FMap<_, _>) =
        table.Count

    [<CompiledName("Keys")>]
    let keys (table: FMap<_, _>) =
        table.Keys

    [<CompiledName("Values")>]
    let values (table: FMap<_, _>) =
        table.Values

    [<CompiledName("MinKeyValue")>]
    let minKeyValue (table: FMap<_, _>) =
        table.MinKeyValue

    [<CompiledName("MaxKeyValue")>]
    let maxKeyValue (table: FMap<_, _>) =
        table.MaxKeyValue
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

/// An enumerator for FStack.
type 'a FStackEnumerator (front : 'a array, back : 'a array) =

    let mutable inFront = true
    let mutable index = -1

    member this.MoveNext () =
        index <- inc index
        if inFront then
            if  index >= Array.length front then
                index <- 0
                inFront <- false
                index < Array.length back
            else true
        else index < Array.length back

    member this.Current =
        if inFront
        then front.[index]
        else back.[index]

    member this.Reset () =
        inFront <- true
        index <- -1

    member this.Dispose () = ()

    interface 'a IEnumerator with
        member this.MoveNext () = this.MoveNext ()
        member this.Current = this.Current
        member this.Current = this.Current :> obj
        member this.Reset () = this.Reset ()
        member this.Dispose () = this.Dispose ()

[<RequireQualifiedAccess>]
module FStack =

    /// A functional stack with fast iteration and amortized sqrt n conj.
    /// NOTE: not supported by SymbolicConverter.
    type [<CustomEquality; NoComparison; DefaultValue "[]">] 'a FStack =
        private
            { Front : 'a array
              Back : 'a array }

        interface 'a IEnumerable with
            member this.GetEnumerator () = new FStackEnumerator<'a> (this.Front, this.Back) :> 'a IEnumerator
            member this.GetEnumerator () = new FStackEnumerator<'a> (this.Front, this.Back) :> IEnumerator

        override this.Equals that =
            match that with
            | :? ('a FStack) as that ->
                refEq this that || // OPTIMIZATION: first check ref equality
                Seq.forall2 Unchecked.equals this that
            | _ -> false

        override this.GetHashCode () =
            let mutable hash = 1
            for a in this do hash <- 31 * hash + Unchecked.hash a
            hash

        member this.GetEnumerator () =
            new FStackEnumerator<'a> (this.Front, this.Back)

        member this.Item with get index =
            if index >= 0 then
                if index >= this.Front.Length then 
                    let index' = index - this.Front.Length
                    if index' >= this.Back.Length
                    then raise (IndexOutOfRangeException "Cannot index outside of FStack's range.")
                    else this.Back.[index']
                else this.Front.[index]
            else raise (IndexOutOfRangeException "Cannot index outside of FStack's range.")

    let rec private balance stack =
        if length stack > 0 then
            let buffer = stack |> length |> double |> Math.Sqrt |> int
            if stack.Back.Length > buffer * 2 then
                let front = Array.zeroCreate (stack.Front.Length + buffer)
                let back = Array.zeroCreate (stack.Back.Length - buffer)
                Array.blit stack.Front 0 front 0 stack.Front.Length
                Array.blit stack.Back 0 front stack.Front.Length buffer
                Array.blit stack.Back buffer back 0 back.Length
                { Front = front; Back = back }
            elif stack.Back.Length = 0 then
                let front = Array.zeroCreate (stack.Front.Length - buffer)
                let back = Array.zeroCreate buffer
                Array.blit stack.Front front.Length back 0 buffer
                Array.blit stack.Front 0 front 0 front.Length
                { Front = front; Back = back }
            else stack
        else stack

    /// The number of elements in an FStack.
    and length stack =
        stack.Front.Length +
        stack.Back.Length
        
    /// Check that an FStack has no elements.
    let isEmpty stack =
        length stack = 0

    /// Check that an FSatck has one or more elements.
    let notEmpty stack =
        length stack <> 0

    /// Make an FStack from a sequence of values.
    let ofSeq seq =
        let stack = { Front = Seq.toArray seq; Back = [||] }
        balance stack

    /// Make an FStack from a list of values.
    let ofList lst =
        let stack = { Front = List.toArray lst; Back = [||] }
        balance stack

    /// Make an FStack from an array of values.
    let ofArray arr =
        let stack = { Front = [||]; Back = arr }
        balance stack

    /// Convert an FStack to a seq.
    let toSeq (stack : 'a FStack) =
        stack :> 'a seq

    /// Convert an FStack to a list.
    let toList (stack : 'a FStack) =
        stack :> 'a seq |> Seq.toList

    /// Convert an FStack to an array.
    let toArray (stack : 'a FStack) =
        Array.append stack.Front stack.Back

    /// Fold over the elements of an FStack.
    let fold (f : 'a -> 'b -> 'a) s (stack : 'b FStack) =
        stack |> toSeq |> Seq.fold f s

    /// Map over the elements of an FStack.
    let map f (stack : 'a FStack) =
        stack |> toSeq |> Seq.map f |> ofSeq

    /// Filter the elements of an FStack.
    let filter f (stack : 'a FStack) =
        stack |> toSeq |> Seq.filter f |> ofSeq

    /// Get the first element of an FStack or raise an IndexOutOfRangeException.
    let head stack =
        stack.Front.[0]

    /// Get the first element of an FStack or None.
    let tryHead stack =
        if stack.Front.Length <> 0
        then Some stack.Front.[0]
        else None

    /// Remove all elements from an FStack that satisfy the given predicate.
    let remove pred stack =
        let stack = { Front = Array.remove pred stack.Front; Back = Array.remove pred stack.Back }
        balance stack

    /// Remove an element at the given index or raise IndexOutOfRangeException.
    let removeAt index stack =
        let arr = toArray stack
        let arr = Array.removeAt index arr
        let stack = { Front = arr; Back = [||] }
        balance stack
        
    /// Replace all elements from an FStack that satisfy the given predicate with the given value.
    let replace pred replacement stack =
        { Front = Array.replace pred replacement stack.Front
          Back = Array.replace pred replacement stack.Back }

    /// Replace an element at the given index with the given value or raise IndexOutOfRangeException.
    let replaceAt index replacement stack =
        if index < stack.Front.Length then
            let front = Array.copy stack.Front
            front.[index] <- replacement
            { stack with Front = front }
        else
            let index' = index - stack.Front.Length
            if index' < stack.Back.Length then
                let back = Array.copy stack.Back
                back.[index'] <- replacement
                { stack with Back = back }
            else raise (IndexOutOfRangeException "Cannot index outside of FStack's range.")

    /// Attempt to find the element in an FStack that satisfies the given predicate.
    let tryFind pred stack =
        match Array.tryFind pred stack.Front with
        | None -> Array.tryFind pred stack.Back
        | Some item -> Some item

    /// Find the element in an FStack that satisfies the given predicate or raise ArgumentException.
    let find pred stack =
        Option.get (tryFind pred stack)

    /// Index the FStack at the given 'a offset or return None.
    let tryIndex index stack =
        if index >= 0 then
            if index >= stack.Front.Length then 
                let index' = index - stack.Front.Length
                if index' >= stack.Back.Length
                then None
                else Some stack.Back.[index']
            else Some stack.Front.[index]
        else None

    /// Index the FStack at the given 'a offset or raise IndexOutOfRangeException.
    let index i (stack : 'a FStack) =
        stack.[i]

    /// Add an element to the end of an FStack.
    let conj a stack =
        let stack = { Front = stack.Front; Back = Array.add a stack.Back }
        balance stack

    /// Attempt to pop an element off the given FStack.
    let tryUnconj stack =
        match stack.Back with
        | [||] ->
            match stack.Front with
            | [||] -> None
            | _ ->
                let front = Array.zeroCreate (stack.Front.Length - 1)
                Array.blit stack.Front 0 front 0 front.Length
                let stack = { Front = front; Back = stack.Back }
                Some (balance stack)
        | _ ->
            let back = Array.zeroCreate (stack.Back.Length - 1)
            Array.blit stack.Back 0 back 0 back.Length
            let stack = { Front = stack.Front; Back = back }
            Some (balance stack)

    /// Attempt to pop an element off the given FStack or raise InvalidOperationException.
    let unconj stack =
        match tryUnconj stack with
        | Some stack -> stack
        | None -> raise (InvalidOperationException "Cannot unconj an empty FStack.")

    /// Check that an FStack contains the given item.
    let contains item stack =
        Array.contains item stack.Front ||
        Array.contains item stack.Back

    /// Make an FStack with a single element.
    let singleton a =
        { Front = [|a|]; Back = [||] }

    /// The empty FStack.
    let empty =
        { Front = [||]; Back = [||] }

/// A functional stack with fast iteration and amortized sqrt n conj.
/// NOTE: not supported by SymbolicConverter.
type 'a FStack = 'a FStack.FStack
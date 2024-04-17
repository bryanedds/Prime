// Prime - A PRIMitivEs code library.
// Copyright reserved for FSharpx.

namespace Prime
open System

/// FQueue is an ordered linear data structure where elements are added at the end (right)
/// and inspected and removed at the beginning (left). Ordering is by insertion history.
/// The qualities of the Queue structure make elements first in, first out (fifo).
/// "head" inspects the first or left-most element in the structure, while "conj"
/// inserts an element at the end, or right of the structure.
/// Purely functional (immutable) Queue based on Okasaki's batched queue.
///
/// Source code taken from - https://github.com/fsprojects/FSharpx.Collections/blob/3f566db698c832ea34b8c1f715d6891b2591d9f9/src/FSharpx.Collections/Deque.fs
/// Licensed under Apache-2.0 by its original authors.
[<DefaultValue "[]">]
type FQueue<'T>(front: list<'T>, rBack: list<'T>) =
    let mutable hashCode = None
    member internal this.front = front
    member internal this.rBack = rBack

    override this.GetHashCode() =
        match hashCode with
        | None ->
            let mutable hash = 1

            for x in this do
                hash <- 31 * hash + Unchecked.hash x

            hashCode <- Some hash
            hash
        | Some hash -> hash

    override this.Equals(other) =
        if refEq (this :> obj) other
        then true
        else
            match other with
            | :? FQueue<'T> as y ->
                if refEq this.front y.front && refEq this.rBack y.rBack then true
                else (this :> System.IEquatable<FQueue<'T>>).Equals y
            | _ -> false

    ///O(1). Returns a new queue with the element added to the end. (Enqueue)
    member this.Conj x =
        match front, x :: rBack with
        | [], r -> FQueue((List.rev r), [])
        | f, r -> FQueue(f, r)

    ///O(1). Returns the first element. (Peek)
    member this.Head =
        match front with
        | hd :: _ -> hd
        | _ -> raise(new System.Exception("Queue is empty"))

    ///O(1). Returns option first element
    member this.TryHead =
        match front with
        | hd :: _ -> Some(hd)
        | _ -> None

    ///O(1). Returns true if the queue has no elements.
    member this.IsEmpty = front.IsEmpty

    ///O(1). Returns true if the queue has elements.
    member this.NotEmpty = not front.IsEmpty

    ///O(1). Returns the count of elememts.
    member this.Length = front.Length + rBack.Length

    ///O(n). Returns queue reversed.
    member this.Rev() =
        match rBack, front with
        | [], r -> FQueue((List.rev r), [])
        | f, r -> FQueue(f, r)

    ///O(1) amortized, O(n) worst-case. Returns a new queue of the elements trailing the first element. (Dequeue)
    member this.Tail =
        match front with
        | _ :: tl ->
            match tl, rBack with
            | [], r -> FQueue((List.rev r), [])
            | f, r -> FQueue(f, r)
        | _ -> raise(new System.Exception("Queue is empty"))

    ///O(1) amortized, O(n) worst-case. Returns option queue of the elements trailing the first element.
    member this.TryTail =
        match front with
        | _ :: tl ->
            match tl, rBack with
            | [], r -> Some(FQueue((List.rev r), []))
            | f, r -> Some(FQueue(f, r))
        | _ -> None

    ///O(1) amortized, O(n) worst-case. Returns the first element and tail.
    member this.Uncons =
        match front with
        | hd :: tl ->
            hd,
            (match tl, rBack with
             | [], r -> FQueue((List.rev r), [])
             | f, r -> FQueue(f, r))
        | _ -> raise(new System.Exception("Queue is empty"))

    ///O(1) amortized, O(n) worst-case. Returns option first element and tail.
    member this.TryUncons =
        match front with
        | hd :: tl ->
            match tl, rBack with
            | [], r -> Some(hd, FQueue((List.rev r), []))
            | f, r -> Some(hd, FQueue(f, r))
        | _ -> None

    ///O(1). Returns a queue of the list
    static member OfList xs =
        FQueue<'T>(xs, [])

    ///O(n). Returns a queue of the seq.
    static member OfSeq xs =
        FQueue<'T>((List.ofSeq xs), [])

    interface System.IEquatable<FQueue<'T>> with
        member this.Equals(y: FQueue<'T>) =
            if this.Length <> y.Length then false
            else if this.GetHashCode() <> y.GetHashCode() then false
            else Seq.forall2 (Unchecked.equals) this y

    interface System.Collections.Generic.IEnumerable<'T> with
        override this.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> =
            let e = seq {
                yield! front
                yield! (List.rev rBack)
            }

            e.GetEnumerator()

    interface System.Collections.IEnumerable with
        override this.GetEnumerator() =
            (this :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator

    interface System.Collections.Generic.IReadOnlyCollection<'T> with
        member this.Count = this.Length

[<RequireQualifiedAccess>]
module FQueue =

    ///O(1) amortized, O(n), worst case. Returns the first element and tail.
    let (|Cons|Nil|)(q: FQueue<'T>) =
        match q.TryUncons with
        | Some(a, b) -> Cons(a, b)
        | None -> Nil

    ///O(1). Returns a new queue with the element added to the end. (enqueue)
    let inline conj (x: 'T) (q: FQueue<'T>) = q.Conj x

    ///O(1). Returns queue of no elements.
    let empty<'T> : FQueue<'T> = FQueue<_>([], [])

    ///O(n). Applies a function to each element of the queue, threading an accumulator argument through the computation, left to right.
    let fold (f: ('State -> 'T -> 'State)) (state: 'State) (q: FQueue<'T>) =
        let s = List.fold f state q.front
        List.fold f s (List.rev q.rBack)

    ///O(n). Applies a function to each element of the queue, threading an accumulator argument through the computation, right to left.
    let foldBack (f: ('T -> 'State -> 'State)) (q: FQueue<'T>) (state: 'State) =
        let s = List.foldBack f (List.rev q.rBack) state
        (List.foldBack f q.front s)

    ///O(1). Returns the first element. (peek)
    let inline head(q: FQueue<'T>) = q.Head

    ///O(1). Returns option first element.
    let inline tryHead(q: FQueue<'T>) = q.TryHead

    ///O(1). Returns true if the queue has no elements.
    let inline isEmpty(q: FQueue<'T>) = q.IsEmpty

    ///O(1). Returns true if the queue has elements.
    let inline notEmpty(q: FQueue<'T>) = q.NotEmpty

    ///O(1). Returns the count of elememts.
    let inline length(q: FQueue<'T>) = q.Length

    ///O(1). Returns a queue of the list
    let ofList xs = FQueue.OfList xs

    ///O(n). Returns a queue of the seq.
    let ofSeq xs = FQueue.OfSeq xs

    ///O(n). Returns queue reversed.
    let inline rev(q: FQueue<'T>) = q.Rev()

    ///O(1). The singleton queue.
    let inline singleton x = conj x empty

    ///O(1) amortized, O(n) worst-case. Returns a new queue of the elements trailing the first element. (dequeue)
    let inline tail(q: FQueue<'T>) = q.Tail

    ///O(1) amortized, O(n) worst-case. Returns option queue of the elements trailing the first element
    let inline tryTail(q: FQueue<'T>) = q.TryTail

    ///O(n). Views the given queue as a sequence.
    let inline toSeq(q: FQueue<'T>) = q :> seq<'T>

    ///O(1) amortized, O(n) worst-case. Returns the first element and tail.
    let inline uncons(q: FQueue<'T>) = q.Uncons

    ///O(1) amortized, O(n) worst-case. Returns option first element and tail.
    let inline tryUncons(q: FQueue<'T>) = q.TryUncons
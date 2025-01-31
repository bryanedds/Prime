// Prime - A PRIMitivEs code library.
// Copyright reserved for FSharpx.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

/// Double-ended queue is an ordered linear structure implementing the signature of List
/// (head, tail, cons) as well as the mirror-image Vector signature (last, initial, conj). "head" inspects
/// the first or left-most element in the structure, while "last" inspects the last or
/// right-most element. "rev" (reverse) has time complexity O(1). Ordering is by insertion history.
///
/// Source code taken from - https://github.com/fsprojects/FSharpx.Collections/blob/3f566db698c832ea34b8c1f715d6891b2591d9f9/src/FSharpx.Collections/Queue.fs
/// Licensed under Apache-2.0 by its original authors.
type [<DefaultValue "[]">] FDeque<'T>(front, rBack) =
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
            | :? FDeque<'T> as y ->
                if refEq this.front y.front && refEq this.rBack y.rBack then true
                else (this :> IEquatable<FDeque<'T>>).Equals(y)
            | _ -> false

    /// O(1). Returns a new deque with the element added to the end.
    member this.Conj x =
        FDeque(front, x :: rBack)

    /// O(1). Returns a new deque with the element added to the beginning.
    member this.Cons x =
        FDeque(x :: front, rBack)

    /// O(1) amortized, O(n), worst case. Returns first element.
    member this.Head =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | hd :: _, _ -> hd
        | [], xs -> List.rev xs |> List.head

    /// O(1) amortized, O(n), worst case. Returns option first element.
    member this.TryHead: 'T option =
        match front, rBack with
        | [], [] -> None
        | hd :: _, _ -> Some(hd)
        | [], xs ->
            let x = List.rev xs |> List.head
            Some(x)

    /// O(1) amortized, O(n), worst case. Returns the first element.
    member this.Initial =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | _, _ :: xs -> FDeque(front, xs)
        | _, [] -> //splits front in two, favoring frontbot for odd length
            let half = front.Length / 2
            let a = Array.ofList front
            let frontA = Array.create half front.Head
            let rBackA = Array.create ((front.Length - half) - 1) front.Head
            Array.blit a 0 frontA 0 frontA.Length
            Array.blit a frontA.Length rBackA 0 rBackA.Length
            let rBackA' = Array.rev rBackA
            FDeque(List.ofArray frontA, List.ofArray rBackA')

    /// O(1) amortized, O(n), worst case. Returns option first element.
    member this.TryInitial =
        match front, rBack with
        | [], [] -> None
        | _, _ :: xs -> Some(FDeque(front, xs))
        | _, [] -> //splits front in two, favoring frontbot for odd length
            let half = front.Length / 2
            let a = Array.ofList front
            let frontA = Array.create half front.Head
            let rBackA = Array.create ((front.Length - half) - 1) front.Head
            Array.blit a 0 frontA 0 frontA.Length
            Array.blit a frontA.Length rBackA 0 rBackA.Length
            let rBackA' = Array.rev rBackA
            Some(FDeque(List.ofArray frontA, List.ofArray rBackA'))

    /// O(1). Returns true if the deque has no elements.
    member this.IsEmpty =
        match front, rBack with
        | [], [] -> true
        | _ -> false

    /// O(1) amortized, O(n), worst case. Returns the last element.
    member this.Last =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | xs, [] -> List.rev xs |> List.head
        | _, hd :: _ -> hd

    /// O(1) amortized, O(n), worst case. Returns option last element.
    member this.TryLast =
        match front, rBack with
        | [], [] -> None
        | xs, [] -> Some(List.rev xs |> List.head)
        | _, hd :: _ -> Some(hd)

    /// O(1). Returns the count of elememts.
    member this.Length = front.Length + rBack.Length

    /// O(1). Returns deque reversed.
    member this.Rev = (new FDeque<'T>(rBack, front))

    /// O(1) amortized, O(n), worst case. Returns a new deque of the elements trailing the first element.
    member this.Tail =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | _ :: xs, _ -> FDeque(xs, rBack)
        | [], [ _ ] -> FDeque([], [])
        | _, _ -> //splits rear in two, favoring rearbot for odd length
            let a = Array.ofList rBack
            let half = a.Length / 2
            let frontA = Array.create half Unchecked.defaultof<_>
            let rBackA = Array.create ((a.Length - half) - 1) Unchecked.defaultof<_>
            Array.blit a 0 rBackA 0 rBackA.Length
            Array.blit a rBackA.Length frontA 0 frontA.Length
            let frontA' = Array.rev frontA
            FDeque(List.ofArray frontA', List.ofArray rBackA)

    /// O(1) amortized, O(n), worst case. Returns option deque of the elements trailing the first element.
    member this.TryTail =
        match front, rBack with
        | [], [] -> None
        | _ :: xs, _ -> Some(FDeque(xs, rBack))
        | _, _ -> //splits rear in two, favoring rearbot for odd length
            let half = rBack.Length / 2
            let a = Array.ofList rBack
            let frontA = Array.create half rBack.Head
            let rBackA = Array.create ((rBack.Length - half) - 1) rBack.Head
            Array.blit a 0 rBackA 0 rBackA.Length
            Array.blit a rBackA.Length frontA 0 frontA.Length
            let frontA' = Array.rev frontA
            Some(FDeque(List.ofArray frontA', List.ofArray rBackA))

    /// O(1) amortized, O(n), worst case. Returns the first element and tail.
    member this.Uncons =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | _, _ -> this.Head, this.Tail

    /// O(1) amortized, O(n), worst case. Returns option first element and tail.
    member this.TryUncons =
        match front, rBack with
        | [], [] -> None
        | _, _ -> Some(this.Head, this.Tail)

    /// O(1) amortized, O(n), worst case. Returns init and the last element.
    member this.Unconj =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | _, _ -> this.Initial, this.Last

    /// O(1) amortized, O(n), worst case. Returns option init and the last element.
    member this.TryUnconj =
        match front, rBack with
        | [], [] -> None
        | _, _ -> Some(this.Initial, this.Last)

    /// O(1). Returns a deque of the list
    static member OfList xs =
        FDeque<'T>(xs, [])

    /// O(n). Returns a deque of the seq.
    static member OfSeq xs =
        FDeque<'T>((List.ofSeq xs), [])

    interface IEquatable<FDeque<'T>> with
        member this.Equals(y) =
            if this.Length <> y.Length then false
            else if this.GetHashCode() <> y.GetHashCode() then false
            else Seq.forall2 (Unchecked.equals) this y

    interface IEnumerable<'T> with
        member this.GetEnumerator() =
            let e = seq {
                yield! front
                yield! (List.rev rBack)
            }

            e.GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() =
            (this :> _ seq).GetEnumerator() :> IEnumerator

    interface IReadOnlyCollection<'T> with
        member this.Count = this.Length

[<RequireQualifiedAccess>]
module FDeque =

    /// O(1) amortized, O(n), worst case. Returns the first element and tail.
    let (|Cons|Nil|)(q: FDeque<'T>) =
        match q.TryUncons with
        | Some(a, b) -> Cons(a, b)
        | None -> Nil

    /// O(1) amortized, O(n), worst case. Returns initial and last.
    let (|Conj|Nil|)(q: FDeque<'T>) =
        match q.TryUnconj with
        | Some(a, b) -> Conj(a, b)
        | None -> Nil

    /// O(1). Returns a new deque with the element added to the end.
    let inline conj (x: 'T) (q: FDeque<'T>) = q.Conj x

    /// O(1). Returns a new deque with the element added to the beginning.
    let inline cons (x: 'T) (q: FDeque<'T>) = q.Cons x

    /// O(1). Returns deque of no elements.
    let empty<'T> = FDeque<'T>(List.Empty, List.Empty)

    /// O(n). Applies a function to each element of the deque, threading an accumulator argument through the computation, left to right
    let fold (f: ('State -> 'T -> 'State)) (state: 'State) (q: FDeque<'T>) =
        let s = List.fold f state q.front
        List.fold f s (List.rev q.rBack)

    /// O(n). Applies a function to each element of the deque, threading an accumulator argument through the computation, right to left
    let foldBack (f: ('T -> 'State -> 'State)) (q: FDeque<'T>) (state: 'State) =
        let s = List.foldBack f (List.rev q.rBack) state
        (List.foldBack f q.front s)

    /// O(1) amortized, O(n), worst case. Returns the first element.
    let inline head(q: FDeque<'T>) = q.Head

    /// O(1) amortized, O(n), worst case. Returns option first element.
    let inline tryHead(q: FDeque<'T>) = q.TryHead

    /// O(1) amortized, O(n), worst case. Returns a new deque of the elements before the last element.
    let inline initial(q: FDeque<'T>) = q.Initial

    /// O(1) amortized, O(n), worst case. Returns option deque of the elements before the last element.
    let inline tryInitial(q: FDeque<'T>) = q.TryInitial

    /// O(1). Returns true if the deque has no elements.
    let inline isEmpty(q: FDeque<'T>) = q.IsEmpty

    /// O(1) amortized, O(n), worst case. Returns the last element.
    let inline last(q: FDeque<'T>) = q.Last

    /// O(1) amortized, O(n), worst case. Returns option last element.
    let inline tryLast(q: FDeque<'T>) = q.TryLast

    /// O(1). Returns the count of elememts.
    let inline length(q: FDeque<'T>) = q.Length

    /// Check that an FStack contains the given item.
    let contains item (q: FDeque<'T>) = Seq.contains item q

    /// O(n), worst case. Returns a deque of the two lists concatenated.
    let ofCatLists (xs: 'T list) (ys: 'T list) = FDeque<'T>(xs, (List.rev ys))

    /// O(1). Returns a deque of the list
    let ofList xs = FDeque.OfList xs

    /// O(n). Returns a deque of the seq.
    let ofSeq xs = FDeque.OfSeq xs

    /// O(1). Returns deque reversed.
    let inline rev(q: FDeque<'T>) = q.Rev

    /// O(1). Returns a deque of one element.
    let singleton(x: 'T) = FDeque<'T>([ x ], List.Empty)

    /// O(1) amortized, O(n), worst case. Returns a new deque of the elements trailing the first element.
    let inline tail(q: FDeque<'T>) = q.Tail

    /// O(1) amortized, O(n), worst case. Returns option deque of the elements trailing the first element.
    let inline tryTail(q: FDeque<'T>) = q.TryTail

    /// O(1) amortized, O(n), worst case. Returns init and the last element.
    let inline uncons(q: FDeque<'T>) = q.Uncons

    /// O(1) amortized, O(n), worst case. Returns option init and the last element.
    let inline tryUncons(q: FDeque<'T>) = q.TryUncons

    /// O(1) amortized, O(n), worst case. Returns the first element and tail.
    let inline unconj(q: FDeque<'T>) = q.Unconj

    /// O(n). Views the given deque as a sequence.
    let inline toSeq(q: FDeque<'T>) = q :> seq<'T>

    /// O(1) amortized, O(n), worst case. Returns option first element and tail.
    let inline tryUnconj(q: FDeque<'T>) = q.TryUnconj

    /// O(n). Filter implemented in terms of seq (to save some development time).
    let filter pred (q: FDeque<'T>) = Seq.filter pred q |> ofSeq

    /// O(n). Map implemented in terms of seq (to save some development time).
    let map mapper (q: FDeque<'T>) : FDeque<'T2> = Seq.map mapper q |> ofSeq

    /// O(n). Choose implemented in terms of seq (to save some development time).
    let choose chooser (q: FDeque<'T option>) : FDeque<'T2> = Seq.choose chooser q |> ofSeq

    /// O(n). Append implemented in terms of seq (to save some development time).
    let append (q: FDeque<'T>) (q2: FDeque<'T>) = Seq.append q q2 |> ofSeq

    /// O(n). Concat implemented in terms of seq (to save some development time).
    let concat (q: FDeque<'T seq>) = Seq.concat q |> ofSeq
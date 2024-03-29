namespace Prime
open System
open System.Collections
open System.Collections.Generic

/// Double-ended queue is an ordered linear linear structure implementing the signature of List
/// (head, tail, cons) as well as the mirror-image Vector signature (last, initial, conj). "head" inspects
/// the first or left-most element in the structure, while "last" inspects the last or
/// right-most element. "rev" (reverse) has time complexity O(1). Ordering is by insertion history.
///
/// Source code taken from - https://github.com/fsprojects/FSharpx.Collections/blob/3f566db698c832ea34b8c1f715d6891b2591d9f9/src/FSharpx.Collections/Queue.fs
/// Licensed under Apache-2.0 by its original authors.
///
/// TODO: rewrite this to cut association with FSharpx.
[<DefaultValue "[]">]
type Deque<'T>(front, rBack) =
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
        match other with
        | :? Deque<'T> as y -> (this :> IEquatable<Deque<'T>>).Equals(y)
        | _ -> false

    ///O(1). Returns a new deque with the element added to the end.
    member this.Conj x =
        Deque(front, x :: rBack)

    ///O(1). Returns a new deque with the element added to the beginning.
    member this.Cons x =
        Deque(x :: front, rBack)

    ///O(1) amortized, O(n), worst case. Returns first element.
    member this.Head =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | hd :: _, _ -> hd
        | [], xs -> List.rev xs |> List.head

    ///O(1) amortized, O(n), worst case. Returns option first element.
    member this.TryHead: 'T option =
        match front, rBack with
        | [], [] -> None
        | hd :: _, _ -> Some(hd)
        | [], xs ->
            let x = List.rev xs |> List.head
            Some(x)

    ///O(1) amortized, O(n), worst case. Returns the first element.
    member this.Initial =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | _, _ :: xs -> Deque(front, xs)
        | _, [] -> //splits front in two, favoring frontbot for odd length
            let half = front.Length / 2
            let a = Array.ofList front
            let frontA = Array.create half front.Head
            let rBackA = Array.create ((front.Length - half) - 1) front.Head
            Array.blit a 0 frontA 0 frontA.Length
            Array.blit a frontA.Length rBackA 0 rBackA.Length
            let rBackA' = Array.rev rBackA
            Deque(List.ofArray frontA, List.ofArray rBackA')

    ///O(1) amortized, O(n), worst case. Returns option first element.
    member this.TryInitial =
        match front, rBack with
        | [], [] -> None
        | _, _ :: xs -> Some(Deque(front, xs))
        | _, [] -> //splits front in two, favoring frontbot for odd length
            let half = front.Length / 2
            let a = Array.ofList front
            let frontA = Array.create half front.Head
            let rBackA = Array.create ((front.Length - half) - 1) front.Head
            Array.blit a 0 frontA 0 frontA.Length
            Array.blit a frontA.Length rBackA 0 rBackA.Length
            let rBackA' = Array.rev rBackA
            Some(Deque(List.ofArray frontA, List.ofArray rBackA'))

    ///O(1). Returns true if the deque has no elements.
    member this.IsEmpty =
        match front, rBack with
        | [], [] -> true
        | _ -> false

    ///O(1) amortized, O(n), worst case. Returns the last element.
    member this.Last =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | xs, [] -> List.rev xs |> List.head
        | _, hd :: _ -> hd

    ///O(1) amortized, O(n), worst case. Returns option last element.
    member this.TryLast =
        match front, rBack with
        | [], [] -> None
        | xs, [] -> Some(List.rev xs |> List.head)
        | _, hd :: _ -> Some(hd)

    ///O(1). Returns the count of elememts.
    member this.Length = front.Length + rBack.Length

    ///O(1). Returns deque reversed.
    member this.Rev = (new Deque<'T>(rBack, front))

    ///O(1) amortized, O(n), worst case. Returns a new deque of the elements trailing the first element.
    member this.Tail =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | _ :: xs, _ -> Deque(xs, rBack)
        | [], [ _ ] -> Deque([], [])
        | _, _ -> //splits rear in two, favoring rearbot for odd length
            let a = Array.ofList rBack
            let half = a.Length / 2
            let frontA = Array.create half Unchecked.defaultof<_>
            let rBackA = Array.create ((a.Length - half) - 1) Unchecked.defaultof<_>
            Array.blit a 0 rBackA 0 rBackA.Length
            Array.blit a rBackA.Length frontA 0 frontA.Length
            let frontA' = Array.rev frontA
            Deque(List.ofArray frontA', List.ofArray rBackA)

    ///O(1) amortized, O(n), worst case. Returns option deque of the elements trailing the first element.
    member this.TryTail =
        match front, rBack with
        | [], [] -> None
        | _ :: xs, _ -> Some(Deque(xs, rBack))
        | _, _ -> //splits rear in two, favoring rearbot for odd length
            let half = rBack.Length / 2
            let a = Array.ofList rBack
            let frontA = Array.create half rBack.Head
            let rBackA = Array.create ((rBack.Length - half) - 1) rBack.Head
            Array.blit a 0 rBackA 0 rBackA.Length
            Array.blit a rBackA.Length frontA 0 frontA.Length
            let frontA' = Array.rev frontA
            Some(Deque(List.ofArray frontA', List.ofArray rBackA))

    ///O(1) amortized, O(n), worst case. Returns the first element and tail.
    member this.Uncons =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | _, _ -> this.Head, this.Tail

    ///O(1) amortized, O(n), worst case. Returns option first element and tail.
    member this.TryUncons =
        match front, rBack with
        | [], [] -> None
        | _, _ -> Some(this.Head, this.Tail)

    ///O(1) amortized, O(n), worst case. Returns init and the last element.
    member this.Unconj =
        match front, rBack with
        | [], [] -> raise(new System.Exception("Deque is empty"))
        | _, _ -> this.Initial, this.Last

    ///O(1) amortized, O(n), worst case. Returns option init and the last element.
    member this.TryUnconj =
        match front, rBack with
        | [], [] -> None
        | _, _ -> Some(this.Initial, this.Last)

    ///O(1). Returns a deque of the list
    static member OfList xs =
        Deque<'T>(xs, [])

    ///O(n). Returns a deque of the seq.
    static member OfSeq xs =
        Deque<'T>((List.ofSeq xs), [])

    interface IEquatable<Deque<'T>> with
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
module Deque =

    ///O(1) amortized, O(n), worst case. Returns the first element and tail.
    let (|Cons|Nil|)(q: Deque<'T>) =
        match q.TryUncons with
        | Some(a, b) -> Cons(a, b)
        | None -> Nil

    ///O(1) amortized, O(n), worst case. Returns option first element and tail.
    let (|Conj|Nil|)(q: Deque<'T>) =
        match q.TryUnconj with
        | Some(a, b) -> Conj(a, b)
        | None -> Nil

    ///O(1). Returns a new deque with the element added to the end.
    let inline conj (x: 'T) (q: Deque<'T>) =
        (q.Conj x)

    ///O(1). Returns a new deque with the element added to the beginning.
    let inline cons (x: 'T) (q: Deque<'T>) = q.Cons x

    ///O(1). Returns deque of no elements.
    let empty<'T> = Deque<'T>(List.Empty, List.Empty)

    ///O(n). Applies a function to each element of the deque, threading an accumulator argument through the computation, left to right
    let fold (f: ('State -> 'T -> 'State)) (state: 'State) (q: Deque<'T>) =
        let s = List.fold f state q.front
        List.fold f s (List.rev q.rBack)

    ///O(n). Applies a function to each element of the deque, threading an accumulator argument through the computation, right to left
    let foldBack (f: ('T -> 'State -> 'State)) (q: Deque<'T>) (state: 'State) =
        let s = List.foldBack f (List.rev q.rBack) state
        (List.foldBack f q.front s)

    ///O(1) amortized, O(n), worst case. Returns the first element.
    let inline head(q: Deque<'T>) = q.Head

    ///O(1) amortized, O(n), worst case. Returns option first element.
    let inline tryHead(q: Deque<'T>) = q.TryHead

    ///O(1) amortized, O(n), worst case. Returns a new deque of the elements before the last element.
    let inline initial(q: Deque<'T>) = q.Initial

    ///O(1) amortized, O(n), worst case. Returns option deque of the elements before the last element.
    let inline tryInitial(q: Deque<'T>) =
        q.TryInitial

    ///O(1). Returns true if the deque has no elements.
    let inline isEmpty(q: Deque<'T>) = q.IsEmpty

    ///O(1) amortized, O(n), worst case. Returns the last element.
    let inline last(q: Deque<'T>) = q.Last

    ///O(1) amortized, O(n), worst case. Returns option last element.
    let inline tryLast(q: Deque<'T>) = q.TryLast

    ///O(1). Returns the count of elememts.
    let inline length(q: Deque<'T>) = q.Length

    ///O(n), worst case. Returns a deque of the two lists concatenated.
    let ofCatLists (xs: 'T list) (ys: 'T list) =
        Deque<'T>(xs, (List.rev ys))

    ///O(1). Returns a deque of the list
    let ofList xs = Deque<'T>.OfList xs

    ///O(n). Returns a deque of the seq.
    let ofSeq xs = Deque<'T>.OfSeq xs

    ///O(1). Returns deque reversed.
    let inline rev(q: Deque<'T>) = q.Rev

    ///O(1). Returns a deque of one element.
    let singleton(x: 'T) = Deque<'T>([ x ], List.Empty)

    ///O(1) amortized, O(n), worst case. Returns a new deque of the elements trailing the first element.
    let inline tail(q: Deque<'T>) = q.Tail

    ///O(1) amortized, O(n), worst case. Returns option deque of the elements trailing the first element.
    let inline tryTail(q: Deque<'T>) = q.TryTail

    ///O(1) amortized, O(n), worst case. Returns init and the last element.
    let inline uncons(q: Deque<'T>) = q.Uncons

    ///O(1) amortized, O(n), worst case. Returns option init and the last element.
    let inline tryUncons(q: Deque<'T>) = q.TryUncons

    ///O(1) amortized, O(n), worst case. Returns the first element and tail.
    let inline unconj(q: Deque<'T>) = q.Unconj

    ///O(n). Views the given deque as a sequence.
    let inline toSeq(q: Deque<'T>) = q :> seq<'T>

    ///O(1) amortized, O(n), worst case. Returns option first element and tail.
    let inline tryUnconj(q: Deque<'T>) = q.TryUnconj
// Prime - A PRIMitivEs code library.
// Copyright reserved for FSharpx.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

/// Functional list with fast equality.
///
/// Source code taken from - https://github.com/fsprojects/FSharpx.Collections/blob/3f566db698c832ea34b8c1f715d6891b2591d9f9/src/FSharpx.Collections/Queue.fs
/// Licensed under Apache-2.0 by its original authors.
type [<DefaultValue "[]">] FList<'T>(front) =
    let mutable hashCode = None
    member internal this.front = front

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
            | :? FList<'T> as y ->
                if refEq this.front y.front then true
                else (this :> IEquatable<FList<'T>>).Equals(y)
            | _ -> false

    /// Returns a new deque with the element added to the beginning.
    member this.Cons x =
        FList(x :: front)

    /// Returns first element.
    member this.Head =
        match front with
        | [] -> raise(new System.Exception("Deque is empty"))
        | hd :: _ -> hd

    /// Returns option first element.
    member this.TryHead: 'T option =
        match front with
        | [] -> None
        | hd :: _ -> Some(hd)

    /// Returns true if the deque has no elements.
    member this.IsEmpty =
        match front with
        | [] -> true
        | _ -> false

    /// Returns the last element.
    member this.Last =
        match front with
        | [] -> raise(new System.Exception("Deque is empty"))
        | xs -> List.rev xs |> List.head

    /// Returns option last element.
    member this.TryLast =
        match front with
        | [] -> None
        | xs -> Some(List.rev xs |> List.head)

    /// Returns the count of elememts.
    member this.Length = front.Length

    /// Returns deque reversed.
    member this.Rev = (new FList<'T>(List.rev front))

    /// Returns a new deque of the elements trailing the first element.
    member this.Tail =
        match front with
        | [] -> raise(new System.Exception("Deque is empty"))
        | _ :: xs -> FList(xs)

    /// Returns option deque of the elements trailing the first element.
    member this.TryTail =
        match front with
        | [] -> None
        | _ :: xs -> Some(FList(xs))

    /// Returns the first element and tail.
    member this.Uncons =
        match front with
        | [] -> raise(new System.Exception("Deque is empty"))
        | _ -> this.Head, this.Tail

    /// Returns option first element and tail.
    member this.TryUncons =
        match front with
        | [] -> None
        | _ -> Some(this.Head, this.Tail)

    /// Returns an an FList of the list
    static member OfList xs =
        FList<'T>(xs)

    /// Returns an an FList of the seq.
    /// NOTE: do not move / rename this function as reflection code relies on it being exactly here!
    static member OfSeq xs =
        FList<'T>(List.ofSeq xs)

    interface IEquatable<FList<'T>> with
        member this.Equals(y) =
            if this.Length <> y.Length then false
            else if this.GetHashCode() <> y.GetHashCode() then false
            else Seq.forall2 (Unchecked.equals) this y

    interface IEnumerable<'T> with
        member this.GetEnumerator() =
            (front :> _ seq).GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() =
            (front :> _ seq).GetEnumerator() :> IEnumerator

    interface IReadOnlyCollection<'T> with
        member this.Count = this.Length

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FList =

    /// Returns the first element and tail.
    let (|Cons|Nil|)(q: FList<'T>) =
        match q.TryUncons with
        | Some(a, b) -> Cons(a, b)
        | None -> Nil

    /// Returns a new deque with the element added to the beginning.
    let inline cons (x: 'T) (q: FList<'T>) = q.Cons x

    /// Returns deque of no elements.
    let empty<'T> = FList<'T>(List.Empty)

    /// Applies a function to each element of the deque, threading an accumulator argument through the computation, left to right
    let fold (f: ('State -> 'T -> 'State)) (state: 'State) (q: FList<'T>) =
        List.fold f state q.front

    /// Applies a function to each element of the deque, threading an accumulator argument through the computation, right to left
    let foldBack (f: ('T -> 'State -> 'State)) (q: FList<'T>) (state: 'State) =
        List.foldBack f q.front state

    /// Returns the first element.
    let inline head(q: FList<'T>) = q.Head

    /// Returns option first element.
    let inline tryHead(q: FList<'T>) = q.TryHead

    /// Returns true if the deque has no elements.
    let inline isEmpty(q: FList<'T>) = q.IsEmpty

    /// Returns the last element.
    let inline last(q: FList<'T>) = q.Last

    /// Returns option last element.
    let inline tryLast(q: FList<'T>) = q.TryLast

    /// Returns the count of elememts.
    let inline length(q: FList<'T>) = q.Length

    /// Returns an an FList of the list
    let ofList xs = FList.OfList xs

    /// Returns an an FList of the seq.
    let ofSeq xs = FList.OfSeq xs

    /// Returns deque reversed.
    let inline rev(q: FList<'T>) = q.Rev

    /// Returns an an FList of one element.
    let singleton(x: 'T) = FList<'T>([ x ])

    /// Returns a new deque of the elements trailing the first element.
    let inline tail(q: FList<'T>) = q.Tail

    /// Returns option deque of the elements trailing the first element.
    let inline tryTail(q: FList<'T>) = q.TryTail

    /// Returns init and the last element.
    let inline uncons(q: FList<'T>) = q.Uncons

    /// Returns option init and the last element.
    let inline tryUncons(q: FList<'T>) = q.TryUncons

    /// Views the given deque as a sequence.
    let inline toSeq(q: FList<'T>) = q :> seq<'T>

    /// O(n). Filter implemented in terms of seq (to save some development time).
    let filter pred (q: FList<'T>) = Seq.filter pred q |> ofSeq

    /// O(n). Map implemented in terms of seq (to save some development time).
    let map mapper (q: FList<'T>) : FList<'T2> = Seq.map mapper q |> ofSeq

    /// O(n). Choose implemented in terms of seq (to save some development time).
    let choose chooser (q: FList<'T option>) : FList<'T2> = Seq.choose chooser q |> ofSeq

    /// O(n). Append implemented in terms of seq (to save some development time).
    let append (q: FList<'T>) (q2: FList<'T>) = Seq.append q q2 |> ofSeq

    /// O(n). Concat implemented in terms of seq (to save some development time).
    let concat (q: FList<'T seq>) = Seq.concat q |> ofSeq

[<AutoOpen>]
module FListOperators =

    /// Returns the first element and tail.
    let (|Cons|Nil|)(q: FList<'T>) =
        match q.TryUncons with
        | Some(a, b) -> Cons(a, b)
        | None -> Nil
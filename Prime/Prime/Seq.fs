﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections
open Prime

[<RequireQualifiedAccess>]
module Seq =

    /// Check that a sequence is not empty.
    let inline notEmpty seq =
        not (Seq.isEmpty seq)

    /// Get Some head of the seq or None.
    let inline tryHead seq =
        Seq.tryFind tautology seq

    /// Get a seq head or a default value if there is none.
    let inline headOrDefault seq aDefault =
        match tryHead seq with
        | Some _ as head -> head
        | None -> aDefault

    /// Convert option values to definite values.
    let inline definitize opts =
        Seq.choose id opts

    /// Convert option values to definite values, returning an additional flag to indicate that all values were some.
    let definitizePlus opts =
        let struct (flag, list) =
            Seq.foldBack
                (fun opt struct (allDefinite, values) ->
                    match opt with
                    | Some value -> struct (allDefinite, value :: values)
                    | None -> struct (false, values))
                opts struct (true, [])
        (flag, Seq.ofList list)

    /// Fold with two inputs (plus state).
    let fold2 folder state seq seq2 =
        let zipped = Seq.zip seq seq2
        Seq.fold (fun state (a, b) -> folder state a b) state zipped

    /// Fold, now with a counter!
    let foldi folder state seq =
        let struct (_, result) =
            Seq.fold
                (fun struct (i, state) item -> struct (i + 1, folder i state item))
                struct (0, state)
                seq
        result

    /// Fold-back for seqs.
    let foldBack folder values state =
        List.foldBack folder (List.ofSeq values) state

    /// Check if no items satisfy a predicate in a seq.
    let fornone pred seq =
        let notPred = not << pred
        Seq.forall notPred seq

    /// A more tolerant and open-minded take.
    let tryTake (count : int) (seq : _ seq) =
        System.Linq.Enumerable.Take (seq, count)

    /// A more tolerant and open-minded skip.
    let trySkip (count : int) (seq : _ seq) =
        System.Linq.Enumerable.Skip (seq, count)

    /// Try to find a value.
    let tryFindPlus (pred : 'a -> 'b option) (seq : 'a seq) : 'b option =
        let mutable result = None
        let enr = seq.GetEnumerator ()
        while Option.isNone result && enr.MoveNext () do
            match pred enr.Current with
            | Some _ as found -> result <- found
            | None -> ()
        result

    /// Project the first sequence onto the second.
    let project projector (seq_ : 'a seq) (seq2 : 'b option seq) =
        use enr = seq_.GetEnumerator ()
        use enr2 = seq2.GetEnumerator ()
        seq {
            while enr.MoveNext () do
                let projection = 
                    if enr2.MoveNext () then
                        match projector enr2.Current with
                        | Some projection -> projection
                        | None -> enr.Current
                    else enr.Current
                yield projection }

    /// Implement a fold while folder results in Some.
    let foldWhile folder (state : 's) (seq : 't seq) =
        let mutable lastState = state
        let mutable stateOpt = Some lastState
        let mutable enr = seq.GetEnumerator ()
        while stateOpt.IsSome && enr.MoveNext () do
            lastState <- stateOpt.Value
            stateOpt <- folder lastState enr.Current
        match stateOpt with
        | Some state -> state
        | None -> lastState

    /// Implement a fold while folder results in Right.
    let foldWhileRight folder (state : Either<_, _>) (seq : 't seq) =
        let mutable state = state // make mutable
        let mutable enr = seq.GetEnumerator ()
        while Either.isRight state && enr.MoveNext () do
            state <- folder (Either.getRightValue state) enr.Current
        state

    /// Implement a fold until folder results in Some.
    let foldUntil folder (state : 's) (seq : 't seq) =
        let mutable isFirst = true // no do while necessitates this flag
        let mutable lastState = state
        let mutable stateOpt = Some lastState
        let mutable enr = seq.GetEnumerator ()
        while (isFirst || stateOpt.IsNone) && enr.MoveNext () do
            isFirst <- false
            lastState <- stateOpt.Value
            stateOpt <- folder lastState enr.Current
        match stateOpt with
        | Some state -> state
        | None -> lastState

    /// Implement a fold until folder results in Right.
    let foldUntilRight folder (state : Either<_, _>) (seq : 't seq) =
        let mutable state = state // make mutable
        let mutable enr = seq.GetEnumerator ()
        while Either.isLeft state && enr.MoveNext () do
            state <- folder (Either.getLeftValue state) enr.Current
        state

    /// Check that a predicate passes for NO items in a sequence.
    let inline notExists pred seq =
        not (Seq.exists pred seq)

    /// Split a sequence on a predicate.
    let split pred seq =
        let rec splitInner pred left right seq =
            match tryHead seq with
            | Some head ->
                if pred head
                then splitInner pred (head :: left) right (Seq.tail seq)
                else splitInner pred left (head :: right) (Seq.tail seq)
            | None -> struct (left, right)
        let struct (list, list2) = splitInner pred [] [] seq
        (List.rev list, List.rev list2)
        
    /// Eagerly evaluate a sequence
    let eval seq =
        seq |> Seq.toArray |> Seq.ofArray

[<RequireQualifiedAccess>]
module IEnumerable =

    /// Get the nth item in an enumerable.
    let item n (enum : IEnumerable) =
        let mutable i = 0
        let mutable enr = enum.GetEnumerator ()
        while enr.MoveNext () && i < n do i <- i + 1
        enr.Current
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System.Collections.Generic

[<RequireQualifiedAccess>]
module GenericList =

    /// Check that a list is empty.
    let isEmpty (list : _ List) =
        list.Count = 0

    /// Check that a list is non-empty.
    let notEmpty (list : _ List) =
        list.Count > 0

    /// Hash a list.
    let hash (list : _ List) =
        let mutable h = 0
        for item in list do
            h <- h ^^^ item.GetHashCode ()
        h

    /// Fold over list.
    let fold<'s, 't> folder (state : 's) (list : 't List) =
        let folder = OptimizedClosures.FSharpFunc<_, _, _>.Adapt folder
        let mutable state = state
        let mutable enr = list.GetEnumerator ()
        while enr.MoveNext () do
            state <- folder.Invoke (state, enr.Current)
        state

    /// Map over a list. A new list is produced.
    let map (mapper : 'a -> 'a) (list : 'a List) =
        let result = List<'a> ()
        for item in list do result.Add (mapper item)
        result

    /// Iterate over a list.
    let iter<'a> (action : 'a -> unit) (list : 'a List) =
        let mutable enr = list.GetEnumerator ()
        while enr.MoveNext () do
            action enr.Current

    /// Make a list with a single item.
    let inline singleton comparer item =
        List.toHashSet comparer [item]

[<AutoOpen>]
module GenericListOperators =

    /// List pattern matching.
    let (|EmptyGenericList|NonEmptyGenericList|) (list : 'a List) =
        if list.IsEmpty
        then EmptyGenericList
        else NonEmptyGenericList list

[<AutoOpen>]
module GenericListExtension =

    type 'a List with

        /// Check that a list is non-empty.
        member this.NotEmpty =
            GenericList.notEmpty this
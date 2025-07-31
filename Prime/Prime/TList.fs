// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module TList =

    type private 'a Log =
        | Add of add : 'a
        | Remove of remove : 'a
        | Set of index : int * value : 'a
        | Clear

    /// A list that supports transaction-based rewinding.
    type [<ReferenceEquality; DefaultValue "[]">] 'a TList =
        private
            { mutable TListOpt : 'a TList
              TConfig : TConfig
              ImpList : 'a List
              ImpListOrigin : 'a List
              Logs : 'a Log list
              LogsLength : int }

        static member (>>.) (list : 'a2 TList, builder : TExpr<unit, 'a2 TList>) =
            snd' (builder list)

        static member (.>>) (list : 'a2 TList, builder : TExpr<'a2, 'a2 TList>) =
            fst' (builder list)

        static member (.>>.) (list : 'a2 TList, builder : TExpr<'a2, 'a2 TList>) =
            builder list

    let private commit list =
        let oldList = list
        let impListOrigin = List<'a> list.ImpListOrigin
        List.foldBack (fun log () ->
            match log with
            | Add value -> impListOrigin.Add value
            | Remove value -> impListOrigin.Remove value |> ignore
            | Set (index, value) -> impListOrigin.[index] <- value
            | Clear -> impListOrigin.Clear ())
            list.Logs ()
        let impList = List<'a> impListOrigin
        let list = { list with ImpList = impList; ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
        oldList.TListOpt <- Unchecked.defaultof<'a TList>
        list.TListOpt <- list
        list

    let private compress list =
        let oldList = list
        let impListOrigin = List<'a> list.ImpList
        let list = { list with ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
        oldList.TListOpt <- Unchecked.defaultof<'a TList>
        list.TListOpt <- list
        list

    let private validate2 list =
        lock list.Logs (fun () ->
            match box list.TListOpt with
            | null -> commit list
            | target ->
                match obj.ReferenceEquals (target, list) with
                | true ->
                    if list.LogsLength > list.ImpList.Count
                    then compress list
                    else list
                | false -> commit list)

    let private update updater list =
        let oldList = list
        let list = validate2 list
        let list = updater list
        oldList.TListOpt <- Unchecked.defaultof<'a TList>
        list.TListOpt <- list
        list

    let private validate list =
        if TConfig.isFunctional list.TConfig
        then validate2 list
        else list

    /// Create a TList containing the given sequence of values.
    let makeFromSeq config (items : 'a seq) =
        if TConfig.isFunctional config then 
            let impList = List<'a> items
            let impListOrigin = List<'a> impList
            let list =
                { TListOpt = Unchecked.defaultof<'a TList>
                  TConfig = config
                  ImpList = impList
                  ImpListOrigin = impListOrigin
                  Logs = []
                  LogsLength = 0 }
            list.TListOpt <- list
            list
        else
            { TListOpt = Unchecked.defaultof<'a TList>
              TConfig = config
              ImpList = List<'a> items
              ImpListOrigin = List<'a> ()
              Logs = []
              LogsLength = 0 }

    /// Create a TList containing the given array of values.
    let makeFromArray config (items : 'a array) =
        makeFromSeq config items

    /// Create an empty TList.
    let makeEmpty<'a> config =
        makeFromSeq config (List<'a> ())

    /// Get the semantics configuration of a TList.
    let config list =
        struct (list.TConfig, list)

    /// Get the value of the given index.
    let get index list =
        let list = validate list
        struct (list.ImpList.[index], list)

    /// Set the value of the given index.
    let set index value list =
        if TConfig.isFunctional list.TConfig then 
            update (fun list ->
                let list = { list with Logs = Set (index, value) :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.[index] <- value
                list)
                list
        else list.ImpList.[index] <- value; list

    /// Add an element to a TList.
    let add value list =
        if TConfig.isFunctional list.TConfig then 
            update (fun list ->
                let list = { list with Logs = Add value :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.Add value |> ignore
                list)
                list
        else list.ImpList.Add value |> ignore; list

    /// Remove all matching elements from a TList.
    let remove value list =
        if TConfig.isFunctional list.TConfig then
            update (fun list ->
                let list = { list with Logs = Remove value :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.Remove value |> ignore
                list)
                list
        else list.ImpList.Remove value |> ignore; list

    /// Remove all elements from a TList.
    let clear list =
        if TConfig.isFunctional list.TConfig then
            update (fun list ->
                let list = { list with Logs = Clear :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.Clear ()
                list)
                list
        else list.ImpList.Clear (); list

    /// Check that a TList has no elements.
    let isEmpty list =
        let list = validate list
        struct (list.ImpList.Count = 0, list)

    /// Check that a TList has one or more elements.
    let notEmpty list =
        let list = validate list
        mapFst' not (isEmpty list)

    /// Get the length of a TList (constant-time).
    let length list =
        let list = validate list
        struct (list.ImpList.Count, list)

    /// Check that a value is contained in a TList.
    let contains value list =
        let list = validate list
        struct (list.ImpList.Contains value, list)

    /// Convert a TList to an array.
    let toArray list =
        let list = validate list
        struct (Array.ofSeq list.ImpList, list)

    /// Convert a TList to a seq. Note that the entire list is iterated eagerly when functional.
    let toSeq list =
        if TConfig.isFunctional list.TConfig then
            let list = validate2 list
            let struct (sarr, list) = struct (SArray.ofSeq list.ImpList, list)
            struct (sarr :> _ seq, list)
        else struct (list.ImpList, list)

    /// Convert a TList to an imperative System.Collections.Generic.List.
    let toImpList list =
        let list = validate list
        let result = List<'a> list.ImpList
        struct (result, list)

    /// Make a TList from a sequence of values.
    let ofSeq config values =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty config)
            values

    /// Make a TList from an array of values.
    let ofArray config (values : 'a array) =
        ofSeq config values

    /// Fold over the elements of a TList.
    let fold folder state list =
        let struct (seq, list) = toSeq list
        let folded = Seq.fold folder state seq
        struct (folded, list)

    /// Map the elements of a TList.
    let map (mapper : 'a -> 'b) (list : 'a TList) =
        let list = validate list
        let seqMapped = Seq.map mapper list.ImpList
        let listMapped = makeFromSeq list.TConfig seqMapped
        struct (listMapped, list)

    /// Filter the elements of a TList.
    let filter pred list =
        let list = validate list
        let seqFiltered = Seq.filter pred list.ImpList
        let listFiltered = makeFromSeq list.TConfig seqFiltered
        struct (listFiltered, list)

    /// Iterate over the elements of a TList with an action.
    let iter action list =
        let struct (seq, list) = toSeq list
        Seq.iter action seq
        list

    /// Reverse the elements of a TList.
    let rev list =
        let list = validate list
        let seqReversed = Seq.rev list.ImpList
        let listReversed = makeFromSeq list.TConfig seqReversed
        struct (listReversed, list)

    /// Sort the elements of a TList with the given comparer function.
    let sortWith comparer list =
        let list = validate list
        let seqSorted = Seq.sortWith comparer list.ImpList
        let listSorted = makeFromSeq list.TConfig seqSorted
        struct (listSorted, list)

    /// Sort the elements of a TList with the given by function.
    let sortBy by list =
        let list = validate list
        let seqSorted = Seq.sortBy by list.ImpList
        let listSorted = makeFromSeq list.TConfig seqSorted
        struct (listSorted, list)

    /// Sort the elements of a TList by their natural order.
    let sort list =
        let list = validate list
        let seqSorted = Seq.sort list.ImpList
        let listSorted = makeFromSeq list.TConfig seqSorted
        struct (listSorted, list)

    /// Convert option elements to definite elements.
    let definitize list =
        let listMapped = filter Option.isSome list |> fst'
        map Option.get listMapped

    /// Create a TList from a TList of TList of values.
    let makeFromLists config lists =
        // OPTIMIZATION: elides building of avoidable transactions.
        let listsAsSeq = toSeq lists |> fst'
        let tempList = List<'a> ()
        for list in listsAsSeq do tempList.AddRange (toSeq list |> fst')
        makeFromSeq config tempList

    /// Add all the given items to a TList.
    let addMany (items : 'a seq) list =
        let list = validate list
        let lists = add list (makeFromArray list.TConfig [|makeFromSeq list.TConfig items|])
        makeFromLists list.TConfig lists

    /// Remove all the given items from a TList.
    let removeMany items list =
        Seq.fold (flip remove) list items

    /// Make a TList with a single element.
    let singleton<'a> config (item : 'a) =
        makeFromSeq config [item]

/// A list that supports transaction-based rewinding.
type 'a TList = 'a TList.TList

[<AutoOpen>]
module TListBuilder =

    /// Builds a TList.
    let tlist<'a> = TExprBuilder<'a TList> ()
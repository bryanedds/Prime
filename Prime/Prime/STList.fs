// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module STList =

    type private 'a Log =
        | Add of add : 'a
        | Remove of remove : 'a
        | Set of index : int * value : 'a
        | Clear
        
    type [<ReferenceEquality>] 'a STList =
        private
            { mutable STListOpt : 'a STList
              TConfig : TConfig
              ImpList : 'a SList
              ImpListOrigin : 'a SList
              Logs : 'a Log list
              LogsLength : int }

        static member (>>.) (list : 'a2 STList, builder : TExpr<unit, 'a2 STList>) =
            snd' (builder list)

        static member (.>>) (list : 'a2 STList, builder : TExpr<'a2, 'a2 STList>) =
            fst' (builder list)

        static member (.>>.) (list : 'a2 STList, builder : TExpr<'a2, 'a2 STList>) =
            builder list

    let private commit list =
        let oldList = list
        let impListOrigin = SList.makeFromSegmentedList list.ImpListOrigin
        List.foldBack (fun log () ->
            match log with
            | Add value -> impListOrigin.Add value
            | Remove value -> impListOrigin.Remove value |> ignore
            | Set (index, value) -> impListOrigin.[index] <- value
            | Clear -> impListOrigin.Clear ())
            list.Logs ()
        let impList = SList.makeFromSegmentedList impListOrigin
        let list = { list with ImpList = impList; ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
        oldList.STListOpt <- Unchecked.defaultof<'a STList>
        list.STListOpt <- list
        list

    let private compress list =
        let oldList = list
        let impListOrigin = SList.makeFromSegmentedList list.ImpList
        let list = { list with ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
        oldList.STListOpt <- Unchecked.defaultof<'a STList>
        list.STListOpt <- list
        list

    let private validate2 list =
        lock list.Logs (fun () ->
            match box list.STListOpt with
            | null -> commit list
            | target ->
                match obj.ReferenceEquals (target, list) with
                | true ->
                    if list.LogsLength > list.ImpList.Length
                    then compress list
                    else list
                | false -> commit list)

    let private update updater list =
        let oldList = list
        let list = validate2 list
        let list = updater list
        oldList.STListOpt <- Unchecked.defaultof<'a STList>
        list.STListOpt <- list
        list

    let private validate list =
        if TConfig.isFunctional list.TConfig
        then validate2 list
        else list

    let makeFromSeq config (items : 'a seq) =
        if TConfig.isFunctional config then 
            let impList = SList.ofSeq items
            let impListOrigin = SList.makeFromSegmentedList impList
            let list =
                { STListOpt = Unchecked.defaultof<'a STList>
                  TConfig = config
                  ImpList = impList
                  ImpListOrigin = impListOrigin
                  Logs = []
                  LogsLength = 0 }
            list.STListOpt <- list
            list
        else
            { STListOpt = Unchecked.defaultof<'a STList>
              TConfig = config
              ImpList = SList.ofSeq items
              ImpListOrigin = SList.make ()
              Logs = []
              LogsLength = 0 }

    let makeFromArray config (items : 'a array) =
        makeFromSeq config items

    let makeEmpty<'a> config =
        makeFromSeq config (List<'a> ())

    let getConfig list =
        struct (list.TConfig, list)

    let get index list =
        let list = validate list
        struct (list.ImpList.[index], list)

    let set index value list =
        if TConfig.isFunctional list.TConfig then 
            update (fun list ->
                let list = { list with Logs = Set (index, value) :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.[index] <- value
                list)
                list
        else list.ImpList.[index] <- value; list

    let add value list =
        if TConfig.isFunctional list.TConfig then 
            update (fun list ->
                let list = { list with Logs = Add value :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.Add value |> ignore
                list)
                list
        else list.ImpList.Add value |> ignore; list

    let remove value list =
        if TConfig.isFunctional list.TConfig then
            update (fun list ->
                let list = { list with Logs = Remove value :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.Remove value |> ignore
                list)
                list
        else list.ImpList.Remove value |> ignore; list

    let clear list =
        if TConfig.isFunctional list.TConfig then
            update (fun list ->
                let list = { list with Logs = Clear :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.Clear ()
                list)
                list
        else list.ImpList.Clear (); list

    let isEmpty list =
        let list = validate list
        struct (list.ImpList.Length = 0, list)

    let notEmpty list =
        let list = validate list
        mapFst' not (isEmpty list)

    /// Get the length of the list (constant-time).
    let length list =
        let list = validate list
        struct (list.ImpList.Length, list)

    /// Check that a value is contain in the list.
    let contains value list =
        let list = validate list
        struct (list.ImpList.Contains value, list)

    /// Convert a STList to an array. Note that entire list is iterated eagerly since the underlying .NET List could
    /// otherwise opaquely change during iteration.
    let toArray list =
        let list = validate list
        struct (Array.ofSeq list.ImpList, list)

    /// Convert a STList to a seq. Note that entire list is iterated eagerly since the underlying .NET List could
    /// otherwise opaquely change during iteration.
    let toSeq list =
        let struct (arr, list) = toArray list
        struct (Seq.ofArray arr, list)

    /// Convert a STList to an imperative System.Collections.Generic.List.
    let toImpList list =
        let list = validate list
        let result = List<'a> list.ImpList
        struct (result, list)

    let map (mapper : 'a -> 'b) (list : 'a STList) =
        let list = validate list
        let seqMapped = Seq.map mapper list.ImpList
        let listMapped = makeFromSeq list.TConfig seqMapped
        struct (listMapped, list)

    let filter pred list =
        let list = validate list
        let seqFiltered = Seq.filter pred list.ImpList
        let listFiltered = makeFromSeq list.TConfig seqFiltered
        struct (listFiltered, list)

    let rev list =
        let list = validate list
        let seqReversed = Seq.rev list.ImpList
        let listReversed = makeFromSeq list.TConfig seqReversed
        struct (listReversed, list)

    let sortWith comparison list =
        let list = validate list
        let seqSorted = Seq.sortWith comparison list.ImpList
        let listSorted = makeFromSeq list.TConfig seqSorted
        struct (listSorted, list)

    let sortBy by list =
        let list = validate list
        let seqSorted = Seq.sortBy by list.ImpList
        let listSorted = makeFromSeq list.TConfig seqSorted
        struct (listSorted, list)

    let sort list =
        let list = validate list
        let seqSorted = Seq.sort list.ImpList
        let listSorted = makeFromSeq list.TConfig seqSorted
        struct (listSorted, list)

    let fold folder state list =
        let struct (seq, list) = toSeq list
        let folded = Seq.fold folder state seq
        struct (folded, list)

    let definitize list =
        let listMapped = filter Option.isSome list |> fst'
        map Option.get listMapped

    let makeFromLists config lists =
        // OPTIMIZATION: elides building of avoidable transactions.
        let listsAsSeq = toSeq lists |> fst'
        let tempList = List<'a> ()
        for list in listsAsSeq do tempList.AddRange (toSeq list |> fst')
        makeFromSeq config tempList

    /// Add all the given items to the list.
    let addMany (items : 'a seq) list =
        let list = validate list
        let lists = add list (makeFromArray list.TConfig [|makeFromSeq list.TConfig items|])
        makeFromLists list.TConfig lists

    /// Remove all the given items from the list.
    let removeMany items list =
        Seq.fold (flip remove) list items

    /// Make a STList with a single item.
    let singleton<'a> config (item : 'a) =
        makeFromSeq config [item]

type 'a STList = 'a STList.STList

[<AutoOpen>]
module STListBuilder =

    let tlist<'a> = TExprBuilder<'a STList> ()
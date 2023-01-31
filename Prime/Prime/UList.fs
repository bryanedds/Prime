// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module UList =

    type [<ReferenceEquality>] 'a UList =
        private
            { mutable List : 'a TList }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () =
                let struct (seq, tlist) = TList.toSeq this.List
                this.List <- tlist
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a IEnumerable).GetEnumerator () :> IEnumerator

        member this.Item index =
            let struct (result, tlist) = TList.get index this.List
            this.List <- tlist
            result

    let makeFromSeq config items =
        { List = TList.makeFromSeq config items }

    let makeFromArray config items =
        { List = TList.makeFromArray config items }

    let makeEmpty<'a> config =
        { List = TList.makeEmpty<'a> config }

    let getConfig list =
        let struct (result, tlist) = TList.getConfig list.List
        list.List <- tlist
        result

    let get (index : int) (list : 'a UList) =
        list.[index]

    let set index value list =
        { List = TList.set index value list.List }

    let add value list =
        { List = TList.add value list.List }

    let remove value list =
        { List = TList.remove value list.List }

    let clear list =
        { List = TList.clear list.List }

    let isEmpty list =
        let struct (result, tlist) = TList.isEmpty list.List
        list.List <- tlist
        result

    let notEmpty list =
        not (isEmpty list)

    let length list =
        let struct (result, tlist) = TList.length list.List
        list.List <- tlist
        result

    let contains value list =
        let struct (result, tlist) = TList.contains value list.List
        list.List <- tlist
        result

    let toSeq (list : _ UList) =
        list :> _ seq

    let toArray (list : _ UList) =
        let struct (arr, tlist) = TList.toArray list.List
        list.List <- tlist
        arr

    let toImpList (list : _ UList) =
        let struct (arr, tlist) = TList.toImpList list.List
        list.List <- tlist
        arr

    let ofSeq values config =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty config)
            values

    let ofArray (values : 'a array) config =
        ofSeq values config

    let map mapper list =
        let struct (result, tlist) = TList.map mapper list.List
        list.List <- tlist
        { List = result }

    let filter pred list =
        let struct (result, tlist) = TList.filter pred list.List
        list.List <- tlist
        { List = result }

    let rev list =
        let struct (result, tlist) = TList.rev list.List
        list.List <- tlist
        { List = result }

    let sortWith comparison list =
        let struct (result, tlist) = TList.sortWith comparison list.List
        list.List <- tlist
        { List = result }

    let sortBy by list =
        let struct (result, tlist) = TList.sortBy by list.List
        list.List <- tlist
        { List = result }

    let sort list =
        let struct (result, tlist) = TList.sort list.List
        list.List <- tlist
        { List = result }

    let fold folder state list =
        let struct (result, tlist) = TList.fold folder state list.List
        list.List <- tlist
        result

    let definitize list =
        let struct (result, tlist) = TList.definitize list.List
        list.List <- tlist
        { List = result }

    let makeFromLists config lists =
        let tlists = (map (fun (list : 'a UList) -> list.List) lists).List
        let tlist = TList.makeFromLists config tlists
        { List = tlist }

    /// Add all the given items to the list.
    let addMany items list =
        { List = TList.addMany items list.List }

    /// Remove all the given items from the list.
    let removeMany items list =
        { List = TList.removeMany items list.List }

    /// Make a UList with a single item.
    let singleton<'a> config item =
        { List = TList.singleton<'a> config item }

type 'a UList = 'a UList.UList
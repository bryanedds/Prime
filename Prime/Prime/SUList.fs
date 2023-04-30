// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SUList =

    type [<ReferenceEquality>] 'a SUList =
        private
            { mutable List : 'a STList }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () =
                let struct (seq, tlist) = STList.toSeq this.List
                this.List <- tlist
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a IEnumerable).GetEnumerator () :> IEnumerator

        member this.Item index =
            let struct (result, tlist) = STList.get index this.List
            this.List <- tlist
            result

    let makeFromSeq config items =
        { List = STList.makeFromSeq config items }

    let makeFromArray config items =
        { List = STList.makeFromArray config items }

    let makeEmpty<'a> config =
        { List = STList.makeEmpty<'a> config }

    let getConfig list =
        let struct (result, tlist) = STList.getConfig list.List
        list.List <- tlist
        result

    let get (index : int) (list : 'a SUList) =
        list.[index]

    let set index value list =
        { List = STList.set index value list.List }

    let add value list =
        { List = STList.add value list.List }

    let remove value list =
        { List = STList.remove value list.List }

    let clear list =
        { List = STList.clear list.List }

    let isEmpty list =
        let struct (result, tlist) = STList.isEmpty list.List
        list.List <- tlist
        result

    let notEmpty list =
        not (isEmpty list)

    let length list =
        let struct (result, tlist) = STList.length list.List
        list.List <- tlist
        result

    let contains value list =
        let struct (result, tlist) = STList.contains value list.List
        list.List <- tlist
        result

    let toSeq (list : _ SUList) =
        list :> _ seq

    let toArray (list : _ SUList) =
        let struct (arr, tlist) = STList.toArray list.List
        list.List <- tlist
        arr

    let toImpList (list : _ SUList) =
        let struct (arr, tlist) = STList.toImpList list.List
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
        let struct (result, tlist) = STList.map mapper list.List
        list.List <- tlist
        { List = result }

    let filter pred list =
        let struct (result, tlist) = STList.filter pred list.List
        list.List <- tlist
        { List = result }

    let rev list =
        let struct (result, tlist) = STList.rev list.List
        list.List <- tlist
        { List = result }

    let sortWith comparison list =
        let struct (result, tlist) = STList.sortWith comparison list.List
        list.List <- tlist
        { List = result }

    let sortBy by list =
        let struct (result, tlist) = STList.sortBy by list.List
        list.List <- tlist
        { List = result }

    let sort list =
        let struct (result, tlist) = STList.sort list.List
        list.List <- tlist
        { List = result }

    let fold folder state list =
        let struct (result, tlist) = STList.fold folder state list.List
        list.List <- tlist
        result

    let definitize list =
        let struct (result, tlist) = STList.definitize list.List
        list.List <- tlist
        { List = result }

    let makeFromLists config lists =
        let tlists = (map (fun (list : 'a SUList) -> list.List) lists).List
        let tlist = STList.makeFromLists config tlists
        { List = tlist }

    /// Add all the given items to the list.
    let addMany items list =
        { List = STList.addMany items list.List }

    /// Remove all the given items from the list.
    let removeMany items list =
        { List = STList.removeMany items list.List }

    /// Make a SUList with a single item.
    let singleton<'a> config item =
        { List = STList.singleton<'a> config item }

type 'a SUList = 'a SUList.SUList
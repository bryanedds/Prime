// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SUList =

    /// A list that supports transaction-based rewinding with a more convenient interface than STList.
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

    /// Create a SUList containing the given sequence of values.
    let makeFromSeq config items =
        { List = STList.makeFromSeq config items }

    /// Create a SUList containing the given array of values.
    let makeFromArray config items =
        { List = STList.makeFromArray config items }

    /// Create an empty SUList.
    let makeEmpty<'a> config =
        { List = STList.makeEmpty<'a> config }

    /// Get the semantics configuration of a SUList.
    let getConfig list =
        let struct (result, tlist) = STList.getConfig list.List
        list.List <- tlist
        result

    /// Get the value of the given index.
    let get (index : int) (list : 'a SUList) =
        list.[index]

    /// Set the value of the given index.
    let set index value list =
        { List = STList.set index value list.List }

    /// Add an element to a SUList.
    let add value list =
        { List = STList.add value list.List }

    /// Remove all matching elements from a SUList.
    let remove value list =
        { List = STList.remove value list.List }

    /// Remove all elements from a SUList.
    let clear list =
        { List = STList.clear list.List }

    /// Check that a SUList has no elements.
    let isEmpty list =
        let struct (result, tlist) = STList.isEmpty list.List
        list.List <- tlist
        result

    /// Check that a SUList has one or more elements.
    let notEmpty list =
        not (isEmpty list)

    /// Get the length of a SUList (constant-time).
    let length list =
        let struct (result, tlist) = STList.length list.List
        list.List <- tlist
        result

    /// Check that a value is contained in a SUList.
    let contains value list =
        let struct (result, tlist) = STList.contains value list.List
        list.List <- tlist
        result
        
    /// Convert a SUList to an array. Note that entire list is iterated eagerly since the underlying .NET List could
    /// otherwise opaquely change during iteration.
    let toArray (list : _ SUList) =
        let struct (arr, tlist) = STList.toArray list.List
        list.List <- tlist
        arr
        
    /// Convert a SUList to a seq. Note that entire list is iterated eagerly since the underlying .NET List could
    /// otherwise opaquely change during iteration.
    let toSeq (list : _ SUList) =
        list :> _ seq

    /// Convert a SUList to an imperative System.Collections.Generic.List.
    let toImpList (list : _ SUList) =
        let struct (arr, tlist) = STList.toImpList list.List
        list.List <- tlist
        arr

    /// Make a SUList from a sequence of values.
    let ofSeq config values =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty config)
            values

    /// Make a SUList from an array of values.
    let ofArray config (values : 'a array) =
        ofSeq config values

    /// Map the elements of a SUList.
    let map mapper list =
        let struct (result, tlist) = STList.map mapper list.List
        list.List <- tlist
        { List = result }

    /// Filter the elements of a SUList.
    let filter pred list =
        let struct (result, tlist) = STList.filter pred list.List
        list.List <- tlist
        { List = result }

    /// Reverse the elements of a SUList.
    let rev list =
        let struct (result, tlist) = STList.rev list.List
        list.List <- tlist
        { List = result }

    /// Sort the elements of a SUList with the given comparer function.
    let sortWith comparison list =
        let struct (result, tlist) = STList.sortWith comparison list.List
        list.List <- tlist
        { List = result }

    /// Sort the elements of a SUList with the given by function.
    let sortBy by list =
        let struct (result, tlist) = STList.sortBy by list.List
        list.List <- tlist
        { List = result }

    /// Sort the elements of a SUList by their natural order.
    let sort list =
        let struct (result, tlist) = STList.sort list.List
        list.List <- tlist
        { List = result }

    /// Fold over the elements of a SUList.
    let fold folder state list =
        let struct (result, tlist) = STList.fold folder state list.List
        list.List <- tlist
        result

    /// Convert option elements to definite elements.
    let definitize list =
        let struct (result, tlist) = STList.definitize list.List
        list.List <- tlist
        { List = result }

    /// Create a SUList from a SUList of SUList of values.
    let makeFromLists config lists =
        let tlists = (map (fun (list : 'a SUList) -> list.List) lists).List
        let tlist = STList.makeFromLists config tlists
        { List = tlist }

    /// Add all the given items to a SUList.
    let addMany items list =
        { List = STList.addMany items list.List }

    /// Remove all the given items from a SUList.
    let removeMany items list =
        { List = STList.removeMany items list.List }

    /// Make a SUList with a single element.
    let singleton<'a> config item =
        { List = STList.singleton<'a> config item }

/// A list that supports transaction-based rewinding with a more convenient interface than STList.
type 'a SUList = 'a SUList.SUList
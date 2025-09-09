// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module UList =

    /// A list that supports transaction-based rewinding with a more convenient interface than TList.
    type [<ReferenceEquality; DefaultValue "[]">] 'a UList =
        private
            { mutable List : 'a TList }

        /// Get the length of a UList (constant-time).
        member this.Length =
            let struct (result, tlist) = TList.length this.List
            this.List <- tlist
            result

        /// Get the value of the given index.
        member this.Item index =
            let struct (result, tlist) = TList.get index this.List
            this.List <- tlist
            result
    
        interface 'a IEnumerable with
            member this.GetEnumerator () =
                let struct (seq, tlist) = TList.toSeq this.List
                this.List <- tlist
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a IEnumerable).GetEnumerator () :> IEnumerator

    /// Create a UList containing the given sequence of values.
    let makeFromSeq config items =
        { List = TList.makeFromSeq config items }

    /// Create a UList containing the given array of values.
    let makeFromArray config items =
        { List = TList.makeFromArray config items }

    /// Create an empty UList.
    let makeEmpty<'a> config =
        { List = TList.makeEmpty<'a> config }

    /// Get the semantics configuration of a UList.
    let config list =
        let struct (result, tlist) = TList.config list.List
        list.List <- tlist
        result

    /// Get the value of the given index.
    let get (index : int) (list : 'a UList) =
        list.[index]

    /// Set the value of the given index.
    let set index value list =
        { List = TList.set index value list.List }

    /// Add an element to a UList.
    let add value list =
        { List = TList.add value list.List }

    /// Remove all matching elements from a UList.
    let remove value list =
        { List = TList.remove value list.List }

    /// Remove all elements from a UList.
    let clear list =
        { List = TList.clear list.List }

    /// Check that a UList has no elements.
    let isEmpty list =
        let struct (result, tlist) = TList.isEmpty list.List
        list.List <- tlist
        result

    /// Check that a UList has one or more elements.
    let notEmpty list =
        not (isEmpty list)

    /// Get the length of a UList (constant-time).
    let length list =
        let struct (result, tlist) = TList.length list.List
        list.List <- tlist
        result

    /// Check that a value is contained in a UList.
    let contains value list =
        let struct (result, tlist) = TList.contains value list.List
        list.List <- tlist
        result
        
    /// Convert a UList to an array.
    let toArray (list : _ UList) =
        let struct (arr, tlist) = TList.toArray list.List
        list.List <- tlist
        arr
        
    /// Convert a UList to a seq. Note that the entire list is iterated eagerly when functional.
    let toSeq (list : _ UList) =
        list :> _ seq

    /// Convert a UList to an imperative System.Collections.Generic.List.
    let toImpList (list : _ UList) =
        let struct (arr, tlist) = TList.toImpList list.List
        list.List <- tlist
        arr

    /// Make a UList from a sequence of values.
    let ofSeq config values =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty config)
            values

    /// Convert a sequence of values to a UList assuming functional representation.
    let ofSeq1 pairs =
        ofSeq Functional pairs

    /// Fold over the elements of a UList.
    let fold folder state list =
        let struct (result, tlist) = TList.fold folder state list.List
        list.List <- tlist
        result

    /// Map the elements of a UList.
    let map mapper list =
        let struct (result, tlist) = TList.map mapper list.List
        list.List <- tlist
        { List = result }

    /// Filter the elements of a UList.
    let filter pred list =
        let struct (result, tlist) = TList.filter pred list.List
        list.List <- tlist
        { List = result }

    /// Iterate over the elements of a UList with an action.
    let iter action list =
        let tlist = TList.iter action list.List
        list.List <- tlist

    /// Reverse the elements of a UList.
    let rev list =
        let struct (result, tlist) = TList.rev list.List
        list.List <- tlist
        { List = result }

    /// Sort the elements of a UList with the given comparer function.
    let sortWith comparison list =
        let struct (result, tlist) = TList.sortWith comparison list.List
        list.List <- tlist
        { List = result }

    /// Sort the elements of a UList with the given by function.
    let sortBy by list =
        let struct (result, tlist) = TList.sortBy by list.List
        list.List <- tlist
        { List = result }

    /// Sort the elements of a UList by their natural order.
    let sort list =
        let struct (result, tlist) = TList.sort list.List
        list.List <- tlist
        { List = result }

    /// Convert option elements to definite elements.
    let definitize list =
        let struct (result, tlist) = TList.definitize list.List
        list.List <- tlist
        { List = result }

    /// Create a UList from a UList of UList of values.
    let makeFromLists config lists =
        let tlists = (map (fun (list : 'a UList) -> list.List) lists).List
        let tlist = TList.makeFromLists config tlists
        { List = tlist }

    /// Add all the given items to a UList.
    let addMany items list =
        { List = TList.addMany items list.List }

    /// Remove all the given items from a UList.
    let removeMany items list =
        { List = TList.removeMany items list.List }

    /// Make a UList with a single element.
    let singleton<'a> config item =
        { List = TList.singleton<'a> config item }

/// A list that supports transaction-based rewinding with a more convenient interface than TList.
type 'a UList = 'a UList.UList
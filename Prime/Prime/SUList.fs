// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SUList =

    /// A list that supports transaction-based rewinding with a more convenient interface than STList.
    type [<ReferenceEquality; DefaultValue "[]">] 'a SUList =
        private
            { mutable List : 'a STList }

        /// Get the length of a UList (constant-time).
        member this.Length =
            let struct (result, tlist) = STList.length this.List
            this.List <- tlist
            result

        /// Get the value of the given index.
        member this.Item index =
            let struct (result, tlist) = STList.get index this.List
            this.List <- tlist
            result
    
        interface 'a IEnumerable with
            member this.GetEnumerator () =
                let struct (seq, tlist) = STList.toSeq this.List
                this.List <- tlist
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a IEnumerable).GetEnumerator () :> IEnumerator

    /// Create a SUList containing the given sequence of values.
    let makeFromSeq config items =
        { List = STList.makeFromSeq config items }

    /// Create a SUList containing the given array of values.
    let makeFromArray config items =
        { List = STList.makeFromArray config items }

    /// Create an empty SUList.
    let makeEmpty<'a> config =
        { List = STList.makeEmpty<'a> config }

    /// Get the semantic configuration of a SUList.
    let config list =
        STList.config list.List

    /// Get the value of the given index.
    let get (index : int) (list : 'a SUList) =
        list.[index]

    /// Set the value of the given index.
    let set index value list =
        match STList.config list.List with
        | Functional -> { List = STList.set index value list.List }
        | Imperative -> STList.set index value list.List |> ignore; list

    /// Add an element to a SUList.
    let add value list =
        match STList.config list.List with
        | Functional -> { List = STList.add value list.List }
        | Imperative -> STList.add value list.List |> ignore; list

    /// Remove all matching elements from a SUList.
    let remove value list =
        match STList.config list.List with
        | Functional -> { List = STList.remove value list.List }
        | Imperative -> STList.remove value list.List |> ignore; list

    /// Remove all elements from a SUList.
    let clear list =
        match STList.config list.List with
        | Functional -> { List = STList.clear list.List }
        | Imperative -> STList.clear list.List |> ignore; list

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
        
    /// Convert a SUList to an array.
    let toArray (list : _ SUList) =
        let struct (arr, tlist) = STList.toArray list.List
        list.List <- tlist
        arr
        
    /// Convert a SUList to a seq. Note that the entire list is iterated eagerly when functional.
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

    /// Fold over the elements of a SUList.
    let fold folder state list =
        let struct (result, tlist) = STList.fold folder state list.List
        list.List <- tlist
        result

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

    /// Iterate over the elements of a SUList with an action.
    let iter action list =
        let tlist = STList.iter action list.List
        list.List <- tlist

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
        match STList.config list.List with
        | Functional -> { List = STList.addMany items list.List }
        | Imperative -> STList.addMany items list.List |> ignore; list

    /// Remove all the given items from a SUList.
    let removeMany items list =
        match STList.config list.List with
        | Functional -> { List = STList.removeMany items list.List }
        | Imperative -> STList.removeMany items list.List |> ignore; list

    /// Make a SUList with a single element.
    let singleton<'a> config item =
        { List = STList.singleton<'a> config item }

/// A list that supports transaction-based rewinding with a more convenient interface than STList.
type 'a SUList = 'a SUList.SUList
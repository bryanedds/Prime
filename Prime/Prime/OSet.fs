// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

/// An enumerator for OSet.
type 'a OSetEnumerator (enr : FStackEnumerator<struct (bool * 'a)>) =
    member this.MoveNext () =
        let result = enr.MoveNext ()
        if result then
            let struct (active, _) = enr.Current
            if not active then this.MoveNext ()
            else true
        else false
    interface 'a IEnumerator with
        member this.MoveNext () = this.MoveNext ()
        member this.Current = let struct (_, item) = enr.Current in item
        member this.Current = let struct (_, item) = enr.Current in item :> obj
        member this.Reset () = enr.Reset ()
        member this.Dispose () = enr.Dispose ()

[<RequireQualifiedAccess>]
module OSet =

    /// An ordered persistent set based on UMap and FStack.
    /// TODO: see if it would make sense to build UOrderedSet based on the more efficently-traversible
    /// OrderedDictionary.
    type [<ReferenceEquality>] OSet<'a> =
        private
            { Indices : UMap<'a, int>
              Entries : struct (bool * 'a) FStack
              InactiveCount : int }

        /// Get the enumerator.
        member this.GetEnumerator () =
            new OSetEnumerator<'a> (this.Entries.GetEnumerator ())

        interface 'a IEnumerable with
            member this.GetEnumerator () =
                this.GetEnumerator ()

        interface IEnumerable with
            member this.GetEnumerator () =
                this.GetEnumerator ()

    let private compact set =
        let entries = FStack.filter (fun (struct (active, _)) -> active) set.Entries
        let indices = Seq.foldi (fun i u (struct (_, item)) -> UMap.add item i u) (UMap.makeEmpty (UMap.getComparer set.Indices) (UMap.getConfig set.Indices)) entries
        { Indices = indices
          Entries = entries
          InactiveCount = 0 }

    /// Create an empty OSet.
    let makeEmpty<'a> comparer config =
        { Indices = UMap.makeEmpty<'a, int> comparer config
          Entries = (FStack.empty : struct (bool * 'a) FStack)
          InactiveCount = 0 }

    /// Check that an OSet is empty.
    let isEmpty set =
        UMap.isEmpty set.Indices

    /// Check that an OSet is empty.
    let notEmpty set =
        UMap.notEmpty set.Indices

    /// Get the set key comparer.
    let getComparer set =
        UMap.getComparer set.Indices

    /// Get the set configuration.
    let getConfig set =
        UMap.getConfig set.Indices

    /// Add a value with the key to an OSet.
    let add (item : 'a) set =
        match UMap.tryFind item set.Indices with
        | Some index ->
            { set with
                Indices = UMap.add item index set.Indices
                Entries = FStack.replaceAt index struct (true, item) set.Entries }
        | None ->
            { set with
                Indices = UMap.add item (FStack.length set.Entries) set.Indices
                Entries = FStack.conj struct (true, item) set.Entries }

    /// Add all the given entries to an OSet.
    let addMany items set =
        Seq.fold (fun set item -> add item set) set items

    /// Remove a value with the given key from an OSet.
    let remove item set =
        match UMap.tryFind item set.Indices with
        | Some index ->
            let struct (_, _) = set.Entries.[index]
            let set =
                { Indices = UMap.remove item set.Indices
                  Entries = FStack.replaceAt index struct (false, Unchecked.defaultof<_>) set.Entries
                  InactiveCount = inc set.InactiveCount }
            if set.InactiveCount > FStack.length set.Entries / 2
            then compact set
            else set
        | None -> set

    /// Remove all values with the given keys from an OSet.
    let removeMany items set =
        Seq.fold (fun set (key : 'k) -> remove key set) set items

    /// Remove a value with the given key from an OSet.
    let removeBy (by : 'a -> bool) (set : OSet<'a>) =
        FStack.fold
            (fun set struct (active, item) -> if active && by item then remove item set else set)
            set set.Entries

    /// Check that an OSet contains a value with the given key.
    let contains item set =
        UMap.containsKey item set.Indices
        
    /// Combine the contents of two OSets, taking an item from the second set in the case of a key conflict.
    let concat set set2 =
        Seq.fold (flip add) set set2

    /// Fold over an OSet.
    let fold folder state (set : 'a OSet) =
        Seq.fold (fun s struct (active, item) -> if active then folder s item else s) state set.Entries

    /// Set over an OSet.
    let map mapper set =
        fold
            (fun state item -> add (mapper item) state)
            (makeEmpty (UMap.getComparer set.Indices) (UMap.getConfig set.Indices))
            set

    /// Filter an OSet.
    let filter pred set =
        fold
            (fun state item -> if pred item then add item state else state)
            (makeEmpty (UMap.getComparer set.Indices) (UMap.getConfig set.Indices))
            set

    /// Convert an OSet to a sequence of svalues.
    let toSeq (set : 'a OSet) =
        set :> _ IEnumerable

    /// Convert a sequence of values to an OSet.
    let ofSeq comparer config pairs =
        Seq.fold
            (fun set item -> add item set)
            (makeEmpty comparer config)
            pairs

    /// Convert a sequence of values to an OSet assuming structural comparison and functional representation.
    let ofSeq1 pairs =
        ofSeq HashIdentity.Structural Functional pairs

    /// Make an OSet with a single entry.
    let singleton<'a> comparer config item =
        let empty = makeEmpty<'a> comparer config
        add item empty

/// An ordered persistent set based on UMap and FStack.
/// NOTE: not supported by SymbolicConverter.
type 'a OSet = 'a OSet.OSet
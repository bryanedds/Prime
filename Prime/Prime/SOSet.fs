// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

/// An enumerator for SOSet.
type 'a SOSetEnumerator (enr : FStackEnumerator<struct (bool * 'a)>) =
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
module SOSet =

    /// An ordered persistent set based on SUMap and FStack.
    /// NOTE: not supported by SymbolicConverter.
    /// TODO: see if it would make sense to build UOrderedSet based on the more efficently-traversible
    /// OrderedDictionary.
    type [<ReferenceEquality; DefaultValue "[]">] SOSet<'a> =
        private
            { Indices : SUMap<'a, int>
              Entries : struct (bool * 'a) FStack
              InactiveCount : int }

        /// Get the enumerator.
        member this.GetEnumerator () =
            new SOSetEnumerator<'a> (this.Entries.GetEnumerator ())

        interface 'a IEnumerable with
            member this.GetEnumerator () =
                this.GetEnumerator ()

        interface IEnumerable with
            member this.GetEnumerator () =
                this.GetEnumerator ()

    let private compact set =
        let entries = FStack.filter (fun (struct (active, _)) -> active) set.Entries
        let indices = Seq.foldi (fun i u (struct (_, item)) -> SUMap.add item i u) (SUMap.makeEmpty (SUMap.getComparer set.Indices) (SUMap.getConfig set.Indices)) entries
        { Indices = indices
          Entries = entries
          InactiveCount = 0 }

    /// Create an empty SOSet.
    let makeEmpty<'a> comparer config =
        { Indices = SUMap.makeEmpty<'a, int> comparer config
          Entries = (FStack.empty : struct (bool * 'a) FStack)
          InactiveCount = 0 }

    /// Check that an SOSet is empty.
    let isEmpty set =
        SUMap.isEmpty set.Indices

    /// Check that an SOSet is empty.
    let notEmpty set =
        SUMap.notEmpty set.Indices

    /// Get the set key comparer.
    let getComparer set =
        SUMap.getComparer set.Indices

    /// Get the set configuration.
    let getConfig set =
        SUMap.getConfig set.Indices

    /// Add a value with the key to an SOSet.
    /// Linear time complexity when updating existing entry.
    let add (item : 'a) set =
        match SUMap.tryFind item set.Indices with
        | Some index ->
            { set with
                Indices = SUMap.add item index set.Indices
                Entries = FStack.replaceAt index struct (true, item) set.Entries }
        | None ->
            { set with
                Indices = SUMap.add item (FStack.length set.Entries) set.Indices
                Entries = FStack.conj struct (true, item) set.Entries }

    /// Add all the given entries to an SOSet.
    let addMany items set =
        Seq.fold (fun set item -> add item set) set items

    /// Remove a value with the given key from an SOSet.
    /// Linear time complexity.
    let remove item set =
        match SUMap.tryFind item set.Indices with
        | Some index ->
            let struct (_, _) = set.Entries.[index]
            let set =
                { Indices = SUMap.remove item set.Indices
                  Entries = FStack.replaceAt index struct (false, Unchecked.defaultof<_>) set.Entries
                  InactiveCount = inc set.InactiveCount }
            if set.InactiveCount > FStack.length set.Entries / 2
            then compact set
            else set
        | None -> set

    /// Remove all values with the given keys from an SOSet.
    let removeMany items set =
        Seq.fold (fun set (key : 'k) -> remove key set) set items

    /// Remove a value with the given key from an SOSet.
    let removeBy (by : 'a -> bool) (set : SOSet<'a>) =
        FStack.fold
            (fun set struct (active, item) -> if active && by item then remove item set else set)
            set set.Entries

    /// Check that an SOSet contains a value with the given key.
    let contains item set =
        SUMap.containsKey item set.Indices
        
    /// Combine the contents of two SOSets, taking an item from the second set in the case of a key conflict.
    let concat set set2 =
        Seq.fold (flip add) set set2

    /// Fold over an SOSet.
    let fold folder state (set : 'a SOSet) =
        Seq.fold (fun s struct (active, item) -> if active then folder s item else s) state set.Entries

    /// Set over an SOSet.
    let map mapper set =
        fold
            (fun state item -> add (mapper item) state)
            (makeEmpty (SUMap.getComparer set.Indices) (SUMap.getConfig set.Indices))
            set

    /// Filter an SOSet.
    let filter pred set =
        fold
            (fun state item -> if pred item then add item state else state)
            (makeEmpty (SUMap.getComparer set.Indices) (SUMap.getConfig set.Indices))
            set

    /// Convert an SOSet to a sequence of pairs of keys and values.
    let toSeq (set : 'a SOSet) =
        set :> _ IEnumerable

    /// Convert a sequence of keys and values to an SOSet.
    let ofSeq comparer config pairs =
        Seq.fold
            (fun set item -> add item set)
            (makeEmpty comparer config)
            pairs

    /// Make an SOSet with a single entry.
    let singleton<'a> comparer config item =
        let empty = makeEmpty<'a> comparer config
        add item empty

/// An ordered persistent set based on SUMap and FStack.
/// NOTE: not supported by SymbolicConverter.
type 'a SOSet = 'a SOSet.SOSet
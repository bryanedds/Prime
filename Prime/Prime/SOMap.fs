// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

/// An enumerator for SOMap.
type SOMapEnumerator<'k, 'v> (enr : FStackEnumerator<struct (bool * 'k * 'v)>) =
    member this.MoveNext () =
        let result = enr.MoveNext ()
        if result then
            let struct (active, _, _) = enr.Current
            if not active then this.MoveNext ()
            else true
        else false
    interface IEnumerator<'k * 'v> with
        member this.MoveNext () = this.MoveNext ()
        member this.Current = let struct (_, k, v) = enr.Current in (k, v)
        member this.Current = let struct (_, k, v) = enr.Current in (k, v) :> obj
        member this.Reset () = enr.Reset ()
        member this.Dispose () = enr.Dispose ()

[<RequireQualifiedAccess>]
module SOMap =

    /// An ordered persistent map based on SUMap and FStack.
    /// NOTE: not supported by SymbolicConverter.
    /// TODO: see if it would make sense to build UOrderedMap based on the more efficently-traversible
    /// OrderedDictionary.
    type [<ReferenceEquality; DefaultValue "[]">] SOMap<'k, 'v> =
        private
            { Indices : SUMap<'k, int>
              Entries : struct (bool * 'k * 'v) FStack
              InactiveCount : int }

        /// Get the enumerator.
        member this.GetEnumerator () =
            new SOMapEnumerator<'k, 'v> (this.Entries.GetEnumerator ())

        /// Try to find a value with the given key in an SOMap without allocating.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
        member this.TryGetValue (key, valueRef : 'v outref) =
            let mutable indexRef = 0
            match SUMap.tryGetValue (key, this.Indices, &indexRef) with
            | true ->
                let struct (_, _, v) = this.Entries.[indexRef]
                valueRef <- v
                true
            | false -> false

        /// The index operator.
        member this.Item
            with get (key : 'k) =
                let index = this.Indices.[key]
                this.Entries.[index]

        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                this.GetEnumerator ()

        interface IEnumerable with
            member this.GetEnumerator () =
                this.GetEnumerator ()

    let private compact map =
        let entries = FStack.filter (fun (struct (a, _, _)) -> a) map.Entries
        let indices = Seq.foldi (fun i u (struct (_, k, _)) -> SUMap.add k i u) (SUMap.makeEmpty (SUMap.getComparer map.Indices) (SUMap.getConfig map.Indices)) entries
        { Indices = indices
          Entries = entries
          InactiveCount = 0 }

    /// Create an empty SOMap.
    let makeEmpty<'k, 'v> comparer config =
        { Indices = SUMap.makeEmpty<'k, int> comparer config
          Entries = (FStack.empty : struct (bool * 'k * 'v) FStack)
          InactiveCount = 0 }

    /// Check that an SOMap is empty.
    let isEmpty map =
        SUMap.isEmpty map.Indices

    /// Check that an SOMap is empty.
    let notEmpty map =
        SUMap.notEmpty map.Indices

    /// Get the map key comparer.
    let getComparer map =
        SUMap.getComparer map.Indices

    /// Get the map configuration.
    let getConfig map =
        SUMap.getConfig map.Indices

    /// Add a value with the key to an SOMap.
    let add (key : 'k) (value : 'v) map =
        match SUMap.tryFind key map.Indices with
        | Some index ->
            { map with
                Indices = SUMap.add key index map.Indices
                Entries = FStack.replaceAt index struct (true, key, value) map.Entries }
        | None ->
            { map with
                Indices = SUMap.add key (FStack.length map.Entries) map.Indices
                Entries = FStack.conj struct (true, key, value) map.Entries }

    /// Add all the given entries to an SOMap.
    let addMany entries map =
        Seq.fold (fun map (key : 'k, value : 'v) -> add key value map) map entries

    /// Remove a value with the given key from an SOMap.
    let remove (key : 'k) map =
        match SUMap.tryFind key map.Indices with
        | Some index ->
            let struct (_, _, _) = map.Entries.[index]
            let map =
                { Indices = SUMap.remove key map.Indices
                  Entries = FStack.replaceAt index struct (false, Unchecked.defaultof<_>, Unchecked.defaultof<_>) map.Entries
                  InactiveCount = inc map.InactiveCount }
            if map.InactiveCount > FStack.length map.Entries / 2
            then compact map
            else map
        | None -> map

    /// Remove all values with the given keys from an SOMap.
    let removeMany keys map =
        Seq.fold (fun map (key : 'k) -> remove key map) map keys

    /// Remove a value with the given key from an SOMap.
    let removeBy (by : 'v -> bool) (map : SOMap<'k, 'v>) =
        FStack.fold
            (fun map struct (active, k, v) -> if active && by v then remove k map else map)
            map map.Entries

    /// Try to find a value with the given key in an SOMap.
    /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
    let tryFind (key : 'k) map : 'v option =
        match SUMap.tryFind key map.Indices with
        | Some index -> match map.Entries.[index] with (_, _, v) -> Some v
        | None -> None

    /// Try to find a value with the given key in an SOMap without allocating.
    /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
    let tryGetValue (key : 'k, map, valueRef : _ outref) =
        let mutable indexRef = 0
        match SUMap.tryGetValue (key, map.Indices, &indexRef) with
        | true ->
            let struct (_, _, v) = map.Entries.[indexRef]
            valueRef <- v
            true
        | false -> false

    /// Find a value with the given key in an SOMap.
    /// Constant-time complexity with approx. 1/3 speed of Dictionary.GetValue.
    let find (key : 'k) map : 'v =
        let struct (_, _, value) = map.Entries.[map.Indices.[key]]
        value

    /// Try to find a value with the predicate.
    let tryFindBy by map =
        let foundOpt = FStack.tryFind (fun struct (active, key, value) -> if active then by key value else false) map.Entries
        Option.map (fun (struct (_, _, v)) -> v) foundOpt

    /// Find a value with the given predicate.
    let findBy by map =
        match tryFindBy by map with
        | Some found -> found
        | None -> raise (KeyNotFoundException "Could not find key in SOMap.")

    /// Check that an SOMap contains a value with the given key.
    let containsKey key map =
        SUMap.containsKey key map.Indices
        
    /// Combine the contents of two SOMaps, taking an item from the second map in the case of a key conflict.
    let concat map map2 =
        Seq.fold (flip (uncurry add)) map map2

    /// Fold over an SOMap.
    let fold folder (state : 's) (map : SOMap<'k, 'v>) =
        Seq.fold (fun s struct (a, k, v) -> if a then folder s k v else s) state map.Entries

    /// Fold over the values of an SOMap.
    let foldv folder (state : 's) (map : SOMap<'k, 'v>) =
        Seq.fold (fun s struct (a, _, v) -> if a then folder s v else s) state map.Entries

    /// Map over an SOMap.
    let map mapper map =
        fold
            (fun state key value -> add key (mapper key value) state)
            (makeEmpty (SUMap.getComparer map.Indices) (SUMap.getConfig map.Indices))
            map

    /// Filter an SOMap.
    let filter pred map =
        fold
            (fun state key value -> if pred key value then add key value state else state)
            (makeEmpty (SUMap.getComparer map.Indices) (SUMap.getConfig map.Indices))
            map

    /// Convert an SOMap to a sequence of pairs of keys and values.
    let toSeq (map : SOMap<'k, 'v>) =
        map :> _ IEnumerable

    /// Convert a sequence of keys and values to an SOMap.
    let ofSeq comparer config pairs =
        Seq.fold
            (fun map (key, value) -> add key value map)
            (makeEmpty comparer config)
            pairs

    /// Make an SOMap with a single entry.
    let singleton<'k, 'v> comparer config key value =
        let empty = makeEmpty<'k, 'v> comparer config
        add key value empty

/// An ordered persistent map based on SUMap and FStack.
/// NOTE: not supported by SymbolicConverter.
type SOMap<'k, 'v> = SOMap.SOMap<'k, 'v>
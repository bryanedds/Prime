namespace Prime
open System
open System.Collections
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module OMap =

    /// An ordered persistent map based on UMap and FStack.
    /// NOTE: not supported by SymbolicConverter.
    type [<NoEquality; NoComparison>] OMap<'k, 'v when 'k : equality> =
        private
            { Indices : UMap<'k, int>
              Entries : struct (bool * 'k * 'v) FStack
              InactiveCount : int }

        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                this.Entries |>
                Seq.filter (fun (struct (active, _, _)) -> active) |>
                Seq.map (fun (struct (_, k, v)) -> (k, v)) |>
                fun entries -> entries.GetEnumerator ()

        interface IEnumerable with
            member this.GetEnumerator () =
                this :>
                IEnumerable<_> :>
                IEnumerable |>
                fun entries -> entries.GetEnumerator ()

        /// Try to find a value with the given key in an OMap without allocating.
        /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
        member this.TryGetValue (key, valueRef : 'v byref) =
            let mutable indexRef = 0
            match UMap.tryGetValue (key, this.Indices, &indexRef) with
            | true ->
                let struct (_, _, v) = this.Entries.[indexRef]
                valueRef <- v
                true
            | false -> false

        member this.Item
            with get (key : 'k) =
                let index = this.Indices.[key]
                this.Entries.[index]

    let private compact map =
        let entries = FStack.filter (fun (struct (a, _, _)) -> a) map.Entries
        let indices = Seq.foldi (fun i u (struct (_, k, _)) -> UMap.add k i u) (UMap.makeEmpty (UMap.getConfig map.Indices)) entries
        { Indices = indices
          Entries = entries
          InactiveCount = 0 }

    /// Create an empty OMap.
    let makeEmpty<'k, 'v when 'k : equality> config =
        { Indices = UMap.makeEmpty<'k, int> config
          Entries = (FStack.empty : struct (bool * 'k * 'v) FStack)
          InactiveCount = 0 }

    /// Check that an OMap is empty.
    let isEmpty map =
        UMap.isEmpty map.Indices

    /// Check that an OMap is empty.
    let notEmpty map =
        UMap.notEmpty map.Indices

    /// Add a value with the key to an OMap.
    let add (key : 'k) (value : 'v) map =
        match UMap.tryFind key map.Indices with
        | Some index ->
            { map with
                Indices = UMap.add key index map.Indices
                Entries = FStack.replaceAt index struct (true, key, value) map.Entries }
        | None ->
            { map with
                Indices = UMap.add key (FStack.length map.Entries) map.Indices
                Entries = FStack.conj struct (true, key, value) map.Entries }

    /// Add all the given entries to an OMap.
    let addMany entries map =
        Seq.fold (fun map (key : 'k, value : 'v) -> add key value map) map entries

    /// Remove a value with the given key from an OMap.
    let remove (key : 'k) map =
        match UMap.tryFind key map.Indices with
        | Some index ->
            let struct (_, k, v) = map.Entries.[index]
            let map =
                { Indices = UMap.remove key map.Indices
                  Entries = FStack.replaceAt index struct (false, k, v) map.Entries
                  InactiveCount = inc map.InactiveCount }
            if map.InactiveCount > FStack.length map.Entries
            then compact map
            else map
        | None -> map

    /// Remove all values with the given keys from an OMap.
    let removeMany keys map =
        Seq.fold (fun map (key : 'k) -> remove key map) map keys

    /// Remove a value with the given key from an OMap.
    let removeBy (by : 'v -> bool) (map : OMap<'k, 'v>) =
        FStack.fold
            (fun map struct (active, k, v) -> if active && by v then remove k map else map)
            map map.Entries

    /// Try to find a value with the given key in an OMap.
    /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
    let tryFind (key : 'k) map : 'v option =
        match UMap.tryFind key map.Indices with
        | Some index -> match map.Entries.[index] with (_, _, v) -> Some v
        | None -> None

    /// Try to find a value with the given key in an OMap without allocating.
    /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
    let tryGetValue (key : 'k, map, valueRef : _ byref) =
        let mutable indexRef = 0
        match UMap.tryGetValue (key, map.Indices, &indexRef) with
        | true ->
            let struct (_, _, v) = map.Entries.[indexRef]
            valueRef <- v
            true
        | false -> false

    /// Find a value with the given key in an OMap.
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
        | None -> raise (KeyNotFoundException "Could not find key in OMap.")

    /// Check that an OMap contains a value with the given key.
    let containsKey key map =
        UMap.containsKey key map.Indices
        
    /// Combine the contents of two OMaps, taking an item from the second map in the case of a key conflict.
    let concat map map2 =
        Seq.fold (flip (uncurry add)) map map2

    /// Fold over an OMap.
    let fold folder state (map : OMap<'k, 'v>) =
        Seq.fold (fun s struct (a, k, v) -> if a then folder s k v else s) state map.Entries

    /// Map over an OMap.
    let map mapper map =
        fold
            (fun state key value -> add key (mapper key value) state)
            (makeEmpty (UMap.getConfig map.Indices))
            map

    /// Filter an OMap.
    let filter pred map =
        fold
            (fun state key value -> if pred key value then add key value state else state)
            (makeEmpty (UMap.getConfig map.Indices))
            map

    /// Convert an OMap to a sequence of pairs of keys and values.
    let toSeq (map : OMap<'k, 'v>) =
        map :> _ IEnumerable

    /// Convert a sequence of keys and values to an OMap.
    let ofSeq pairs config =
        Seq.fold
            (fun map (key, value) -> add key value map)
            (makeEmpty config)
            pairs

    /// Make an OMap with a single entry.
    let makeSingleton<'k, 'v when 'k : equality> key value config =
        let empty = makeEmpty<'k, 'v> config
        add key value empty

/// An ordered persistent map based on UMap and FStack.
/// NOTE: not supported by SymbolicConverter.
type OMap<'k, 'v when 'k : equality> = OMap.OMap<'k, 'v>
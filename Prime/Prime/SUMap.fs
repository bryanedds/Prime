// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SUMap =

    /// A hashing map that supports transaction-based rewinding, but with a more convenient interface than STMap.
    type [<ReferenceEquality; DefaultValue "[]">] SUMap<'k, 'v> =
        private
            { mutable Map : STMap<'k, 'v> }

        member this.TryGetValue (key, valueRef : 'v outref) =
            let struct (found, tmap) = STMap.tryGetValue (key, this.Map, &valueRef)
            this.Map <- tmap
            found

        member this.Item with get key =
            let struct (item, tmap) = STMap.find key this.Map
            this.Map <- tmap
            item
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                let struct (seq, tmap) = STMap.toSeq this.Map
                this.Map <- tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> seq<'k * 'v>).GetEnumerator () :> IEnumerator

    /// Create a SUMap containing the given sequence of entries.
    let makeFromSeq<'k, 'v> comparer config entries =
        { Map = STMap.makeFromSeq<'k, 'v> comparer config entries }

    /// Create an empty SUMap.
    let makeEmpty<'k, 'v> comparer config =
        { Map = STMap.makeEmpty<'k, 'v> comparer config }

    /// Get the comparer function used to determine key uniqueness in a SUMap.
    let comparer map =
        let struct (result, tmap) = STMap.comparer map.Map
        map.Map <- tmap
        result

    /// Get the semantic configuration of the TSet.
    let config map =
        let struct (result, tmap) = STMap.config map.Map
        map.Map <- tmap
        result

    /// Add an entry to a SUMap.
    let add key value map =
        { Map = STMap.add key value map.Map }

    /// Remove any entry with a matching key from a SUMap.
    let remove key map =
        { Map = STMap.remove key map.Map }

    /// Clear all elements from a SUMap.
    let clear map =
        { Map = STMap.clear map.Map }

    /// Check that a SUMap has no entries.
    let isEmpty map =
        let struct (result, tmap) = STMap.isEmpty map.Map
        map.Map <- tmap
        result

    /// Check that a SUMap has one or more entries.
    let notEmpty map =
        not (isEmpty map)

    /// Get the length of a SUMap (constant-time).
    let length map =
        let struct (result, tmap) = STMap.length map.Map
        map.Map <- tmap
        result

    /// Attempt to get the value with the given key.
    let tryFind key map =
        let struct (valueOpt, tmap) = STMap.tryFind key map.Map
        map.Map <- tmap
        valueOpt

    /// Attempt to get the value with the given key.
    let tryGetValue (key, map, valueRef : _ outref) =
        let struct (found, tmap) = STMap.tryGetValue (key, map.Map, &valueRef)
        map.Map <- tmap
        found

    /// Find the given keyed value or raise a KeyNotFoundException.
    let find key (map : SUMap<'k, 'v>) =
        map.[key]

    /// Check that a SUMap contains the given key.
    let containsKey key map =
        let struct (result, tmap) = STMap.containsKey key map.Map
        map.Map <- tmap
        result

    /// Add all the given entries to a SUMap.
    let addMany entries map =
        { Map = STMap.addMany entries map.Map }

    /// Remove all values with the given keys from a SUMap.
    let removeMany keys map =
        { Map = STMap.removeMany keys map.Map }

    /// Convert a sequence of keys and values to a SUMap.
    let ofSeq comparer config pairs =
        Seq.fold
            (fun map (key, value) -> add key value map)
            (makeEmpty comparer config)
            pairs

    /// Convert a SUMap to a seq. Note that the entire map is iterated eagerly when functional.
    let toSeq (map : SUMap<_, _>) =
        map :> _ seq

    /// Convert a SUMap to a SDictionary.
    let toDict (map : SUMap<_, _>) =
        let struct (dict, tmap) = STMap.toDict map.Map
        map.Map <- tmap
        dict

    /// Fold over the entries of a SUMap.
    let fold folder state map =
        let struct (result, tmap) = STMap.fold folder state map.Map
        map.Map <- tmap
        result

    /// Map over the entries of a SUMap.
    let map mapper map =
        let struct (result, tmap) = STMap.map mapper map.Map
        map.Map <- tmap
        { Map = result }

    /// Filter the entries of a SUMap.
    let filter pred map =
        let struct (result, tmap) = STMap.filter pred map.Map
        map.Map <- tmap
        { Map = result }

    /// Iterate over the entries of a SUMap with an action.
    let iter action map =
        let tmap = STMap.iter action map.Map
        map.Map <- tmap

    /// Make a SUMap with a single entry.
    let singleton<'k, 'v> comparer config key value =
        { Map = STMap.singleton<'k, 'v> comparer config key value }

/// A hashing map that supports transaction-based rewinding, but with a more convenient interface than STMap.
type SUMap<'k, 'v> = SUMap.SUMap<'k, 'v>
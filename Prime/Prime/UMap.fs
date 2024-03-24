// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module UMap =

    /// A hashing map that supports transaction-based rewinding, but with a more convenient interface than TMap.
    type [<ReferenceEquality>] UMap<'k, 'v> =
        private
            { mutable Map : TMap<'k, 'v> }

        member this.TryGetValue (key, valueRef : 'v outref) =
            let struct (found, tmap) = TMap.tryGetValue (key, this.Map, &valueRef)
            this.Map <- tmap
            found

        member this.Item with get key =
            let struct (item, tmap) = TMap.find key this.Map
            this.Map <- tmap
            item
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                let struct (seq, tmap) = TMap.toSeq this.Map
                this.Map <- tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> seq<'k * 'v>).GetEnumerator () :> IEnumerator

    /// Create a UMap containing the given sequence of entries.
    let makeFromSeq<'k, 'v> comparer config entries =
        { Map = TMap.makeFromSeq<'k, 'v> comparer config entries }

    /// Create an empty UMap.
    let makeEmpty<'k, 'v> comparer config =
        { Map = TMap.makeEmpty<'k, 'v> comparer config }

    /// Get the comparer function used to determine key uniqueness in a UMap.
    let getComparer map =
        let struct (result, tmap) = TMap.getComparer map.Map
        map.Map <- tmap
        result

    /// Get the semantic configuration of the TSet.
    let getConfig map =
        let struct (result, tmap) = TMap.getConfig map.Map
        map.Map <- tmap
        result

    /// Add an entry to a UMap.
    let add key value map =
        { Map = TMap.add key value map.Map }

    /// Remove any entry with a matching key from a UMap.
    let remove key map =
        { Map = TMap.remove key map.Map }

    /// Clear all elements from a UMap.
    let clear map =
        { Map = TMap.clear map.Map }

    /// Check that a UMap has no entries.
    let isEmpty map =
        let struct (result, tmap) = TMap.isEmpty map.Map
        map.Map <- tmap
        result

    /// Check that a UMap has one or more entries.
    let notEmpty map =
        not (isEmpty map)

    /// Get the length of a UMap (constant-time).
    let length map =
        let struct (result, tmap) = TMap.length map.Map
        map.Map <- tmap
        result

    /// Attempt to get the value with the given key.
    let tryFind key map =
        let struct (valueOpt, tmap) = TMap.tryFind key map.Map
        map.Map <- tmap
        valueOpt

    /// Attempt to get the value with the given key.
    let tryGetValue (key, map, valueRef : _ outref) =
        let struct (found, tmap) = TMap.tryGetValue (key, map.Map, &valueRef)
        map.Map <- tmap
        found

    /// Find the given keyed value or raise a KeyNotFoundException.
    let find key (map : UMap<'k, 'v>) =
        map.[key]

    /// Check that a UMap contains the given key.
    let containsKey key map =
        let struct (result, tmap) = TMap.containsKey key map.Map
        map.Map <- tmap
        result

    /// Add all the given entries to a UMap.
    let addMany entries map =
        { Map = TMap.addMany entries map.Map }

    /// Remove all values with the given keys from a UMap.
    let removeMany keys map =
        { Map = TMap.removeMany keys map.Map }

    /// Convert a sequence of keys and values to a UMap.
    let ofSeq comparer config pairs =
        Seq.fold
            (fun map (key, value) -> add key value map)
            (makeEmpty comparer config)
            pairs

    /// Convert a UMap to a seq. Note that entire map is iterated eagerly since the underlying
    /// Dictionary could otherwise opaquely change during iteration.
    let toSeq (map : UMap<_, _>) =
        map :> _ seq

    /// Convert a UMap to a Dictionary.
    let toDict (map : UMap<_, _>) =
        let struct (dict, tmap) = TMap.toDict map.Map
        map.Map <- tmap
        dict

    /// Fold over the entries of a UMap.
    let fold folder state map =
        let struct (result, tmap) = TMap.fold folder state map.Map
        map.Map <- tmap
        result

    /// Map over the entries of a UMap.
    let map mapper map =
        let struct (result, tmap) = TMap.map mapper map.Map
        map.Map <- tmap
        { Map = result }

    /// Filter the entries of a UMap.
    let filter pred map =
        let struct (result, tmap) = TMap.filter pred map.Map
        map.Map <- tmap
        { Map = result }

    /// Make a UMap with a single entry.
    let singleton<'k, 'v> comparer config key value =
        { Map = TMap.singleton<'k, 'v> comparer config key value }

/// A hashing map that supports transaction-based rewinding, but with a more convenient interface than TMap.
type UMap<'k, 'v> = UMap.UMap<'k, 'v>
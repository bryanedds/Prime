// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module UMap =

    type [<ReferenceEquality>] UMap<'k, 'v> =
        private
            { mutable Map : TMap<'k, 'v> }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                let struct (seq, tmap) = TMap.toSeq this.Map
                this.Map <- tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> seq<'k * 'v>).GetEnumerator () :> IEnumerator

        member this.TryGetValue (key, valueRef : 'v outref) =
            let struct (found, tmap) = TMap.tryGetValue (key, this.Map, &valueRef)
            this.Map <- tmap
            found

        member this.Item with get key =
            let struct (item, tmap) = TMap.find key this.Map
            this.Map <- tmap
            item

    let makeFromSeq<'k, 'v> comparer config entries =
        { Map = TMap.makeFromSeq<'k, 'v> comparer config entries }

    let makeEmpty<'k, 'v> comparer config =
        { Map = TMap.makeEmpty<'k, 'v> comparer config }

    let getComparer map =
        let struct (result, tmap) = TMap.getComparer map.Map
        map.Map <- tmap
        result

    let getConfig map =
        let struct (result, tmap) = TMap.getConfig map.Map
        map.Map <- tmap
        result

    let add key value map =
        { Map = TMap.add key value map.Map }

    let remove key map =
        { Map = TMap.remove key map.Map }

    let length map =
        let struct (result, tmap) = TMap.length map.Map
        map.Map <- tmap
        result

    let isEmpty map =
        let struct (result, tmap) = TMap.isEmpty map.Map
        map.Map <- tmap
        result

    let notEmpty map =
        not (isEmpty map)

    let tryFind key map =
        let struct (valueOpt, tmap) = TMap.tryFind key map.Map
        map.Map <- tmap
        valueOpt

    let tryGetValue (key, map, valueRef : _ outref) =
        let struct (found, tmap) = TMap.tryGetValue (key, map.Map, &valueRef)
        map.Map <- tmap
        found

    let find key (map : UMap<'k, 'v>) =
        map.[key]

    let containsKey key map =
        let struct (result, tmap) = TMap.containsKey key map.Map
        map.Map <- tmap
        result

    /// Add all the given entries to the map.
    let addMany entries map =
        { Map = TMap.addMany entries map.Map }

    /// Remove all values with the given keys from the map.
    let removeMany keys map =
        { Map = TMap.removeMany keys map.Map }

    let toSeq (map : UMap<_, _>) =
        map :> _ seq

    let toDict (map : UMap<_, _>) =
        let struct (dict, tmap) = TMap.toDict map.Map
        map.Map <- tmap
        dict

    /// Convert a sequence of keys and values to a UMap.
    let ofSeq comparer config pairs =
        Seq.fold
            (fun map (key, value) -> add key value map)
            (makeEmpty comparer config)
            pairs

    let fold folder state map =
        let struct (result, tmap) = TMap.fold folder state map.Map
        map.Map <- tmap
        result

    let map mapper map =
        let struct (result, tmap) = TMap.map mapper map.Map
        map.Map <- tmap
        { Map = result }

    let filter pred map =
        let struct (result, tmap) = TMap.filter pred map.Map
        map.Map <- tmap
        { Map = result }

    let singleton<'k, 'v> comparer config key value =
        { Map = TMap.singleton<'k, 'v> comparer config key value }

type UMap<'k, 'v> = UMap.UMap<'k, 'v>
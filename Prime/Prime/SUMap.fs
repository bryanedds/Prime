﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SUMap =

    type [<ReferenceEquality>] SUMap<'k, 'v> =
        private
            { mutable Map : STMap<'k, 'v> }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                let struct (seq, tmap) = STMap.toSeq this.Map
                this.Map <- tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> seq<'k * 'v>).GetEnumerator () :> IEnumerator

        member this.TryGetValue (key, valueRef : 'v outref) =
            let struct (found, tmap) = STMap.tryGetValue (key, this.Map, &valueRef)
            this.Map <- tmap
            found

        member this.Item with get key =
            let struct (item, tmap) = STMap.find key this.Map
            this.Map <- tmap
            item

    let makeFromSeq<'k, 'v> comparer config entries =
        { Map = STMap.makeFromSeq<'k, 'v> comparer config entries }

    let makeEmpty<'k, 'v> comparer config =
        { Map = STMap.makeEmpty<'k, 'v> comparer config }

    let getComparer map =
        let struct (result, tmap) = STMap.getComparer map.Map
        map.Map <- tmap
        result

    let getConfig map =
        let struct (result, tmap) = STMap.getConfig map.Map
        map.Map <- tmap
        result

    let add key value map =
        { Map = STMap.add key value map.Map }

    let remove key map =
        { Map = STMap.remove key map.Map }

    let length map =
        let struct (result, tmap) = STMap.length map.Map
        map.Map <- tmap
        result

    let isEmpty map =
        let struct (result, tmap) = STMap.isEmpty map.Map
        map.Map <- tmap
        result

    let notEmpty map =
        not (isEmpty map)

    let tryFind key map =
        let struct (valueOpt, tmap) = STMap.tryFind key map.Map
        map.Map <- tmap
        valueOpt

    let tryGetValue (key, map, valueRef : _ outref) =
        let struct (found, tmap) = STMap.tryGetValue (key, map.Map, &valueRef)
        map.Map <- tmap
        found

    let find key (map : SUMap<'k, 'v>) =
        map.[key]

    let containsKey key map =
        let struct (result, tmap) = STMap.containsKey key map.Map
        map.Map <- tmap
        result

    /// Add all the given entries to the map.
    let addMany entries map =
        { Map = STMap.addMany entries map.Map }

    /// Remove all values with the given keys from the map.
    let removeMany keys map =
        { Map = STMap.removeMany keys map.Map }

    let toSeq (map : SUMap<_, _>) =
        map :> _ seq

    let toDict (map : SUMap<_, _>) =
        let struct (dict, tmap) = STMap.toDict map.Map
        map.Map <- tmap
        dict

    /// Convert a sequence of keys and values to a SUMap.
    let ofSeq comparer config pairs =
        Seq.fold
            (fun map (key, value) -> add key value map)
            (makeEmpty comparer config)
            pairs

    let fold folder state map =
        let struct (result, tmap) = STMap.fold folder state map.Map
        map.Map <- tmap
        result

    let map mapper map =
        let struct (result, tmap) = STMap.map mapper map.Map
        map.Map <- tmap
        { Map = result }

    let filter pred map =
        let struct (result, tmap) = STMap.filter pred map.Map
        map.Map <- tmap
        { Map = result }

    let singleton<'k, 'v> comparer config key value =
        { Map = STMap.singleton<'k, 'v> comparer config key value }

type SUMap<'k, 'v> = SUMap.SUMap<'k, 'v>
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module UMap =

    type [<NoEquality; NoComparison>] UMap<'k, 'v when 'k : equality> =
        private
            { mutable Map : TMap<'k, 'v> }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                let struct (seq, tmap) = TMap.toSeq this.Map
                this.Map <- tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'k * 'v>).GetEnumerator () :> IEnumerator

    let makeFromSeq<'k, 'v when 'k : equality> config entries =
        { Map = TMap.makeFromSeq<'k, 'v> config entries }

    let makeEmpty<'k, 'v when 'k : equality> config =
        { Map = TMap.makeEmpty<'k, 'v> config }

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

    let tryGetValue key map =
        let struct (result, tmap) = TMap.tryGetValue key map.Map
        map.Map <- tmap
        result

    let find key map =
        let struct (item, tmap) = TMap.find key map.Map
        map.Map <- tmap
        item

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

type UMap<'k, 'v when 'k : equality> = UMap.UMap<'k, 'v>
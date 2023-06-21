﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections.Generic

// TODO: document this!

[<RequireQualifiedAccess>]
module STMap =

    type private Log<'k, 'v> =
        | Add of key : 'k * value : 'v
        | Remove of remove : 'k
        | Clear

    type [<ReferenceEquality>] STMap<'k, 'v> =
        private
            { mutable STMapOpt : STMap<'k, 'v>
              TConfig : TConfig
              Dict : SDictionary<'k, 'v>
              DictOrigin : SDictionary<'k, 'v>
              Logs : Log<'k, 'v> list
              LogsLength : int }

        static member (>>.) (map : STMap<'k2, 'v2>, builder : TExpr<unit, STMap<'k2, 'v2>>) =
            snd' (builder map)

        static member (.>>) (map : STMap<'k2, 'v2>, builder : TExpr<'v2, STMap<'k2, 'v2>>) =
            fst' (builder map)

        static member (.>>.) (map : STMap<'k2, 'v2>, builder : TExpr<'v2, STMap<'k2, 'v2>>) =
            builder map

    let private commit map =
        let oldMap = map
        let dictOrigin = SDictionary.makeFromSegmentedDictionary map.DictOrigin
        List.foldBack (fun log () ->
            match log with
            | Add (key, value) -> dictOrigin.[key] <- value
            | Remove key -> dictOrigin.Remove key |> ignore
            | Clear -> dictOrigin.Clear ())
            map.Logs ()
        let dict = SDictionary.makeFromSegmentedDictionary dictOrigin
        let map = { map with Dict = dict; DictOrigin = dictOrigin; Logs = []; LogsLength = 0 }
        oldMap.STMapOpt <- Unchecked.defaultof<STMap<'k, 'v>>
        map.STMapOpt <- map
        map

    let private compress map =
        let oldMap = map
        let dictOrigin = SDictionary.makeFromSegmentedDictionary map.Dict
        let map = { map with DictOrigin = dictOrigin; Logs = []; LogsLength = 0 }
        oldMap.STMapOpt <- Unchecked.defaultof<STMap<'k, 'v>>
        map.STMapOpt <- map
        map

    let private validate2 map =
        lock map.Logs (fun () ->
            match box map.STMapOpt with
            | null -> commit map
            | target ->
                match obj.ReferenceEquals (target, map) with
                | true -> if map.LogsLength > map.Dict.Count then compress map else map
                | false -> commit map)

    let private update updater map =
        let oldMap = map
        let map = validate2 map
        let map = updater map
        oldMap.STMapOpt <- Unchecked.defaultof<STMap<'k, 'v>>
        map.STMapOpt <- map
        map

    let private validate map =
        if TConfig.isFunctional map.TConfig
        then validate2 map
        else map

    let makeFromSeq<'k, 'v> (comparer : 'k IEqualityComparer) config (entries : ('k * 'v) seq) =
        if TConfig.isFunctional config then 
            let dict = SDictionary.ofSeq comparer entries
            let dictOrigin = SDictionary.makeFromSegmentedDictionary dict
            let map =
                { STMapOpt = Unchecked.defaultof<STMap<'k, 'v>>
                  TConfig = config
                  Dict = dict
                  DictOrigin = dictOrigin
                  Logs = []
                  LogsLength = 0 }
            map.STMapOpt <- map
            map
        else
            { STMapOpt = Unchecked.defaultof<STMap<'k, 'v>>
              TConfig = config
              Dict = SDictionary.ofSeq comparer entries
              DictOrigin = SDictionary.make comparer
              Logs = []
              LogsLength = 0 }

    let makeEmpty<'k, 'v> comparer config =
        makeFromSeq<'k, 'v> comparer config Seq.empty
        
    let getComparer map =
        struct (map.Dict.Comparer, map)

    let getConfig map =
        struct (map.TConfig, map)

    let add key value map =
        if TConfig.isFunctional map.TConfig then
            update (fun map ->
                let map = { map with Logs = Add (key, value) :: map.Logs; LogsLength = map.LogsLength + 1 }
                map.Dict.[key] <- value
                map)
                map
        else map.Dict.[key] <- value; map

    let remove key map =
        if TConfig.isFunctional map.TConfig then
            update (fun map ->
                let map = { map with Logs = Remove key :: map.Logs; LogsLength = map.LogsLength + 1 }
                map.Dict.Remove key |> ignore
                map)
                map
        else map.Dict.Remove key |> ignore; map

    let clear map =
        if TConfig.isFunctional map.TConfig then
            update (fun map ->
                let map = { map with Logs = Clear :: map.Logs; LogsLength = map.LogsLength + 1 }
                map.Dict.Clear ()
                map)
                map
        else map.Dict.Clear (); map

    let isEmpty map =
        let map = validate map
        struct (map.Dict.Count = 0, map)

    let notEmpty map =
        mapFst' not (isEmpty map)

    /// Get the length of the map (constant-time).
    let length map =
        let map = validate map
        struct (map.Dict.Count, map)

    let tryFind key map =
        let map = validate map
        match map.Dict.TryGetValue key with
        | (true, value) -> struct (Some value, map)
        | (false, _) -> struct (None, map)

    let tryGetValue (key, map, valueRef : _ outref) =
        let map = validate map
        let found = map.Dict.TryGetValue (key, &valueRef)
        struct (found, map)

    let find key map =
        let map = validate map
        struct (map.Dict.[key], map)

    let containsKey key map =
        match tryFind key map with
        | struct (Some _, map) -> struct (true, map)
        | struct (None, map) -> struct (false, map)

    /// Add all the given entries to the map.
    let addMany entries map =
        Seq.fold (flip (uncurry add)) map entries

    /// Remove all values with the given keys from the map.
    let removeMany keys map =
        Seq.fold (flip remove) map keys

    /// Convert a STMap to a seq. Note that entire map is iterated eagerly since the underlying
    /// SDictionary could otherwise opaquely change during iteration.
    let toSeq map =
        let map = validate map
        let seq =
            map.Dict |>
            Seq.map (fun kvp -> (kvp.Key, kvp.Value)) |>
            Array.ofSeq :>
            seq<'k * 'v>
        struct (seq, map)

    /// Convert a STMap to a SDictionary.
    let toDict map =
        let dict = validate map
        let result = SDictionary.makeFromSegmentedDictionary map.Dict
        struct (result, dict)

    let fold folder state map =
        let struct (seq, map) = toSeq map
        let result = Seq.fold (folder >> uncurry) state seq
        struct (result, map)

    let map mapper map =
        fold
            (fun map key value -> add key (mapper value) map)
            (makeEmpty map.Dict.Comparer map.TConfig)
            map

    let filter pred map =
        fold
            (fun state k v -> if pred k v then add k v state else state)
            (makeEmpty map.Dict.Comparer map.TConfig)
            map

    let singleton<'k, 'v> comparer config (key : 'k) (value : 'v) =
        makeFromSeq comparer config [(key, value)]

type STMap<'k, 'v> = STMap.STMap<'k, 'v>

[<AutoOpen>]
module STMapBuilder = 

    let tmap<'k, 'v> = TExprBuilder<STMap<'k, 'v>> ()
﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections.Generic

// TODO: document this!

[<RequireQualifiedAccess>]
module STSet =

    type private 'a Log =
        | Add of add : 'a
        | Remove of remove : 'a
        | Clear

    type [<ReferenceEquality>] STSet<'a when 'a : equality> =
        private
            { mutable STSetOpt : 'a STSet
              TConfig : TConfig
              HashSet : 'a SHashSet
              HashSetOrigin : 'a SHashSet
              Logs : 'a Log list
              LogsLength : int }

        static member (>>.) (set : 'a2 STSet, builder : TExpr<unit, 'a2 STSet>) =
            snd' (builder set)

        static member (.>>) (set : 'a2 STSet, builder : TExpr<'a2, 'a2 STSet>) =
            fst' (builder set)

        static member (.>>.) (set : 'a2 STSet, builder : TExpr<'a2, 'a2 STSet>) =
            builder set

    let private commit set =
        let oldSet = set
        let hashSetOrigin = SHashSet.makeFromSegmentedHashSet set.HashSetOrigin
        List.foldBack (fun log () ->
            match log with
            | Add value -> hashSetOrigin.Add value |> ignore
            | Remove value -> hashSetOrigin.Remove value |> ignore
            | Clear -> hashSetOrigin.Clear ())
            set.Logs ()
        let hashSet = SHashSet.makeFromSegmentedHashSet hashSetOrigin
        let set = { set with HashSet = hashSet; HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
        oldSet.STSetOpt <- Unchecked.defaultof<'a STSet>
        set.STSetOpt <- set
        set

    let private compress set =
        let oldSet = set
        let hashSetOrigin = SHashSet.makeFromSegmentedHashSet set.HashSet
        let set = { set with HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
        oldSet.STSetOpt <- Unchecked.defaultof<'a STSet>
        set.STSetOpt <- set
        set

    let private validate2 set =
        lock set.Logs (fun () ->
            match box set.STSetOpt with
            | null -> commit set
            | target ->
                match obj.ReferenceEquals (target, set) with
                | true -> if set.LogsLength > set.HashSet.Count then compress set else set
                | false -> commit set)

    let private update updater set =
        let oldSet = set
        let set = validate2 set
        let set = updater set
        oldSet.STSetOpt <- Unchecked.defaultof<'a STSet>
        set.STSetOpt <- set
        set

    let private validate set =
        if TConfig.isFunctional set.TConfig
        then validate2 set
        else set

    let makeFromSegmentedHashSet (comparer : 'a IEqualityComparer) config (hashSet : 'a SHashSet) =
        if TConfig.isFunctional config then
            let set =
                { STSetOpt = Unchecked.defaultof<'a STSet>
                  TConfig = config
                  HashSet = hashSet
                  HashSetOrigin = SHashSet.makeFromSegmentedHashSet hashSet
                  Logs = []
                  LogsLength = 0 }
            set.STSetOpt <- set
            set
        else
            { STSetOpt = Unchecked.defaultof<'a STSet>
              TConfig = config
              HashSet = hashSet
              HashSetOrigin = SHashSet.make comparer
              Logs = []
              LogsLength = 0 }

    let makeFromSeq<'a when 'a : equality> comparer config (items : 'a seq) =
        let hashSet = SHashSet.ofSeq comparer items
        makeFromSegmentedHashSet comparer config hashSet

    let makeEmpty<'a when 'a : equality> comparer config =
        makeFromSeq<'a> comparer config Seq.empty

    let getComparer set =
        struct (set.HashSet.Comparer, set)

    let getConfig set =
        struct (set.TConfig, set)

    let add value set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Add value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Add value |> ignore
                set)
                set
        else set.HashSet.Add value |> ignore; set

    let remove value set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Remove value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Remove value |> ignore
                set)
                set
        else set.HashSet.Remove value |> ignore; set

    let clear set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Clear :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Clear ()
                set)
                set
        else set.HashSet.Clear (); set

    let isEmpty set =
        let set = validate set
        struct (set.HashSet.Count = 0, set)

    let notEmpty set =
        mapFst' not (isEmpty set)

    /// Get the length of the set (constant-time).
    let length set =
        let set = validate set
        struct (set.HashSet.Count, set)

    let contains value set =
        let set = validate set
        struct (set.HashSet.Contains value, set)

    /// Add all the given values to the set.
    let addMany values set =
        Seq.fold (flip add) set values

    /// Remove all the given values from the set.
    let removeMany values set =
        Seq.fold (flip remove) set values

    /// Convert a STSet to a seq. Note that entire set is iterated eagerly since the underlying HashMap could
    /// otherwise opaquely change during iteration.
    let toSeq set =
        let set = validate set
        let seq = set.HashSet |> Array.ofSeq :> 'a seq
        struct (seq, set)

    /// Convert a STSet to a HashSet.
    let toHashSet set =
        let set = validate set
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        struct (result, set)

    let fold folder state set =
        let struct (seq, set) = toSeq set
        let result = Seq.fold folder state seq
        struct (result, set)

    let map mapper set =
        fold
            (fun set value -> add (mapper value) set)
            (makeEmpty set.HashSet.Comparer set.TConfig)
            set

    let filter pred set =
        fold
            (fun set value -> if pred value then add value set else set)
            (makeEmpty set.HashSet.Comparer set.TConfig)
            set

    let equals set set2 =
        let (set, set2) = (validate set, validate set2)
        struct (set.HashSet.SetEquals set2.HashSet, set, set2)

    let unionFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        result.UnionWith set2.HashSet
        struct (result, set, set2)

    let intersectFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        result.IntersectWith set2.HashSet
        struct (result, set, set2)

    let disjointFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        result.SymmetricExceptWith set2.HashSet
        struct (result, set, set2)

    let differenceFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        result.ExceptWith set2.HashSet
        struct (result, set, set2)

    let union config set set2 =
        let struct (result, set, set2) = unionFast set set2
        struct (makeFromSegmentedHashSet set.HashSet.Comparer config result, set, set2)

    let intersect config set set2 =
        let struct (result, set, set2) = intersectFast set set2
        struct (makeFromSegmentedHashSet set.HashSet.Comparer config result, set, set2)

    let disjoint config set set2 =
        let struct (result, set, set2) = disjointFast set set2
        struct (makeFromSegmentedHashSet set.HashSet.Comparer config result, set, set2)

    let difference config set set2 =
        let struct (result, set, set2) = differenceFast set set2
        struct (makeFromSegmentedHashSet set.HashSet.Comparer config result, set, set2)

    let singleton<'a when 'a : equality> comparer config (item : 'a) =
        makeFromSeq comparer config [item]

type STSet<'a when 'a : equality> = 'a STSet.STSet

[<AutoOpen>]
module STSetBuilder =
    let tset<'a when 'a : equality> = TExprBuilder<'a STSet> ()
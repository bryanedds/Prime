// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module STSet =

    type private 'a Log =
        | Add of add : 'a
        | Remove of remove : 'a
        | Clear

    /// A segmented hashing set that supports transaction-based rewinding.
    type [<ReferenceEquality; DefaultValue "[]">] STSet<'a when 'a : equality> =
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

    /// Create an STSet containing the element of the given hash.
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

    /// Create a STSet containing the given sequence of values.
    let makeFromSeq<'a when 'a : equality> comparer config (items : 'a seq) =
        let hashSet = SHashSet.ofSeq comparer items
        makeFromSegmentedHashSet comparer config hashSet

    /// Create an empty STSet.
    let makeEmpty<'a when 'a : equality> comparer config =
        makeFromSeq<'a> comparer config Seq.empty

    /// Get the comparer function used to determine uniqueness in a STSet.
    let getComparer set =
        struct (set.HashSet.Comparer, set)

    /// Get the semantic configuration of the STSet.
    let getConfig set =
        struct (set.TConfig, set)

    /// Add an element to a STSet.
    let add value set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Add value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Add value |> ignore
                set)
                set
        else set.HashSet.Add value |> ignore; set

    /// Remove all matching elements from a STSet.
    let remove value set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Remove value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Remove value |> ignore
                set)
                set
        else set.HashSet.Remove value |> ignore; set

    /// Remove all elements from a STSet.
    let clear set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Clear :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Clear ()
                set)
                set
        else set.HashSet.Clear (); set

    /// Check that a STSet has no elements.
    let isEmpty set =
        let set = validate set
        struct (set.HashSet.Count = 0, set)

    /// Check that a STSet has one or more elements.
    let notEmpty set =
        mapFst' not (isEmpty set)

    /// Get the length of a STSet (constant-time).
    let length set =
        let set = validate set
        struct (set.HashSet.Count, set)

    /// Determine that a STSet contains the given value.
    let contains value set =
        let set = validate set
        struct (set.HashSet.Contains value, set)

    /// Add all the given values to a STSet.
    let addMany values set =
        Seq.fold (flip add) set values

    /// Remove all the given values from a STSet.
    let removeMany values set =
        Seq.fold (flip remove) set values
        
    /// Make a STSet from a sequence of values.
    let ofSeq comparer config values =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty comparer config)
            values

    /// Convert a STSet to a seq. Note that the entire set is iterated eagerly when functional.
    let toSeq set =
        if TConfig.isFunctional set.TConfig then
            let list = validate2 set
            let struct (sarr, list) = struct (SArray.ofSeq list.HashSet, list)
            struct (sarr :> _ seq, list)
        else struct (set.HashSet, set)

    /// Convert a STSet to an SHashSet.
    let toHashSet set =
        let set = validate set
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        struct (result, set)

    /// Fold over the elements of a STSet.
    let fold folder state set =
        let struct (seq, set) = toSeq set
        let result = Seq.fold folder state seq
        struct (result, set)

    /// Map the elements of a STSet.
    let map mapper set =
        fold
            (fun set value -> add (mapper value) set)
            (makeEmpty set.HashSet.Comparer set.TConfig)
            set

    /// Filter the elements of a STSet.
    let filter pred set =
        fold
            (fun set value -> if pred value then add value set else set)
            (makeEmpty set.HashSet.Comparer set.TConfig)
            set

    /// Determine equality of two STSets.
    let equals set set2 =
        let (set, set2) = (validate set, validate set2)
        struct (set.HashSet.SetEquals set2.HashSet, set, set2)

    /// Construct a union HashSet.
    let unionFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        result.UnionWith set2.HashSet
        struct (result, set, set2)

    /// Construct an intersection HashSet.
    let intersectFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        result.IntersectWith set2.HashSet
        struct (result, set, set2)

    /// Construct a disjoint HashSet.
    let disjointFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        result.SymmetricExceptWith set2.HashSet
        struct (result, set, set2)

    /// Construct a difference HashSet.
    let differenceFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = SHashSet.makeFromSegmentedHashSet set.HashSet
        result.ExceptWith set2.HashSet
        struct (result, set, set2)

    /// Construct a union STSet.
    let union config set set2 =
        let struct (result, set, set2) = unionFast set set2
        struct (makeFromSegmentedHashSet set.HashSet.Comparer config result, set, set2)

    /// Construct an intersection STSet.
    let intersect config set set2 =
        let struct (result, set, set2) = intersectFast set set2
        struct (makeFromSegmentedHashSet set.HashSet.Comparer config result, set, set2)

    /// Construct a disjoint STSet.
    let disjoint config set set2 =
        let struct (result, set, set2) = disjointFast set set2
        struct (makeFromSegmentedHashSet set.HashSet.Comparer config result, set, set2)

    /// Construct a difference STSet.
    let difference config set set2 =
        let struct (result, set, set2) = differenceFast set set2
        struct (makeFromSegmentedHashSet set.HashSet.Comparer config result, set, set2)

    /// Make a STSet with a single element.
    let singleton<'a when 'a : equality> comparer config (item : 'a) =
        makeFromSeq comparer config [item]

/// A hashing set that supports transaction-based rewinding.
type STSet<'a when 'a : equality> = 'a STSet.STSet

[<AutoOpen>]
module STSetBuilder =

    /// Build a STSet.
    let tset<'a when 'a : equality> = TExprBuilder<'a STSet> ()
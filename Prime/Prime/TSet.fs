// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module TSet =

    type private 'a Log =
        | Add of add : 'a
        | Remove of remove : 'a
        | Clear

    /// A hashing set that supports transaction-based rewinding.
    type [<ReferenceEquality>] 'a TSet =
        private
            { mutable TSetOpt : 'a TSet
              TConfig : TConfig
              HashSet : 'a HashSet
              HashSetOrigin : 'a HashSet
              Logs : 'a Log list
              LogsLength : int }

        static member (>>.) (set : 'a2 TSet, builder : TExpr<unit, 'a2 TSet>) =
            snd' (builder set)

        static member (.>>) (set : 'a2 TSet, builder : TExpr<'a2, 'a2 TSet>) =
            fst' (builder set)

        static member (.>>.) (set : 'a2 TSet, builder : TExpr<'a2, 'a2 TSet>) =
            builder set

    let private commit set =
        let oldSet = set
        let hashSetOrigin = HashSet<'a> (set.HashSetOrigin, set.HashSetOrigin.Comparer)
        List.foldBack (fun log () ->
            match log with
            | Add value -> hashSetOrigin.Add value |> ignore
            | Remove value -> hashSetOrigin.Remove value |> ignore
            | Clear -> hashSetOrigin.Clear ())
            set.Logs ()
        let hashSet = HashSet<'a> (hashSetOrigin, hashSetOrigin.Comparer)
        let set = { set with HashSet = hashSet; HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
        oldSet.TSetOpt <- Unchecked.defaultof<'a TSet>
        set.TSetOpt <- set
        set

    let private compress set =
        let oldSet = set
        let hashSetOrigin = HashSet<'a> (set.HashSet, set.HashSet.Comparer)
        let set = { set with HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
        oldSet.TSetOpt <- Unchecked.defaultof<'a TSet>
        set.TSetOpt <- set
        set

    let private validate2 set =
        lock set.Logs (fun () ->
            match box set.TSetOpt with
            | null -> commit set
            | target ->
                match obj.ReferenceEquals (target, set) with
                | true -> if set.LogsLength > set.HashSet.Count then compress set else set
                | false -> commit set)

    let private update updater set =
        let oldSet = set
        let set = validate2 set
        let set = updater set
        oldSet.TSetOpt <- Unchecked.defaultof<'a TSet>
        set.TSetOpt <- set
        set

    let private validate set =
        if TConfig.isFunctional set.TConfig
        then validate2 set
        else set

    /// Create an TSet containing the element of the given hash.
    let makeFromHashSet (comparer : 'a IEqualityComparer) config (hashSet : 'a HashSet) =
        if TConfig.isFunctional config then
            let set =
                { TSetOpt = Unchecked.defaultof<'a TSet>
                  TConfig = config
                  HashSet = hashSet
                  HashSetOrigin = HashSet<'a> (hashSet, comparer)
                  Logs = []
                  LogsLength = 0 }
            set.TSetOpt <- set
            set
        else
            { TSetOpt = Unchecked.defaultof<'a TSet>
              TConfig = config
              HashSet = hashSet
              HashSetOrigin = HashSet<'a> comparer
              Logs = []
              LogsLength = 0 }

    /// Create a TSet containing the given sequence of values.
    let makeFromSeq<'a> comparer config (items : 'a seq) =
        let hashSet = hashSetPlus comparer items
        makeFromHashSet comparer config hashSet

    /// Create an empty TSet.
    let makeEmpty<'a> comparer config =
        makeFromSeq<'a> comparer config Seq.empty

    /// Get the comparer function used to determine uniqueness in a TSet.
    let getComparer set =
        struct (set.HashSet.Comparer, set)

    /// Get the semantic configuration of the TSet.
    let getConfig set =
        struct (set.TConfig, set)

    /// Add an element to a TSet.
    let add value set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Add value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Add value |> ignore
                set)
                set
        else set.HashSet.Add value |> ignore; set

    /// Remove all matching elements from a TSet.
    let remove value set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Remove value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Remove value |> ignore
                set)
                set
        else set.HashSet.Remove value |> ignore; set

    /// Remove all elements from a TSet.
    let clear set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Clear :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Clear ()
                set)
                set
        else set.HashSet.Clear (); set

    /// Check that a TSet has no elements.
    let isEmpty set =
        let set = validate set
        struct (set.HashSet.Count = 0, set)

    /// Check that a TSet has one or more elements.
    let notEmpty set =
        mapFst' not (isEmpty set)

    /// Get the length of a TSet (constant-time).
    let length set =
        let set = validate set
        struct (set.HashSet.Count, set)

    /// Determine that a TSet contains the given value.
    let contains value set =
        let set = validate set
        struct (set.HashSet.Contains value, set)

    /// Add all the given values to a TSet.
    let addMany values set =
        Seq.fold (flip add) set values

    /// Remove all the given values from a TSet.
    let removeMany values set =
        Seq.fold (flip remove) set values
        
    /// Make a TSet from a sequence of values.
    let ofSeq comparer config values =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty comparer config)
            values

    /// Convert a TSet to a seq. Note that entire set is iterated eagerly since the underlying HashMap could
    /// otherwise opaquely change during iteration.
    let toSeq set =
        let set = validate set
        let seq = set.HashSet |> Array.ofSeq :> 'a seq
        struct (seq, set)

    /// Convert a TSet to a HashSet.
    let toHashSet set =
        let set = validate set
        let result = HashSet<'a> (set.HashSet, set.HashSet.Comparer)
        struct (result, set)

    /// Fold over the elements of a TSet.
    let fold folder state set =
        let struct (seq, set) = toSeq set
        let result = Seq.fold folder state seq
        struct (result, set)

    /// Map the elements of a TSet.
    let map mapper set =
        fold
            (fun set value -> add (mapper value) set)
            (makeEmpty set.HashSet.Comparer set.TConfig)
            set

    /// Filter the elements of a TSet.
    let filter pred set =
        fold
            (fun set value -> if pred value then add value set else set)
            (makeEmpty set.HashSet.Comparer set.TConfig)
            set

    /// Determine equality of two TSets.
    let equals set set2 =
        let (set, set2) = (validate set, validate set2)
        struct (set.HashSet.SetEquals set2.HashSet, set, set2)

    /// Construct a union HashSet.
    let unionFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = HashSet<'a> (set.HashSet, set.HashSet.Comparer)
        result.UnionWith set2.HashSet
        struct (result, set, set2)

    /// Construct an intersection HashSet.
    let intersectFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = HashSet<'a> (set.HashSet, set.HashSet.Comparer)
        result.IntersectWith set2.HashSet
        struct (result, set, set2)

    /// Construct a disjoint HashSet.
    let disjointFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = HashSet<'a> (set.HashSet, set.HashSet.Comparer)
        result.SymmetricExceptWith set2.HashSet
        struct (result, set, set2)

    /// Construct a difference HashSet.
    let differenceFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = HashSet<'a> (set.HashSet, set.HashSet.Comparer)
        result.ExceptWith set2.HashSet
        struct (result, set, set2)

    /// Construct a union TSet.
    let union config set set2 =
        let struct (result, set, set2) = unionFast set set2
        struct (makeFromHashSet set.HashSet.Comparer config result, set, set2)

    /// Construct an intersection TSet.
    let intersect config set set2 =
        let struct (result, set, set2) = intersectFast set set2
        struct (makeFromHashSet set.HashSet.Comparer config result, set, set2)

    /// Construct a disjoint TSet.
    let disjoint config set set2 =
        let struct (result, set, set2) = disjointFast set set2
        struct (makeFromHashSet set.HashSet.Comparer config result, set, set2)

    /// Construct a difference TSet.
    let difference config set set2 =
        let struct (result, set, set2) = differenceFast set set2
        struct (makeFromHashSet set.HashSet.Comparer config result, set, set2)

    /// Make a TSet with a single element.
    let singleton<'a> comparer config (item : 'a) =
        makeFromSeq comparer config [item]

/// A hashing set that supports transaction-based rewinding.
type 'a TSet = 'a TSet.TSet

[<AutoOpen>]
module TSetBuilder =

    /// Build a TSet.
    let tset<'a> = TExprBuilder<'a TSet> ()
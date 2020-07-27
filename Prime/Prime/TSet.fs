// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module TSet =

    type [<NoEquality; NoComparison>] private Log<'a when 'a : equality> =
        | Add of add : 'a
        | Remove of remove : 'a
        | Clear

    type [<NoEquality; NoComparison>] TSet<'a when 'a : equality> =
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
        if List.notEmpty set.Logs then
            let oldSet = set
            let hashSetOrigin = HashSet<'a> (set.HashSetOrigin, HashIdentity.Structural)
            List.foldBack (fun log () ->
                match log with
                | Add value -> hashSetOrigin.TryAdd value |> ignore
                | Remove value -> hashSetOrigin.Remove value |> ignore
                | Clear -> hashSetOrigin.Clear ())
                set.Logs ()
            let hashSet = HashSet<'a> (hashSetOrigin, HashIdentity.Structural)
            let set = { set with HashSet = hashSet; HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
            oldSet.TSetOpt <- Unchecked.defaultof<'a TSet>
            set.TSetOpt <- set
            set
        else set

    let private compress set =
        let oldSet = set
        let hashSetOrigin = HashSet<'a> (set.HashSet, HashIdentity.Structural)
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

    let makeFromHashSet config (hashSet : HashSet<'a>) =
        if TConfig.isFunctional config then
            let set =
                { TSetOpt = Unchecked.defaultof<'a TSet>
                  TConfig = config
                  HashSet = hashSet
                  HashSetOrigin = HashSet<'a> (hashSet, HashIdentity.Structural)
                  Logs = []
                  LogsLength = 0 }
            set.TSetOpt <- set
            set
        else
            { TSetOpt = Unchecked.defaultof<'a TSet>
              TConfig = config
              HashSet = hashSet
              HashSetOrigin = HashSet<'a> HashIdentity.Structural
              Logs = []
              LogsLength = 0 }

    let makeFromSeq<'a when 'a : equality> config (items : 'a seq) =
        let hashSet = hashSetPlus items
        makeFromHashSet config hashSet

    let makeEmpty<'a when 'a : equality> config =
        makeFromSeq<'a> config Seq.empty

    let getConfig set =
        struct (set.TConfig, set)

    let add value set =
        if TConfig.isFunctional set.TConfig then
            update (fun set ->
                let set = { set with Logs = Add value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.TryAdd value |> ignore
                set)
                set
        else set.HashSet.TryAdd value |> ignore; set

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

    /// Get the length of the set (constant-time, obviously).
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

    /// Convert a TSet to a seq. Note that entire set is iterated eagerly since the underlying HashMap could
    /// otherwise opaquely change during iteration.
    let toSeq set =
        let set = validate set
        let seq = set.HashSet |> Array.ofSeq :> 'a seq
        struct (seq, set)

    let fold folder state set =
        let struct (seq, set) = toSeq set
        let result = Seq.fold folder state seq
        struct (result, set)

    let map mapper set =
        fold
            (fun set value -> add (mapper value) set)
            (makeEmpty set.TConfig)
            set

    let filter pred set =
        fold
            (fun set value -> if pred value then add value set else set)
            (makeEmpty set.TConfig)
            set

    let unionFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = HashSet<'a> (set.HashSet, HashIdentity.Structural)
        result.UnionWith set2.HashSet
        struct (result, set, set2)

    let intersectFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = HashSet<'a> (set.HashSet, HashIdentity.Structural)
        result.IntersectWith set2.HashSet
        struct (result, set, set2)

    let disjointFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = HashSet<'a> (set.HashSet, HashIdentity.Structural)
        result.SymmetricExceptWith set2.HashSet
        struct (result, set, set2)

    let differenceFast set set2 =
        let (set, set2) = (validate set, validate set2)
        let result = HashSet<'a> (set.HashSet, HashIdentity.Structural)
        result.ExceptWith set2.HashSet
        struct (result, set, set2)

    let union config set set2 =
        let struct (result, set, set2) = unionFast set set2
        struct (makeFromHashSet config result, set, set2)

    let intersect config set set2 =
        let struct (result, set, set2) = intersectFast set set2
        struct (makeFromHashSet config result, set, set2)

    let disjoint config set set2 =
        let struct (result, set, set2) = disjointFast set set2
        struct (makeFromHashSet config result, set, set2)

    let difference config set set2 =
        let struct (result, set, set2) = differenceFast set set2
        struct (makeFromHashSet config result, set, set2)

type TSet<'a when 'a : equality> = TSet.TSet<'a>

[<AutoOpen>]
module TSetBuilder =

        let tset<'a when 'a : equality> = TExprBuilder<'a TSet> ()
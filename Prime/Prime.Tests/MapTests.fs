// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open System.Collections.Generic
open FsCheck
open FsCheck.NUnit
open Prime
module MapTests =

    type MapAction<'k, 'v> =
        | SetKey of KeyValuePair<'k, 'v>
        | RemoveRandom
        | FoldCombine of 'v

    /// Keeps a reference to all persistent collections returned after
    /// performing an action, and after they are all applied, checks
    /// that they equal what we would get from FSharp.Core.Map
    let eqMapsAfterSteps
        (fsmap : Map<'k, 'v>)
        (testMap : 'm)
        (actions : MapAction<'k, 'v> array)
        (add : 'k->'v->'m->'m when 'k : comparison and 'v : comparison)
        (remove : 'k->'m->'m when 'k : comparison and 'v : comparison)
        (fold : ('m->'k->'v->'m)->'m->'m->'m)
        (combineValues : 'v->'v->'v)
        (eq : 'm->Map<'k, 'v>->bool) =

        let applyAction fsmap testMap action =
            match action with
            | MapAction.SetKey kvp ->
                (Map.add kvp.Key kvp.Value fsmap, add kvp.Key kvp.Value testMap)
            | MapAction.RemoveRandom when Map.isEmpty fsmap ->
                (fsmap, testMap)
            | MapAction.RemoveRandom ->
                let idx = Gen.choose (0, fsmap.Count - 1) |> Gen.sample 0 1 |> List.head
                let ary = Map.toArray fsmap
                let key = fst ary.[idx]
                (Map.remove key fsmap, remove key testMap)
            | MapAction.FoldCombine arg ->
                let newFsmap = Map.fold (fun acc k v -> Map.add k (combineValues v arg) acc) fsmap fsmap
                let newTestMap = fold (fun acc k v -> add k (combineValues v arg) acc) testMap testMap
                (newFsmap, newTestMap)

        let (fsmaps, testMaps) =
            Array.fold
                (fun acc action ->
                    match acc with
                    | (fsmap :: fsmaps, testMap :: testMaps) ->
                        let (newF, newT) = applyAction fsmap testMap action
                        (newF :: fsmap :: fsmaps, newT :: testMap :: testMaps)
                    | _ -> failwithumf ())
                ([fsmap], [testMap])
                actions

        List.forall2 eq testMaps fsmaps

    [<Property (QuietOnSuccess = true)>]
    let hmapsEqualFsmapsAfterSteps (initialMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = HMap.ofSeq (Map.toSeq initialMap)
        let eq (hmap : HMap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq hmap.Pairs = fsmap
        eqMapsAfterSteps initialMap testMap actions HMap.add HMap.remove HMap.fold (+) eq

    [<Property (QuietOnSuccess = true)>]
    let umapsEqualFsmapsAfterSteps (initialMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = UMap.makeFromSeq HashIdentity.Structural Functional (Map.toSeq initialMap)
        let eq (umap : UMap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq umap = fsmap
        eqMapsAfterSteps initialMap testMap actions UMap.add UMap.remove UMap.fold (+) eq

    [<Property (QuietOnSuccess = true)>]
    let sumapsEqualFsmapsAfterSteps (initialMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = SUMap.makeFromSeq HashIdentity.Structural Functional (Map.toSeq initialMap)
        let eq (umap : SUMap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq umap = fsmap
        eqMapsAfterSteps initialMap testMap actions SUMap.add SUMap.remove SUMap.fold (+) eq
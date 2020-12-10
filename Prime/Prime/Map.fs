// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System.Collections.Generic
open Prime

/// Alternative type of map when it is reified to code.
type FSharpMap<'k, 'v when 'k : comparison> = Map<'k, 'v>

[<RequireQualifiedAccess>]
module Map =

    /// Check that a map is not empty.
    let inline notEmpty map =
        not (Map.isEmpty map)

    /// Make a singleton map.
    let inline singleton key value =
        Map.add key value Map.empty
    
    /// Add multiple values to a map.
    let addMany kvps map =
        Seq.fold (fun map (key, value) -> Map.add key value map) map kvps
    
    /// Remove multiple values from a map.
    let removeMany keys map =
        Seq.fold (flip Map.remove) map keys
    
    /// Make a map from a seq by a function.
    let ofSeqBy by seq =
        let pairs = Seq.map by seq
        Map.ofSeq pairs
    
    /// Convert a map to a seq by a function.
    let toSeqBy by map =
        let seq = Map.toSeq map
        Seq.map (fun (k, v) -> by k v) seq
    
    /// Get a seq of a map's keys.
    let toKeySeq map =
        toSeqBy (fun k _ -> k) map
    
    /// Convert a seq of a map's keys by a function.
    let toKeySeqBy by map =
        toSeqBy (fun k _ -> by k) map
    
    /// Get a seq of a map's values.
    let toValueSeq map =
        toSeqBy (fun _ v -> v) map
    
    /// Convert a seq of a map's values by a function.
    let toValueSeqBy by map =
        toSeqBy (fun _ v -> by v) map
    
    /// Make a map from a list by a function.
    let ofListBy by list =
        let pairs = List.map by list
        Map.ofList pairs
    
    /// Convert a map to a list by a function.
    let toListBy by map =
        let list = Map.toList map
        List.map (fun (k, v) -> by k v) list
    
    /// Get a list of a map's keys.
    let toKeyList map =
        toListBy (fun k _ -> k) map
    
    /// Convert a list of a map's keys by a function.
    let toKeyListBy by map =
        toListBy (by << fst) map
    
    /// Get a list of a map's values.
    let toValueList map =
        toListBy (fun _ v -> v) map
    
    /// Convert a list of a map's values by a function.
    let toValueListBy by map =
        toListBy (fun _ v -> by v) map

    /// Index a sequence and convert it to a map.
    let indexed seq =
        seq |>
        Seq.indexed |>
        Map.ofSeq

    /// Map over a map with an index.
    let mapi mapper map =
        let mutable i = 0
        Map.map (fun k v -> let r = mapper i k v in i <- inc i; r) map

    /// Fold over a map with an index.
    let foldi mapper map =
        let mutable i = 0
        Map.fold (fun s k v -> let r = mapper i s k v in i <- inc i; r) map
    
    /// Combine the contents of two maps, taking an item from the second map in the case of a key
    /// conflict.
    let concat map map2 =
        Seq.fold (fun map (kvp : KeyValuePair<_, _>) -> Map.add kvp.Key kvp.Value map) map map2

[<AutoOpen>]
module MapOperators =

    /// Combine the contents of two maps, taking an item from the second map in case of a key overlap.
    let inline (@@) map map2 =
        Map.concat map map2
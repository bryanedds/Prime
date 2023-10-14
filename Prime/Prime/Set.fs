// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System

/// Alternative alias for set when its name is reified.
type FSharpSet<'a when 'a : comparison> = Set<'a>

[<RequireQualifiedAccess>]
module Set =

    /// Check that a set is not empty.
    let inline notEmpty set =
        not (Set.isEmpty set)

    /// Make a singleton set.
    let inline singleton value =
        Set.add value Set.empty

    /// Add multiple values to a set.
    let addMany values set =
        Seq.fold (flip Set.add) set values

    /// Remove multiple values from a set.
    let removeMany values set =
        Seq.fold (flip Set.remove) set values

    /// Make a set from a seq by a function.
    let ofSeqBy by seq =
        let values = Seq.map by seq
        Set.ofSeq values

    /// Convert a set to a seq by a function.
    let toSeqBy by (set : _ Set) =
        Seq.map (fun a -> by a) set

    /// Make a set from a list by a function.
    let ofListBy by seq =
        let values = Seq.map by seq
        Set.ofSeq values

    /// Convert a set to a list by a function.
    let toListBy by (set : _ Set) =
        let values = Seq.map (fun a -> by a) set
        List.ofSeq values

    /// Make a set from an array by a function.
    let ofArrayBy by seq =
        let values = Seq.map by seq
        Array.ofSeq values

    /// Convert a set to a list by a function.
    let toArrayBy by (set : _ Set) =
        let values = Seq.map (fun a -> by a) set
        Array.ofSeq values

    /// Map over a set with an index.
    let mapi mapper map =
        let mutable i = 0
        Set.map (fun a -> let r = mapper i a in i <- inc i; r) map

    /// Fold over a set with an index.
    let foldi mapper map =
        let mutable i = 0
        Set.fold (fun s a -> let r = mapper i s a in i <- inc i; r) map
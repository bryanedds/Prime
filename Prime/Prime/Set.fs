// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open Prime

/// Alternative type of set when its name is reified.
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

    /// Map over a set with an index.
    let mapi mapper map =
        let mutable i = 0
        Set.map (fun a -> let r = mapper i a in i <- inc i; r) map

    /// Fold over a set with an index.
    let foldi mapper map =
        let mutable i = 0
        Set.fold (fun s a -> let r = mapper i s a in i <- inc i; r) map
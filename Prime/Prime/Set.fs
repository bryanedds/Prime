// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

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
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System

/// Allow for a pair of values to be partially-equated.
type [<CustomEquality; NoComparison>] PartialEquatable<'a, 'b when 'a : equality> =
    { Equatable : 'a
      Nonequatable : 'b }

    static member equals left right =
        left.Equatable = right.Equatable

    override this.GetHashCode () =
        hash this.Equatable

    override this.Equals that =
        match that with
        | :? PartialEquatable<'a, 'b> as that -> PartialEquatable<'a, 'b>.equals this that
        | _ -> failwithumf ()

[<RequireQualifiedAccess>]
module PartialEquatable =

    /// Make a partially-equatable value.
    let make equatable nonequatable =
        { Equatable = equatable
          Nonequatable = nonequatable }

    /// Split a partially-equatable value.
    let unmake partialCompare =
        (partialCompare.Equatable, partialCompare.Nonequatable)
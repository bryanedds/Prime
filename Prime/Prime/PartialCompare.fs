// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System

/// Allow for a pair of values to be partially-compared.
type [<CustomEquality; CustomComparison>] PartialComparable<'a, 'b when 'a : comparison> =
    { Comparable : 'a
      Noncomparable : 'b }

    static member equals left right =
        left.Comparable = right.Comparable

    static member compare left right =
        compare left.Comparable right.Comparable

    override this.GetHashCode () =
        hash this.Comparable

    override this.Equals that =
        match that with
        | :? PartialComparable<'a, 'b> as that -> PartialComparable<'a, 'b>.equals this that
        | _ -> failwithumf ()

    interface PartialComparable<'a, 'b> IComparable with
        member this.CompareTo that =
            PartialComparable<'a, 'b>.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? PartialComparable<'a, 'b> as that -> PartialComparable<'a, 'b>.compare this that
            | _ -> failwith "Invalid PartialComparable comparison (comparee not of type PartialComparable)."

[<RequireQualifiedAccess>]
module PartialComparable =

    /// Make a partially-comparable value.
    let make comparable noncomparable =
        { Comparable = comparable
          Noncomparable = noncomparable }

    /// Split a partially-comparable value.
    let unmake partialCompare =
        (partialCompare.Comparable, partialCompare.Noncomparable)
﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime

[<AutoOpen>]
module PairOperators =

    /// Replace pair member fst.
    let inline withFst fst (_, snd) =
        (fst, snd)

    /// Replace pair member snd.
    let inline withSnd snd (fst, _) =
        (fst, snd)

    /// Map over pair member fst.
    let inline mapFst mapper (fst, snd) =
        (mapper fst, snd)

    /// Map over pair member snd.
    let inline mapSnd mapper (fst, snd) =
        (fst, mapper snd)

    /// Make a pair of values.
    let inline pair a b =
        (a, b)

    /// Prepend an item to another build a pair.
    let inline prepend a b =
        (a, b)

    /// Append an item to another build a pair.
    let inline append a b =
        (b, a)

    /// Make a pair from a duplicated value.
    let inline dup a =
        pair a a

    /// Swap the elements of a pair.
    let inline swap (a, b) =
        (b, a)

[<RequireQualifiedAccess>]
module Pair =

    /// Make a pair.
    let inline make a b =
        pair a b
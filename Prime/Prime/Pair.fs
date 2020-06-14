﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

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

[<RequireQualifiedAccess>]
module Pair =

    /// Make a pair.
    let inline make a b =
        pair a b
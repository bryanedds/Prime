// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime

[<AutoOpen>]
module ValuePairOperators =

    /// Get the fst value in a struct pair.
    let inline fst' struct (fst, _) =
        fst

    /// Get the snd value in a struct pair.
    let inline snd' struct (_, snd) =
        snd

    /// Replace struct pair member fst.
    let inline withFst' fst struct (_, snd) =
        struct (fst, snd)

    /// Replace struct pair member snd.
    let inline withSnd' snd struct (fst, _) =
        struct (fst, snd)

    /// Map over struct pair member fst.
    let inline mapFst' mapper struct (fst, snd) =
        struct (mapper fst, snd)

    /// Map over struct pair member snd.
    let inline mapSnd' mapper struct (fst, snd) =
        struct (fst, mapper snd)

    /// Make a struct pair.
    let inline pair' a b =
        struct (a, b)

[<RequireQualifiedAccess>]
module ValuePair =

    /// The first item in a struct pair.
    let fst = fst'
    
    /// The second item in a struct pair.
    let snd = snd'

    /// Replace struct pair member fst.
    let inline withFst fst pair =
        withFst' fst pair

    /// Replace struct pair member snd.
    let inline withSnd snd pair =
        withSnd' snd pair

    /// Map over struct pair member fst.
    let inline mapFst mapper pair =
        mapFst' mapper pair

    /// Map over struct pair member snd.
    let inline mapSnd mapper pair =
        mapSnd' mapper pair

    /// Make a struct pair.
    let inline make a b =
        pair' a b
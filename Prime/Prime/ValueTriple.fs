// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime

[<AutoOpen>]
module ValueTripleOperators =

    /// The first item in a struct triple.
    let inline a__' triple =
        match triple with
        | struct (a, _, _) -> a

    /// The second item in a struct triple.
    let inline _b_' triple =
        match triple with
        | struct (_, b, _) -> b
    
    /// The third item in a struct triple.
    let inline __c' triple =
        match triple with
        | struct (_, _, c) -> c

    /// The first and second items in a struct triple.
    let inline ab_' triple =
        match triple with
        | struct (a, b, _) -> struct (a, b)
    
    /// The first and third items in a struct triple.
    let inline a_c' triple =
        match triple with
        | struct (a, _, c) -> struct (a, c)
    
    /// The second and third items in a struct triple.
    let inline _bc' triple =
        match triple with
        | struct (_, b, c) -> struct (b, c)

    /// Make a struct triple.
    let inline vtriple a b c =
        struct (a, b, c)

[<RequireQualifiedAccess>]
module ValueTriple =

    /// The first item in a struct triple.
    let fst = a__'
    
    /// The second item in a struct triple.
    let snd = _b_'
    
    /// The third item in a struct triple.
    let thd = __c'

    /// Prepend an item to a pair to build a struct triple.
    let inline prepend a struct (b, c) =
        struct (a, b, c)

    /// Insert an item in a pair to build a struct triple.
    let inline insert (b : 'b) struct (a : 'a, c : 'c) =
        struct (a, b, c)

    /// Append an item to a pair to build a struct triple.
    let inline append (c : 'c) struct (a : 'a, b : 'b) =
        struct (a, b, c)

    /// Replace struct triple member a.
    let inline withA (a : 'a) struct (_ : 'd, b : 'b, c : 'c) =
        struct (a, b, c)

    /// Replace struct triple member b.
    let inline withB (b : 'b) struct (a : 'a, _ : 'd, c : 'c) =
        struct (a, b, c)

    /// Replace struct triple member c.
    let inline withC (c : 'c) struct (a : 'a, b : 'b, _ : 'd) =
        struct (a, b, c)

    /// Map over struct triple member a.
    let inline mapA (mapper : 'a -> 'd) struct (a : 'a, b : 'b, c : 'c) =
        struct (mapper a, b, c)

    /// Map over struct triple member b.
    let inline mapB (mapper : 'b -> 'd) struct (a : 'a, b : 'b, c : 'c) =
        struct (a, mapper b, c)

    /// Map over struct triple member c.
    let inline mapC (mapper : 'c -> 'd) struct (a : 'a, b : 'b, c : 'c) =
        struct (a, b, mapper c)

    /// Make a struct triple.
    let inline make a b c =
        struct (a, b, c)
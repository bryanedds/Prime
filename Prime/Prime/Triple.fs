// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime

[<AutoOpen>]
module TripleOperators =

    /// The first item in a triple.
    let inline a__ triple =
        match triple with
        | (a, _, _) -> a
    
    /// The second item in a triple.
    let inline _b_ triple =
        match triple with
        | (_, b, _) -> b
    
    /// The third item in a triple.
    let inline __c triple =
        match triple with
        | (_, _, c) -> c

    /// The first and second items in a triple.
    let inline ab_ triple =
        match triple with
        | (a, b, _) -> (a, b)
    
    /// The first and third items in a triple.
    let inline a_c triple =
        match triple with
        | (a, _, c) -> (a, c)
    
    /// The second and third items in a triple.
    let inline _bc triple =
        match triple with
        | (_, b, c) -> (b, c)

    /// Make a triple.
    let inline triple a b c =
        (a, b, c)

[<RequireQualifiedAccess>]
module Triple =

    /// The first item in a triple.
    let fst = a__
    
    /// The second item in a triple.
    let snd = _b_
    
    /// The third item in a triple.
    let thd = __c

    /// Prepend an item to a pair to build a triple.
    let inline prepend a (b, c) =
        (a, b, c)

    /// Insert an item in a pair to build a triple.
    let inline insert (b : 'b) (a : 'a, c : 'c) =
        (a, b, c)

    /// Append an item to a pair to build a triple.
    let inline append (c : 'c) (a : 'a, b : 'b) =
        (a, b, c)

    /// Replace triple member a.
    let inline withA (a : 'a) (_ : 'd, b : 'b, c : 'c) =
        (a, b, c)

    /// Replace triple member b.
    let inline withB (b : 'b) (a : 'a, _ : 'd, c : 'c) =
        (a, b, c)

    /// Replace triple member c.
    let inline withC (c : 'c) (a : 'a, b : 'b, _ : 'd) =
        (a, b, c)

    /// Map over triple member a.
    let inline mapA (mapper : 'a -> 'd) (a : 'a, b : 'b, c : 'c) =
        (mapper a, b, c)

    /// Map over triple member b.
    let inline mapB (mapper : 'b -> 'd) (a : 'a, b : 'b, c : 'c) =
        (a, mapper b, c)

    /// Map over triple member c.
    let inline mapC (mapper : 'c -> 'd) (a : 'a, b : 'b, c : 'c) =
        (a, b, mapper c)

    /// Make a triple.
    let inline make a b c =
        (a, b, c)
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime

[<AutoOpen>]
module Generics =

    /// The generic zero value.
    let inline zero () = LanguagePrimitives.GenericZero

    /// The generic one value.
    let inline one () = LanguagePrimitives.GenericOne

    /// Check a value for equality with zero.
    let inline isZero a = a = zero ()

    /// Check a value for non-equality with zero.
    let inline notZero a = a <> zero ()

    /// The generic monoidal append operation.
    let inline append a b = a + b

    /// Generic (and sectioned) increment.
    let inline inc n = n + one ()

    /// Generic (and sectioned) decrement.
    let inline dec n = n + -(one ())

    /// Generic (and sectioned) addition.
    let inline add a b = a + b

    /// Generic (and sectioned) subtraction.
    let inline sub a b = a - b

    /// Generic (and sectioned) multiplication.
    let inline mul a b = a * b

    /// Generic (and sectioned) division.
    let inline div a b = a / b
    
    /// The modulus (not remainder!) operator.
    let inline modulus a b =
        let remainder = a % b
        if remainder < zero ()
        then remainder + b
        else remainder

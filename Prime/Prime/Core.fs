// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System

/// Denotes that a value should not be directly mutated by the consumer.
type [<AttributeUsage (AttributeTargets.Field)>] UniformAttribute () =
    inherit Attribute ()
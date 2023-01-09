// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System

/// Denotes that a value should not be altered by the consumer of the API.
type [<AttributeUsage (AttributeTargets.Field)>] UniformAttribute () =
    inherit Attribute ()
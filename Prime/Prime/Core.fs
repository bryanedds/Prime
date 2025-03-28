﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System

/// Denotes that a value should not be altered by the consumer of the API.
type [<AttributeUsage (AttributeTargets.Field)>] UniformAttribute () =
    inherit Attribute ()

/// An attribute to specify the default value of a property.
type [<AttributeUsage (AttributeTargets.Enum ||| AttributeTargets.Struct ||| AttributeTargets.Class); AllowNullLiteral>]
    DefaultValueAttribute (defaultValue : obj) =
    inherit Attribute ()
    member this.DefaultValue = defaultValue
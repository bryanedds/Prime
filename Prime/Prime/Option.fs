// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime

/// Alternative type of option when its name is reified.
type 'a FSharpOption = 'a option

[<RequireQualifiedAccess>]
module Option =

    /// Map an option's value, or missing that, return a default value.
    let mapOrDefaultValue mapper aDefault opt =
        match opt with
        | Some value -> mapper value
        | None -> aDefault

    /// The number of values contained by an option.
    let length opt =
        match opt with
        | Some _ -> 1
        | None -> 0
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime

/// Alternative type of option when its name is reified.
type 'a FSharpOption = 'a option

[<RequireQualifiedAccess>]
module Option =

    /// Get an option's value, or missing that, return a default value.
    let getOrDefault aDefault opt =
        match opt with
        | Some value -> value
        | None -> aDefault

    /// Map an option's value, or missing that, return a default value.
    let mapOrDefault mapper aDefault opt =
        match opt with
        | Some value -> mapper value
        | None -> aDefault

    /// Remove a value if contained by an option.
    let remove value opt =
        match opt with
        | Some value2 -> if value2 = value then None else opt
        | None -> None

    /// The number of value contained by an option.
    let length opt =
        match opt with
        | Some _ -> 1
        | None -> 0

    /// Concatenate a sequence of options.
    let concat opts =
        match Seq.tryFind Option.isSome opts with
        | Some _ as opt -> opt
        | None -> None

    /// Concatenate two options, prefering leftOpt over rightOpt.
    let concat2 leftOpt rightOpt =
        match leftOpt with
        | Some _ -> leftOpt
        | None -> rightOpt
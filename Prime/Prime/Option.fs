// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime

/// Alternative type of option when its name is reified.
type 'a FSharpOption = 'a option

[<RequireQualifiedAccess>]
module Option =

    /// Convert a ValueOption to an Option.
    let ofValueOption<'a> (opt : 'a voption) =
        match opt with
        | ValueSome a -> Some a
        | ValueNone -> None

    /// Convert an Option to a ValueOption.
    let toValueOption<'a> (opt : 'a option) =
        match opt with
        | Some a -> ValueSome a
        | None -> ValueNone

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

[<AutoOpen>]
module OptionExtension =

    /// Option extension methods.
    type 'a Option with

        /// The value of an option or the given default value.
        member this.Default defaultValue =
            match this with
            | Some value -> value
            | None -> defaultValue
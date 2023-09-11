// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime

/// Alternative type of option when its name is reified.
type 'a FSharpValueOption = 'a voption

[<RequireQualifiedAccess>]
module ValueOption =

    /// Convert an Option to a ValueOption.
    let ofOption<'a> (opt : 'a option) =
        match opt with
        | Some a -> ValueSome a
        | None -> ValueNone

    /// Convert a ValueOption to an Option.
    let toOption<'a> (opt : 'a voption) =
        match opt with
        | ValueSome a -> Some a
        | ValueNone -> None

[<AutoOpen>]
module ValueOptionExtension =

    /// ValueOption extension methods.
    type 'a ValueOption with

        /// The value of a voption or the given default value.
        member this.Default defaultValue =
            match this with
            | ValueSome value -> value
            | ValueNone -> defaultValue
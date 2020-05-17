// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System.ComponentModel
open System.Text.RegularExpressions
open Prime

/// Converts Rexpr types.
type RexprConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = typeof<Rexpr>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            string source :> obj
        elif destType = typeof<Symbol> then
            let rexpr = source :?> Rexpr
            Text (string rexpr, None) :> obj
        elif destType = typeof<Rexpr> then source
        else failconv "Invalid RexprConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Rexpr>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as str -> scvalue<Rexpr> str :> obj
        | :? Symbol as symbol ->
            match symbol with
            | Atom (pattern, _) | Text (pattern, _) -> Rexpr pattern :> obj
            | Number (_, _) | Quote (_, _) | Symbols (_, _) -> failconv "Expected Symbol or String for conversion to Rexpr." (Some symbol)
        | :? Rexpr -> source
        | _ -> failconv "Invalid RexprConverter conversion from source." None

/// Effectively new-types the Regex type to implement custom type-conversation without needing
/// explicit initialization by the client program.
and [<NoEquality; NoComparison; TypeConverter (typeof<RexprConverter>)>] Rexpr (pattern) =
    inherit Regex (pattern)
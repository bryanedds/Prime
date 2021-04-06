// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections.Generic
open Prime

[<AutoOpen>]
module SymbolicOperators =

    let private SCValueMemo = Dictionary<string, obj> StringComparer.Ordinal
    let private SCStringMemo = Dictionary<obj, string> HashIdentity.Structural

    /// Convert a value to a symbol.
    let valueToSymbol<'a> (value : 'a) =
        let ty = if isNull (value :> obj) then typeof<'a> else getType value
        let converter = SymbolicConverter (false, None, ty)
        converter.ConvertTo (value, typeof<Symbol>) :?> Symbol

    /// Convert a symbol to a value.
    let symbolToValue<'a> (symbol : Symbol) : 'a =
        let converter = SymbolicConverter (false, None, typeof<'a>)
        converter.ConvertFrom symbol :?> 'a

    /// Uses a symbolic converter to convert a value to a string.
    let scstring2<'a> printing (value : 'a) =
        let ty = if isNull (value :> obj) then typeof<'a> else getType value
        if ty.IsPrimitive then
            // OPTIMIZATION: avoid symbolic conversion for primitives
            value.ToString ()
        else
            let converter = SymbolicConverter (printing, None, ty)
            converter.ConvertToString value

    /// Uses a symbolic converter to convert a value to a string.
    let scstring<'a> value =
        scstring2<'a> false value

    /// Uses a symbolic converter to convert a string to a value.
    let scvalue2<'a> printing (str : string) : 'a =
        let converter = SymbolicConverter (printing, None, typeof<'a>)
        converter.ConvertFromString str :?> 'a

    /// Uses a symbolic converter to convert a string to a value.
    let scvalue<'a> str =
        scvalue2<'a> false str

    /// Get the default value of type 'a taking into account DefaultValue decorations.
    let scdefaultof<'a> () : 'a =
        let defaultPropertyType = typeof<'a>
        let defaultValueAttributeOpt =
            defaultPropertyType.GetCustomAttributes (typeof<DefaultValueAttribute>, true) |>
            Array.map (fun attr -> attr :?> DefaultValueAttribute) |>
            Array.tryHead
        match defaultValueAttributeOpt with
        | Some defaultValueAttribute ->
            match defaultValueAttribute.DefaultValue with
            | :? 'a as defaultValue -> defaultValue
            | _ as defaultValue ->
                let defaultValueType = defaultValue.GetType ()
                let converter = SymbolicConverter (false, None, defaultValueType)
                if converter.CanConvertFrom defaultPropertyType
                then converter.ConvertFrom defaultValue :?> 'a
                else failwith ("Cannot convert '" + scstring defaultValue + "' to type '" + defaultPropertyType.Name + "'.")
        | None -> Unchecked.defaultof<'a>

    /// Convert a symbolic string to a value, memoizing the result.
    let scvaluem<'a> str : 'a =
        match SCValueMemo.TryGetValue str with
        | (true, value) -> value :?> 'a
        | (false, _) ->
            let value = scvalue<'a> str
            SCValueMemo.Add (str, value)
            value

    /// Convert a value to symbolic string, memoizing the result.
    let scstringm<'a> (value : 'a) =
        match SCStringMemo.TryGetValue (value :> obj) with
        | (true, str) -> str
        | (false, _) ->
            let str = scstring<'a> value
            SCStringMemo.Add (value, str)
            str

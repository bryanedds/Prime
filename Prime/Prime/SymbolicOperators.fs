// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open System.Collections.Concurrent

[<AutoOpen>]
module SymbolicOperators =

    let private SymbolToValueMemo = ConcurrentDictionary<struct (Type * Symbol), obj> HashIdentity.Structural
    let private ValueToSymbolMemo = ConcurrentDictionary<struct (Type * obj), Symbol> HashIdentity.Structural
    let private SCValueMemo = ConcurrentDictionary<struct (Type * string), obj> HashIdentity.Structural
    let private SCStringMemo = ConcurrentDictionary<struct (Type * obj), string> HashIdentity.Structural

    /// Convert a value to a value of the given type using symbolic conversion.
    /// Thread-safe.
    let valueToValue<'a, 'b> (value : 'a) : 'b =
        let tyA = typeof<'a>
        let tyB = typeof<'b>
        let converterA = SymbolicConverter tyA
        let converterB = SymbolicConverter tyB
        let symbol = converterA.ConvertTo (value, typeof<Symbol>)
        converterB.ConvertFrom symbol :?> 'b

    /// Convert a value to a symbol.
    /// Thread-safe.
    let valueToSymbolPlus<'a> printing toSymbolMemoOpt (value : 'a) =
        let ty = getType<'a> value
        let converter = SymbolicConverter (printing, None, ty, ?toSymbolMemoOpt = toSymbolMemoOpt)
        converter.ConvertTo (value, typeof<Symbol>) :?> Symbol

    /// Convert a value to a symbol, memoizing the result.
    /// Thread-safe.
    let valueToSymbolMemo<'a> value =
        valueToSymbolPlus<'a> false (Some ValueToSymbolMemo) value

    /// Convert a value to a symbol.
    /// Thread-safe.
    let valueToSymbol<'a> value =
        valueToSymbolPlus<'a> false None value

    /// Convert a symbol to a value.
    /// NOTE: be cautious when dealing with mutable values if memoizing since they are cached and therefore aliased!
    /// Thread-safe with immutable values.
    let symbolToValuePlus<'a> printing ofSymbolMemoOpt (symbol : Symbol) : 'a =
        let converter = SymbolicConverter (printing, None, typeof<'a>, ?ofSymbolMemoOpt = ofSymbolMemoOpt)
        converter.ConvertFrom symbol :?> 'a

    /// Convert a symbol to a value, memoizing the result.
    /// NOTE: be cautious when dealing with mutable values since they are cached and therefore aliased!
    /// Thread-safe with immutable values.
    let symbolToValueMemo<'a> symbol =
        symbolToValuePlus<'a> false (Some SymbolToValueMemo) symbol

    /// Convert a symbol to a value.
    /// Thread-safe.
    let symbolToValue<'a> symbol =
        symbolToValuePlus<'a> false None symbol

    /// Uses a symbolic converter to convert a value to a string.
    /// Thread-safe.
    let scstringPlus<'a> printing toSymbolMemoOpt (value : 'a) =
        let ty = getType<'a> value
        if ty.IsPrimitive then
            // OPTIMIZATION: avoid symbolic conversion for primitives
            value.ToString ()
        else
            let converter = SymbolicConverter (printing, None, ty, ?toSymbolMemoOpt = toSymbolMemoOpt)
            converter.ConvertToString value

    /// Convert a value to symbolic string, memoizing the result.
    /// Thread-safe.
    let scstringMemo<'a> (value : 'a) =
        match SCStringMemo.TryGetValue struct (typeof<'a>, value :> obj) with
        | (true, str) -> str
        | (false, _) ->
            let str = scstringPlus<'a> false (Some ValueToSymbolMemo) value
            SCStringMemo.[struct (typeof<'a>, value)] <- str
            str

    /// Uses a symbolic converter to convert a value to a string.
    /// Thread-safe.
    let scstring<'a> value =
        scstringPlus<'a> false None value

    /// Uses a symbolic converter to convert a string to a value.
    /// NOTE: be cautious when dealing with mutable values if memoizing since they are cached and therefore aliased!
    /// Thread-safe with immutable values.
    let scvaluePlus<'a> printing ofSymbolMemoOpt str =
        let converter = SymbolicConverter (printing, None, typeof<'a>, ?ofSymbolMemoOpt = ofSymbolMemoOpt)
        converter.ConvertFromString str :?> 'a

    /// Convert a symbolic string to a value, memoizing the result.
    /// NOTE: be cautious when dealing with mutable values since they are cached and therefore aliased!
    /// Thread-safe with immutable values.
    let scvalueMemo<'a> str : 'a =
        match SCValueMemo.TryGetValue struct (typeof<'a>, str) with
        | (true, value) -> value :?> 'a
        | (false, _) ->
            let value = scvaluePlus<'a> false (Some SymbolToValueMemo) str
            SCValueMemo.[struct (typeof<'a>, str)] <- value
            value

    /// Uses a symbolic converter to convert a string to a value.
    /// Thread-safe.
    let scvalue<'a> str =
        scvaluePlus<'a> false None str

    /// Get the default value of type 'a taking into account DefaultValue decorations.
    /// Thread-safe.
    let scdefaultof<'a> () : 'a =
        let defaultPropertyType = typeof<'a>
        let defaultValueAttributeOpt =
            defaultPropertyType.GetCustomAttributes (typeof<DefaultValueAttribute>, true)
            |> Array.map (fun attr -> attr :?> DefaultValueAttribute)
            |> Array.tryHead
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
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open FSharp.Reflection

// TODO: implement symbolic conversion for (S)UList, (S)USet, and (S)UMap.
// TODO: also for Queue and Deque.
// TODO: also for OSet and OMap.

/// Expands a record so that its fields are named, preserving field names exactly when specified.
type SymbolicExpansionAttribute (prettifyFieldNames : bool) =
    inherit Attribute ()
    new () = SymbolicExpansionAttribute true
    member this.PrettifyFieldNames = prettifyFieldNames

/// Compresses two unions into a single union in a symbolic-expression.
type [<StructuralEquality; StructuralComparison>] SymbolicCompression<'a, 'b> =
    | SymbolicCompressionA of 'a
    | SymbolicCompressionB of 'b

/// Converts values to and from symbols and symbolic strings.
type SymbolicConverter (printing : bool, designTypeOpt : Type option, pointType : Type, ?toSymbolMemoOpt : IDictionary<obj, Symbol>, ?ofSymbolMemoOpt : IDictionary<Symbol, obj>) =
    inherit TypeConverter ()

    let padWithDefaults (types : Type array) (values : obj array) =
        if values.Length < types.Length then
            let valuesPadded =
                types |>
                Array.skip values.Length |>
                Array.map (fun ty ->
                    match ty.TryGetDefaultValue () with
                    | Some value -> value
                    | None -> failconv ("Cannot create default value for type '" + ty.Name + "'.") None) |>
                Array.append values
            valuesPadded
        else values

    let padWithDefaultProperties (fieldInfos : PropertyInfo array) (values : obj array) =
        padWithDefaults (Array.map (fun (info : PropertyInfo) -> info.PropertyType) fieldInfos) values

    let rec toSymbolInternal (sourceType : Type) (source : obj) =
        match sourceType.TryGetCustomTypeConverter () with
        | Some typeConverter ->

            // symbolize user-defined type
            if not (typeConverter.CanConvertTo typeof<Symbol>)
            then failconv ("Cannot convert type '" + getTypeName source + "' to Prime.Symbol.") None
            else typeConverter.ConvertTo (source, typeof<Symbol>) :?> Symbol

        | None ->

            // symbolize .NET primitive
            if sourceType.IsPrimitive then
                let converted = (TypeDescriptor.GetConverter sourceType).ConvertTo (source, typeof<string>) :?> string
                if sourceType = typeof<bool> then Atom (converted, ValueNone)
                elif sourceType = typeof<char> then Text (converted, ValueNone)
                else Number (converted, ValueNone)

            // symbolize string
            elif sourceType = typeof<string> then
                let sourceStr = string source
                if printing && sourceType = typeof<string> || Symbol.shouldBeExplicit sourceStr then Text (sourceStr, ValueNone)
                elif Symbol.isNumber sourceStr then Number (sourceStr, ValueNone)
                else Atom (sourceStr, ValueNone)

            // symbolize Symbol (no transformation)
            elif sourceType = typeof<Symbol> then
                source :?> Symbol

            // symbolize KeyValuePair
            elif sourceType.Name = typedefof<KeyValuePair<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let kvp = Reflection.objToKeyValuePair source
                let keySymbol = toSymbol gargs.[0] kvp.Key
                let valueSymbol = toSymbol gargs.[1] kvp.Value
                Symbols ([keySymbol; valueSymbol], ValueNone)

            // symbolize DesignerProperty
            elif sourceType = typeof<DesignerProperty> then
                let property = source :?> DesignerProperty
                let nameString = Text (property.DesignerType.AssemblyQualifiedName, ValueNone)
                let valueSymbol = toSymbol property.DesignerType property.DesignerValue
                if Option.isSome designTypeOpt then valueSymbol
                else Symbols ([nameString; valueSymbol], ValueNone)

            // symbolize array
            elif sourceType.IsArray then
                let items = Reflection.objToObjList source
                let symbols = List.map (toSymbol (sourceType.GetElementType ())) items
                Symbols (symbols, ValueNone)

            // symbolize unit
            elif sourceType.Name = typeof<unit>.Name then
                Symbols ([], ValueNone)

            // symbolize list
            elif sourceType.Name = typedefof<_ list>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = Reflection.objToObjList source
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, ValueNone)

            // symbolize Set
            elif sourceType.Name = typedefof<_ Set>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = Reflection.objToComparableSet source |> List.ofSeq
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, ValueNone)

            // symbolize Map
            elif sourceType.Name = typedefof<Map<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let itemType = typedefof<KeyValuePair<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
                let items = Reflection.objToObjList source
                let symbols = List.map (toSymbol itemType) items
                Symbols (symbols, ValueNone)

            // symbolize Queue
            elif sourceType.Name = typedefof<_ Queue>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = Reflection.objToObjSeq source |> List.ofSeq
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, ValueNone)

            // symbolize Deque
            elif sourceType.Name = typedefof<_ Deque>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = Reflection.objToObjSeq source |> List.ofSeq
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, ValueNone)

            // symbolize SymbolicCompression
            elif sourceType.Name = typedefof<SymbolicCompression<_, _>>.Name then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                let value = unionFields.[0]
                let valueType = value.GetType ()
                if unionCase.Tag = 0 then toSymbol valueType value
                else
                    let (_, unionFields) = FSharpValue.GetUnionFields (value, valueType)
                    let value = unionFields.[0]
                    let valueType = value.GetType ()
                    toSymbol valueType value

            // symbolize Tuple
            elif FSharpType.IsTuple sourceType then
                let tupleFields = FSharpValue.GetTupleFields source
                let tupleElementTypes = FSharpType.GetTupleElements sourceType
                let tupleFieldSymbols = Array.mapi (fun i tupleField -> toSymbol tupleElementTypes.[i] tupleField) tupleFields
                Symbols (List.ofArray tupleFieldSymbols, ValueNone)

            // symbolize Record
            elif FSharpType.IsRecord sourceType || FSharpType.isRecordAbstract sourceType then
                if sourceType.IsDefined (typeof<SymbolicExpansionAttribute>, true) then
                    let expansionAttribute = sourceType.GetCustomAttribute<SymbolicExpansionAttribute> true
                    let recordFieldInfos = FSharpType.GetRecordFields (sourceType, true)
                    let recordFields = Array.map (fun info -> (info, FSharpValue.GetRecordField (source, info))) recordFieldInfos
                    let recordFieldSymbols =
                        Array.map (fun (info : PropertyInfo, field) ->
                            let fieldName =
                                if expansionAttribute.PrettifyFieldNames then
                                    if info.Name.EndsWith "_"
                                    then info.Name.Substring (0, dec info.Name.Length)
                                    else String.capitalize info.Name
                                else info.Name
                            Symbols ([Atom (fieldName, ValueNone); toSymbol info.PropertyType field], ValueNone))
                            recordFields
                    Symbols (List.ofArray recordFieldSymbols, ValueNone)
                else
                    let recordFields = FSharpValue.GetRecordFields (source, true);
                    let recordFieldTypes = FSharpType.GetRecordFields (sourceType, true);
                    let recordFieldSymbols = Array.mapi (fun i recordField -> toSymbol recordFieldTypes.[i].PropertyType recordField) recordFields
                    Symbols (List.ofArray recordFieldSymbols, ValueNone)

            // symbolize Union
            elif FSharpType.IsUnion sourceType || FSharpType.isUnionAbstract sourceType then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                let unionFieldInfos = unionCase.GetFields ()
                if not (Array.isEmpty unionFields) then
                    let unionFieldSymbols = Array.mapi (fun i unionField -> toSymbol unionFieldInfos.[i].PropertyType unionField) unionFields
                    let unionSymbols = Array.cons (Atom (unionCase.Name, ValueNone)) unionFieldSymbols
                    Symbols (List.ofArray unionSymbols, ValueNone)
                else Atom (unionCase.Name, ValueNone)

            // symbolize vanilla .NET type
            else
                let typeConverter = TypeDescriptor.GetConverter sourceType
                match typeConverter with
                | :? DateTimeOffsetConverter ->
                    // HACK: we do not want to use this converter here as it strips the time when converting to string!
                    let dateTimeOffsetStr = string source
                    Text (dateTimeOffsetStr, ValueNone)
                | _ ->
                    if typeConverter.CanConvertTo typeof<Symbol>
                    then typeConverter.ConvertTo (source, typeof<Symbol>) :?> Symbol
                    else (typeConverter.ConvertTo (source, typeof<string>) :?> string, ValueNone) |> Atom

    and toSymbol sourceType source =
        match toSymbolMemoOpt with
        | Some toSymbolMemo when notNull source ->
            match toSymbolMemo.TryGetValue source with
            | (false, _) ->
                let symbol = toSymbolInternal sourceType source
                toSymbolMemo.[source] <- symbol
                symbol
            | (true, symbol) -> symbol
        | _ -> toSymbolInternal sourceType source

    let toString (sourceType : Type) (source : obj) =
        let symbol = toSymbol sourceType source
        Symbol.toString symbol

    let rec ofSymbolInternal (destType : Type) (symbol : Symbol) =

        // desymbolize .NET primitive
        if destType.IsPrimitive then
            match symbol with
            | Atom (str, _) | Text (str, _) ->
                try (TypeDescriptor.GetConverter destType).ConvertFromString str
                with :? FormatException as exn -> failconv ("Failed to convert Atom '" + str + "' to " + destType.Name + " due to: " + scstring exn)
            | Number (str, _) ->
                // allow for numbers with single-character suffixes
                let trimmed =
                    if Char.IsLetter str.[str.Length - 1]
                    then str.Substring (0, str.Length - 1)
                    else str
                try (TypeDescriptor.GetConverter destType).ConvertFromString trimmed
                with :? FormatException as exn -> failconv ("Failed to convert Number '" + str + "' to " + destType.Name + " due to: " + scstring exn)
            | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Symbol, Number, or String for conversion to .NET primitive." (Some symbol)

        // desymbolize string
        elif destType = typeof<string> then
            match symbol with
            | Atom (str, _) ->
                if Symbol.isExplicit str
                then str.Substring (1, str.Length - 2) :> obj
                else str :> obj
            | Number (str, _) ->
                str :> obj
            | Text (str, _) ->
                if printing
                then Symbol.OpenStringStr + Symbol.distill str + Symbol.CloseStringStr :> obj
                else str :> obj
            | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Symbol, Number, or String for conversion to string." (Some symbol)

        // desymbolize Symbol (no tranformation)
        elif destType = typeof<Symbol> then
            symbol :> obj

        // desymbolize other...
        else
            
            match destType.TryGetCustomTypeConverter () with
            | Some typeConverter ->

                // desymbolize user-defined type
                if typeConverter.CanConvertFrom typeof<Symbol>
                then typeConverter.ConvertFrom symbol
                else failconv ("Expected ability to convert from Symbol for custom type converter '" + getTypeName typeConverter + "'.") (Some symbol)

            | None ->

                // desymbolize DesignerProperty
                if destType = typeof<DesignerProperty> then
                    match (designTypeOpt, symbol) with
                    | (Some ty, valueSymbol) ->
                        let value = ofSymbol ty valueSymbol
                        let property = { DesignerType = ty; DesignerValue = value }
                        property :> obj
                    | (None, Symbols ([Text (aqTypeName, _); valueSymbol], _)) ->
                        let ty = Type.GetType aqTypeName
                        let value = ofSymbol ty valueSymbol
                        let property = { DesignerType = ty; DesignerValue = value }
                        property :> obj
                    | _ ->
                        failconv "Expected Symbols containing an assembly-qualified type name String and a symbol value." (Some symbol)

                // desymbolize array
                elif destType.IsArray then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let elements = List.map (ofSymbol (destType.GetElementType ())) symbols
                        Reflection.objsToArray destType elements
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to array." (Some symbol)

                // desymbolize unit
                elif destType = typeof<unit> then
                    match symbol with
                    | Symbols ([], _) -> () :> obj
                    | _ -> failconv "Expected empty Symbols for conversion to unit." (Some symbol)

                // desymbolize list
                elif destType.Name = typedefof<_ list>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (ofSymbol elementType) symbols
                        Reflection.objsToList destType elements
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to list." (Some symbol)

                // desymbolize Set
                elif destType.Name = typedefof<_ Set>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (ofSymbol elementType) symbols
                        Reflection.objsToSet destType elements
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to Set." (Some symbol)

                // desymbolize Map
                elif destType.Name = typedefof<Map<_, _>>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        match gargs with
                        | [|fstType; sndType|] ->
                            let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                            let pairs = List.map (ofSymbol pairType) symbols
                            Reflection.pairsToMap destType pairs
                        | _ -> failwithumf ()
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to Map." (Some symbol)

                // desymbolize Queue
                elif destType.Name = typedefof<_ Queue>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (ofSymbol elementType) symbols
                        Reflection.objsToCollection typedefof<_ Queue>.Name destType elements
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to Set." (Some symbol)

                // desymbolize Deque
                elif destType.Name = typedefof<_ Deque>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (ofSymbol elementType) symbols
                        Reflection.objsToCollection typedefof<_ Deque>.Name destType elements
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to Set." (Some symbol)

                // desymbolize SymbolicCompression
                elif destType.Name = typedefof<SymbolicCompression<_, _>>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        match symbols with
                        | (Atom (symbolHead, _)) :: _ ->
                            let gargs = destType.GetGenericArguments ()
                            let aType = gargs.[0]
                            let aCases = FSharpType.GetUnionCases aType
                            match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = symbolHead) aCases with
                            | Some aCase ->
                                let a = ofSymbol aCase.DeclaringType symbol
                                let compressionUnion = (FSharpType.GetUnionCases destType).[0]
                                FSharpValue.MakeUnion (compressionUnion, [|a|])
                            | None ->
                                let bType = gargs.[1]
                                let b = ofSymbol bType symbol
                                let compressionUnion = (FSharpType.GetUnionCases destType).[1]
                                FSharpValue.MakeUnion (compressionUnion, [|b|])
                        | _ -> failconv "Expected Atom value for SymbolicCompression union name." (Some symbol)
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to SymbolicCompression." (Some symbol)

                // desymbolize Tuple
                elif FSharpType.IsTuple destType then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let elementTypes = FSharpType.GetTupleElements destType
                        let elements =
                            symbols |>
                            Array.ofList |>
                            Array.tryTake elementTypes.Length |>
                            Array.mapi (fun i elementSymbol -> ofSymbol elementTypes.[i] elementSymbol)
                        let elements = padWithDefaults elementTypes elements
                        FSharpValue.MakeTuple (elements, destType)
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to Tuple." (Some symbol)

                // desymbolize Record
                elif FSharpType.IsRecord destType || FSharpType.isRecordAbstract destType then
                    match symbol with
                    | Symbols (symbols, _) ->
                        if destType.IsDefined (typeof<SymbolicExpansionAttribute>, true) then
                            let expansionAttribute = destType.GetCustomAttribute<SymbolicExpansionAttribute> true                            
                            let fieldInfos = FSharpType.GetRecordFields (destType, true)
                            if List.forall (function Symbols ([Atom _; _], _) -> true | _ -> false) symbols then
                                let fieldMap =
                                    symbols |>
                                    List.map (function Symbols ([Atom (fieldName, _); fieldSymbol], _) -> (fieldName, fieldSymbol) | _ -> failwithumf ()) |>
                                    Map.ofList
                                let fields =
                                    Array.map (fun (info : PropertyInfo) ->
                                        match Map.tryFind info.Name fieldMap with
                                        | Some fieldSymbol -> ofSymbol info.PropertyType fieldSymbol
                                        | None ->
                                            if expansionAttribute.PrettifyFieldNames then
                                                match Map.tryFind (info.Name.Substring (0, dec info.Name.Length)) fieldMap with
                                                | Some fieldSymbol -> ofSymbol info.PropertyType fieldSymbol
                                                | None ->
                                                    match Map.tryFind (String.uncapitalize info.Name) fieldMap with
                                                    | Some fieldSymbol -> ofSymbol info.PropertyType fieldSymbol
                                                    | None -> info.PropertyType.GetDefaultValue ()
                                            else info.PropertyType.GetDefaultValue ())
                                        fieldInfos
                                FSharpValue.MakeRecord (destType, fields, true)
                            else failconv "Expected Symbols in pairs for expanded Record" (Some symbol)
                        else
                            let fieldInfos = FSharpType.GetRecordFields (destType, true)
                            let fields =
                                symbols |>
                                Array.ofList |>
                                Array.tryTake fieldInfos.Length |>
                                Array.mapi (fun i fieldSymbol -> ofSymbol fieldInfos.[i].PropertyType fieldSymbol)
                            let fields = padWithDefaultProperties fieldInfos fields
                            FSharpValue.MakeRecord (destType, fields, true)
                    | Atom (_, _) | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to unexpanded Record." (Some symbol)

                // desymbolize Union
                elif FSharpType.IsUnion destType || FSharpType.isUnionAbstract destType && destType <> typeof<string list> then
                    let unionCases = FSharpType.GetUnionCases (destType, true)
                    match symbol with
                    | Atom (unionName, _) ->
                        match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases with
                        | Some unionCase -> FSharpValue.MakeUnion (unionCase, [||], true)
                        | None ->
                            let unionNames = unionCases |> Array.map (fun unionCase -> unionCase.Name) |> String.concat " | "
                            failconv ("Expected one of the following Atom values for Union name: '" + unionNames + "'.") (Some symbol)
                    | Symbols (symbols, _) ->
                        match symbols with
                        | (Atom (symbolHead, _)) :: symbolTail ->
                            let unionName = symbolHead
                            match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases with
                            | Some unionCase ->
                                let unionFieldInfos = unionCase.GetFields ()
                                let unionValues =
                                    symbolTail |>
                                    Array.ofList |>
                                    Array.tryTake unionFieldInfos.Length |>
                                    Array.mapi (fun i unionSymbol -> ofSymbol unionFieldInfos.[i].PropertyType unionSymbol)
                                let unionValues = padWithDefaultProperties unionFieldInfos unionValues
                                FSharpValue.MakeUnion (unionCase, unionValues, true)
                            | None ->
                                let unionNames = unionCases |> Array.map (fun unionCase -> unionCase.Name) |> String.concat " | "
                                failconv ("Expected one of the following Atom values for Union name: '" + unionNames + "'.") (Some symbol)
                        | (Number (_, _) | Text (_, _) | Quote (_, _) | Symbols (_, _)) :: _ ->
                            failconv "Expected Atom value for Union name." (Some symbol)
                        | [] ->
                            failconv "Expected Atom value for Union name." (Some symbol)
                    | Number (_, _) | Text (_, _) | Quote (_, _) ->
                        failconv "Expected Atom or Symbols value for conversion to Union." (Some symbol)

                // desymbolize vanilla .NET type
                else
                    match symbol with
                    | Atom (str, _) | Number (str, _) | Text (str, _) ->
                        try (TypeDescriptor.GetConverter destType).ConvertFromString str
                        with _ -> failconv ("Cannot convert from string '" + str + "' to vanilla .NET object of type '" + destType.Name + "'.") (Some symbol)
                    | Quote (_, _) | Symbols (_, _) ->
                        failconv ("Expected Atom, Number, or String value for conversion to vanilla .NET object of type '" + destType.Name + "'.") (Some symbol)

    and ofSymbol destType symbol =
        match ofSymbolMemoOpt with
        | Some ofSymbolMemo ->
            match ofSymbolMemo.TryGetValue symbol with
            | (false, _) ->
                let result = ofSymbolInternal destType symbol
                ofSymbolMemo.[symbol] <- result
                result
            | (true, symbol) -> symbol
        | None -> ofSymbolInternal destType symbol

    let ofString (destType : Type) (source : string) =
        let symbol = Symbol.ofString source None
        ofSymbol destType symbol

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            match source with
            | null ->
                if pointType = typeof<unit> then "[]" :> obj
                elif FSharpType.IsUnion pointType then (FSharpType.GetUnionCases pointType).[0].Name :> obj
                // here we are totally fucked because PropertyGrid passes typeof<obj> to the converter's ctor and we
                // have no information about what the fuck to do...
                else source
            | _ -> toString pointType source :> obj
        elif destType = typeof<Symbol> then toSymbol pointType source :> obj
        elif destType = pointType then source
        else failconv "Invalid SymbolicConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | null -> source
        | _ ->
            let sourceType = source.GetType ()
            if sourceType <> pointType then
                match source with
                | :? string as sourceStr -> ofString pointType sourceStr
                | :? Symbol as sourceSymbol -> ofSymbol pointType sourceSymbol
                | _ -> failconv "Invalid SymbolicConverter conversion from string." None
            else source

    new (pointType : Type) = SymbolicConverter (false, None, pointType)
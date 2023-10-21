﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections.Generic
open System.ComponentModel
open FSharp.Reflection
open Scripting
#nowarn "21"
#nowarn "40"
module ScriptingMarshalling =

    let rec tryImport (tryImportExt : Type -> obj -> Expr option) (ty : Type) (value : obj) : Expr option =

        // try to import from the custom types
        let ty = if ty.IsGenericTypeDefinition then ty.GetGenericTypeDefinition () else ty
        match Importers.TryGetValue ty.Name with
        | (true, tryImportFn) -> tryImportFn tryImportExt ty value
        | (false, _) ->

            // try to import as extension value
            match tryImportExt ty value with
            | None ->

                // failing that, try import as Tuple
                if FSharpType.IsTuple ty then
                    let tupleElementTypes = FSharpType.GetTupleElements ty
                    let tupleFields = FSharpValue.GetTupleFields value
                    let tupleFieldOpts = Array.mapi (fun i tupleField -> tryImport tryImportExt tupleElementTypes.[i] tupleField) tupleFields
                    match Array.definitizePlus tupleFieldOpts with
                    | (true, tupleFields) -> Some (Tuple tupleFields)
                    | (false, _) -> None

                // or as Record
                elif FSharpType.IsRecord ty then
                    let recordFieldInfos = FSharpType.GetRecordFields ty
                    let recordFields = FSharpValue.GetRecordFields value
                    let recordFieldOpts = Array.mapi (fun i recordField -> tryImport tryImportExt recordFieldInfos.[i].PropertyType recordField) recordFields
                    match Array.definitizePlus recordFieldOpts with
                    | (true, recordFields) ->
                        let recordName = match ty.Name.IndexOf '`' with -1 -> ty.Name | index -> ty.Name.Substring (0, index)
                        let recordFieldMap = recordFieldInfos |> Array.mapi (fun i (field : Reflection.PropertyInfo) -> (field.Name, i)) |> Map.ofArray
                        Some (Record (recordName, recordFieldMap, recordFields))
                    | (false, _) -> None

                // or as Union
                elif FSharpType.IsUnion ty then
                    let (unionCase, unionFields) = FSharpValue.GetUnionFields (value, ty)
                    let unionFieldInfos = unionCase.GetFields ()
                    if not (Array.isEmpty unionFields) then
                        let unionFieldOpts = Array.mapi (fun i unionField -> tryImport tryImportExt unionFieldInfos.[i].PropertyType unionField) unionFields
                        match Array.definitizePlus unionFieldOpts with
                        | (true, unionFields) ->
                            match unionFields with
                            | [||] -> Some (Keyword unionCase.Name)
                            | _ -> Some (Union (unionCase.Name, unionFields))
                        | (false, _) -> None
                    else Some (Union (unionCase.Name, [||]))

                // otherwise, we have no conversion
                else None

            // it's an extension value
            | Some value -> Some value

    and tryImportGuid (_ : Type) (value : obj) =
        Some (String ((GuidConverter ()).ConvertToString value))

    and tryImportKeyValuePair tryImportExt (ty : Type) (value : obj) =
        let gargs = ty.GetGenericArguments ()
        let kvp = Reflection.objToKeyValuePair value
        let keyOpt = tryImport tryImportExt gargs.[0] kvp.Key
        let valueOpt = tryImport tryImportExt gargs.[1] kvp.Value
        match (keyOpt, valueOpt) with
        | (Some key, Some value) -> Some (Tuple [|key; value|])
        | (_, _) -> None

    and tryImportOption tryImportExt (ty : Type) (value : obj) =
        let valueType = (ty.GetGenericArguments ()).[0]
        match Reflection.objToOption value with
        | Some value ->
            match tryImport tryImportExt valueType value with
            | Some value -> Some (Option (Some value))
            | None -> None
        | None -> Some NoneValue

    and tryImportEither tryImportExt (ty : Type) (value : obj) =
        let gargs = ty.GetGenericArguments ()
        let leftType = gargs.[0]
        let rightType = gargs.[1]
        match Reflection.objToEither value with
        | Right right -> tryImport tryImportExt rightType right
        | Left left -> tryImport tryImportExt leftType left

    and tryImportList tryImportExt (ty : Type) (value : obj) =
        let itemType = (ty.GetGenericArguments ()).[0]
        let objList = Reflection.objToObjList value
        let itemOpts = List.map (fun item -> tryImport tryImportExt itemType item) objList
        match List.definitizePlus itemOpts with
        | (true, items) -> Some (List items)
        | (false, _) -> None

    and tryImportSet tryImportExt (ty : Type) (value : obj) =
        let itemType = (ty.GetGenericArguments ()).[0]
        let items = Reflection.objToComparableSet value
        let itemOpts = Seq.map (fun item -> tryImport tryImportExt itemType item) items
        match Seq.definitizePlus itemOpts with
        | (true, items) -> Some (Ring (Set.ofSeq items))
        | (false, _) -> None

    and tryImportMap tryImportExt (ty : Type) (value : obj) =
        let gargs = ty.GetGenericArguments ()
        let keyType = gargs.[0]
        let valueType = gargs.[1]
        let pairs = Reflection.objToObjList value
        let pairs = List.map Reflection.objToKeyValuePair pairs
        let pairOpts = pairs |> List.map (fun kvp -> (tryImport tryImportExt keyType kvp.Key, tryImport tryImportExt valueType kvp.Value))
        let pairOpts = pairOpts |> List.map (fun (k, v) -> match (k, v) with (Some k_, Some v_) -> Some (k_, v_) | _ -> None)
        match List.definitizePlus pairOpts with
        | (true, items) -> Some (Table (Map.ofList items))
        | (false, _) -> None

    and tryImportSymbol tryImportExt (ty : Type) (value : obj) =
        match value with
        | :? Symbol as symbol ->
            match symbol with
            | Atom (str, _) -> Some (Union ("Atom", [|String str|]))
            | Number (str, _) -> Some (Union ("Number", [|String str|]))
            | Text (str, _) -> Some (Union ("String", [|String str|]))
            | Symbol.Quote (symbol, _) ->
                match tryImportSymbol tryImportExt ty symbol with
                | Some expr -> Some (Union ("Quote", [|expr|]))
                | None -> None
            | Symbol.Symbols (symbols, _) ->
                let exprOpts = List.map (tryImportSymbol tryImportExt ty) symbols
                match List.definitizePlus exprOpts with
                | (true, exprs) -> Some (Union ("Symbols", List.toArray exprs))
                | (false, _) -> None
        | _ -> None

    and Importers : Dictionary<string, (Type -> obj -> Expr option) -> Type -> obj -> Expr option> =
        [(typeof<Void>.Name, (fun _ _ _ -> Unit |> Some))
         (typeof<unit>.Name, (fun _ _ _ -> Unit |> Some))
         (typeof<bool>.Name, (fun _ _ (value : obj) -> match value with :? bool as bool -> Some (Bool bool) | _ -> None))
         (typeof<int>.Name, (fun _ _ (value : obj) -> match value with :? int as int -> Some (Int int) | _ -> None))
         (typeof<int64>.Name, (fun _ _ (value : obj) -> match value with :? int64 as int64 -> Some (Int64 int64) | _ -> None))
         (typeof<single>.Name, (fun _ _ (value : obj) -> match value with :? single as single -> Some (Single single) | _ -> None))
         (typeof<double>.Name, (fun _ _ (value : obj) -> match value with :? double as double -> Some (Double double) | _ -> None))
         (typeof<char>.Name, (fun _ _ (value : obj) -> match value with :? char as char -> Some (String (string char)) | _ -> None))
         (typeof<string>.Name, (fun _ _ (value : obj) -> match value with :? string as str -> Some (String str) | _ -> None))
         (typedefof<Guid>.Name, (fun _ ty value -> tryImportGuid ty value))
         (typedefof<KeyValuePair<_, _>>.Name, (fun tryImportExt ty value -> tryImportKeyValuePair tryImportExt ty value))
         (typedefof<_ option>.Name, (fun tryImportExt ty value -> tryImportOption tryImportExt ty value))
         (typedefof<Either<_, _>>.Name, (fun tryImportExt ty value -> tryImportEither tryImportExt ty value))
         (typedefof<_ list>.Name, (fun tryImportExt ty value -> tryImportList tryImportExt ty value))
         (typedefof<_ Set>.Name, (fun tryImportExt ty value -> tryImportSet tryImportExt ty value))
         (typedefof<Map<_, _>>.Name, (fun tryImportExt ty value -> tryImportMap tryImportExt ty value))
         (typedefof<Symbol>.Name, (fun tryImportExt ty value -> tryImportSymbol tryImportExt ty value))] |>
        dictPlus StringComparer.Ordinal

    let rec tryExport tryExportExt (ty : Type) (value : Expr) =

        // try to export from the custom types
        match Exporters.TryGetValue ty.Name with
        | (true, tryExport) -> tryExport tryExportExt ty value
        | (false, _) ->

            // try to export as extension value
            match tryExportExt ty value with
            | None ->

                // failing that, try export as Tuple
                if FSharpType.IsTuple ty then
                    match value with
                    | Tuple fields
                    | Union (_, fields)
                    | Record (_, _, fields) ->
                        let fieldInfos = FSharpType.GetTupleElements ty
                        let fieldOpts = Array.mapi (fun i fieldSymbol -> tryExport tryExportExt fieldInfos.[i] fieldSymbol) fields
                        match Array.definitizePlus fieldOpts with
                        | (true, fields) -> Some (FSharpValue.MakeTuple (fields, ty))
                        | (false, _) -> None
                    | _ -> None

                // or as Record
                elif FSharpType.IsRecord ty then
                    match value with
                    | Union (_, fields)
                    | Record (_, _, fields) ->
                        let fieldInfos = FSharpType.GetRecordFields ty
                        let fieldOpts = Array.mapi (fun i fieldSymbol -> tryExport tryExportExt fieldInfos.[i].PropertyType fieldSymbol) fields
                        match Array.definitizePlus fieldOpts with
                        | (true, fields) -> Some (FSharpValue.MakeRecord (ty, fields))
                        | (false, _) -> None
                    | _ -> None

                // or as Union
                elif FSharpType.IsUnion ty && ty <> typeof<string list> then
                    let unionCases = FSharpType.GetUnionCases ty
                    match value with
                    | Keyword name ->
                        match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = name) unionCases with
                        | Some unionCase -> Some (FSharpValue.MakeUnion (unionCase, [||]))
                        | None -> None
                    | Union (name, fields)
                    | Record (name, _, fields) ->
                        match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = name) unionCases with
                        | Some unionCase ->
                            let unionFieldInfos = unionCase.GetFields ()
                            let unionValueOpts = Array.mapi (fun i unionSymbol -> tryExport tryExportExt unionFieldInfos.[i].PropertyType unionSymbol) fields
                            match Array.definitizePlus unionValueOpts with
                            | (true, unionValues) -> Some (FSharpValue.MakeUnion (unionCase, unionValues))
                            | (false, _) -> None
                        | None -> None
                    | _ -> None

                // otherwise, we have no conversion
                else None

            // it's an extension value
            | Some value -> Some value

    and tryExportGuid (_ : Type) (guid : Expr) =
        match guid with
        | String str | Keyword str -> Some ((GuidConverter ()).ConvertFromString str)
        | _ -> None

    and tryExportKvp tryExportExt (ty : Type) (tuple : Expr) =
        match tuple with
        | Tuple [|fst; snd|] ->
            match ty.GetGenericArguments () with
            | [|fstType; sndType|] ->
                let pairType = typedefof<KeyValuePair<_, _>>.MakeGenericType [|fstType; sndType|]
                let fstOpt = tryExport tryExportExt fstType fst
                let sndOpt = tryExport tryExportExt sndType snd
                match (fstOpt, sndOpt) with
                | (Some fst, Some snd) -> Some (Reflection.objsToKeyValuePair fst snd pairType)
                | (_, _) -> None
            | _ -> None
        | _ -> None

    and tryExportOption tryExportExt (ty : Type) (opt : Expr) =
        match opt with
        | Option opt ->
            match opt with
            | Some value ->
                let valueType = (ty.GetGenericArguments ()).[0]
                match tryExport tryExportExt valueType value with
                | Some value -> Some (Activator.CreateInstance (ty, [|value|]))
                | None -> None
            | None -> Some (None :> obj)
        | _ -> None

    and tryExportEither tryExportExt (ty : Type) (eir : Expr) =
        let leftType = (ty.GetGenericArguments ()).[0]
        let leftCase = (FSharpType.GetUnionCases ty).[1]
        let rightType = (ty.GetGenericArguments ()).[1]
        let rightCase = (FSharpType.GetUnionCases ty).[0]
        match eir with
        | Either eir ->
            match eir with
            | Right right ->
                match tryExport tryExportExt rightType right with
                | Some right -> Some (FSharpValue.MakeUnion (rightCase, [|right|]))
                | None -> None
            | Left left ->
                match tryExport tryExportExt leftType left with
                | Some left -> Some (FSharpValue.MakeUnion (leftCase, [|left|]))
                | None -> None
        | _ -> None

    and tryExportList tryExportExt (ty : Type) (list : Expr) =
        match list with
        | List list ->
            let garg = ty.GetGenericArguments () |> Array.item 0
            let itemType = if garg.IsGenericTypeDefinition then garg.GetGenericTypeDefinition () else garg
            let itemOpts = List.map (fun item -> tryExport tryExportExt itemType item) list
            match List.definitizePlus itemOpts with
            | (true, items) -> Some (Reflection.objsToList ty items)
            | (false, _) -> None
        | _ -> None

    and tryExportSet tryExportExt (ty : Type) (ring : Expr) =
        match ring with
        | Ring set ->
            let elementType = (ty.GetGenericArguments ()).[0]
            let elementOpts = Seq.map (fun element -> tryExport tryExportExt elementType element) set
            match Seq.definitizePlus elementOpts with
            | (true, elements) -> Some (Reflection.objsToSet ty elements)
            | (false, _) -> None
        | _ -> None

    and tryExportMap tryExportExt (ty : Type) (table : Expr) =
        match table with
        | Table map ->
            match ty.GetGenericArguments () with
            | [|fstType; sndType|] ->
                let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                let pairOpts = Seq.map (fun (kvp : KeyValuePair<_, _>) -> tryExport tryExportExt pairType (Tuple [|kvp.Key; kvp.Value|])) map
                match Seq.definitizePlus pairOpts with
                | (true, pairs) -> Some (Reflection.pairsToMap ty pairs)
                | (false, _) -> None
            | _ -> None
        | _ -> None

    and tryExportSymbol tryExportExt (ty : Type) (expr : Expr) =
        match expr with
        | Union (name, exprs) ->
            match (name, exprs) with
            | ("Atom", [|String str|]) -> Some (Symbol.Atom (str, ValueNone) :> obj)
            | ("Number", [|String str|]) -> Some (Symbol.Number (str, ValueNone) :> obj)
            | ("String", [|String str|]) -> Some (Symbol.Text (str, ValueNone) :> obj)
            | ("Quote", [|expr|]) ->
                match tryExportSymbol tryExportExt ty expr with
                | Some symbol -> Some (Symbol.Quote (symbol :?> Symbol, ValueNone) :> obj)
                | None -> None
            | ("Symbols", exprs) ->
                let symbolsOpts =
                    Array.map (tryExportSymbol tryExportExt ty) exprs |>
                    Array.map (Option.map cast<Symbol>)
                match Array.definitizePlus symbolsOpts with
                | (true, symbols) -> Some (Symbols (Array.toList symbols, ValueNone) :> obj)
                | (false, _) -> None
            | (_, _) -> None
        | _ -> None

    and Exporters : Dictionary<string, (Type -> Expr -> obj option) -> Type -> Expr -> obj option> =
        [(typeof<Void>.Name, fun _ _ _ -> () :> obj |> Some)
         (typeof<unit>.Name, fun _ _ _ -> () :> obj |> Some)
         (typeof<bool>.Name, fun _ _ evaled -> match evaled with Bool value -> value :> obj |> Some | _ -> None)
         (typeof<int>.Name, fun _ _ evaled -> match evaled with Int value -> value :> obj |> Some | _ -> None)
         (typeof<int64>.Name, fun _ _ evaled -> match evaled with Int64 value -> value :> obj |> Some | _ -> None)
         (typeof<single>.Name, fun _ _ evaled -> match evaled with Single value -> value :> obj |> Some | _ -> None)
         (typeof<double>.Name, fun _ _ evaled -> match evaled with Double value -> value :> obj |> Some | _ -> None)
         (typeof<char>.Name, fun _ _ evaled -> match evaled with String value when value.Length = 1 -> value.[0] :> obj |> Some | _ -> None)
         (typeof<string>.Name, fun _ _ evaled -> match evaled with String value -> value :> obj |> Some | Keyword value -> value :> obj |> Some | _ -> None)
         (typedefof<Guid>.Name, fun _ ty evaled -> tryExportGuid ty evaled)
         (typedefof<KeyValuePair<_, _>>.Name, tryExportKvp)
         (typedefof<_ option>.Name, tryExportOption)
         (typedefof<Either<_, _>>.Name, tryExportEither)
         (typedefof<_ list>.Name, tryExportList)
         (typedefof<_ Set>.Name, tryExportSet)
         (typedefof<Map<_, _>>.Name, tryExportMap)
         (typedefof<Symbol>.Name, tryExportSymbol)] |>
        dictPlus StringComparer.Ordinal

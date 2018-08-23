// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open Prime
open Prime.Scripting
module ScriptingPrimitives =

    let evalSinglet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled|] -> fn fnName argEvaled originOpt world
        | _ -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 1 argument.", originOpt), world)

    let evalDoublet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled; arg2Evaled|] -> fn fnName argEvaled arg2Evaled originOpt world
        | _ -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 2 arguments.", originOpt), world)

    let evalTriplet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled; arg2Evaled; arg3Evaled|] -> fn fnName argEvaled arg2Evaled arg3Evaled originOpt world
        | _ -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 3 arguments.", originOpt), world)

    let evalQuadlet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled; arg2Evaled; arg3Evaled; arg4Evaled|] -> fn fnName argEvaled arg2Evaled arg3Evaled arg4Evaled originOpt world
        | _ -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 4 arguments.", originOpt), world)

    let evalQuintet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled; arg2Evaled; arg3Evaled; arg4Evaled; arg5Evaled|] -> fn fnName argEvaled arg2Evaled arg3Evaled arg4Evaled arg5Evaled originOpt world
        | _ -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 5 arguments.", originOpt), world)

    let evalDereference fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Option opt ->
            match opt with
            | Some value -> struct (value, world)
            | None -> struct (Violation (["InvalidDereference"; String.capitalize fnName], "Function '" + fnName + "' requires some value.", originOpt), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Referent value.", originOpt), world)

    let evalIndexIntInner index fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> Right struct (violation, world)
        | String str ->
            if index >= 0 && index < String.length str
            then Right struct (String (string str.[index]), world)
            else Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "String does not contain element at index " + string index + ".", originOpt), world)
        | Option opt ->
            match (index, opt) with
            | (0, Some value) -> Right struct (value, world)
            | (_, Some _) -> Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Option does not contain element at index " + string index + ".", originOpt), world)
            | (_, None) -> Left struct (Violation (["InvalidIndex"; String.capitalize fnName], "Function '" + fnName + "' requires some value.", originOpt), world)
        | Codata _ ->
            Left struct (Violation (["NotImplemented"; String.capitalize fnName], "Function '" + fnName + "' is not implemented for Codata.", originOpt), world)
        | List list ->
            match List.tryItem index list with
            | Some item -> Right struct (item, world)
            | None -> Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "List does not contain element at index " + string index + ".", originOpt), world)
        | Table map ->
            match Map.tryFind (Int index) map with
            | Some value -> Right struct (value, world)
            | None -> Left struct (Violation (["IndexNotFound"; String.capitalize fnName], "Table does not contain entry at index " + string index + ".", originOpt), world)
        | Tuple fields
        | Union (_, fields)
        | Record (_, _, fields) ->
            if index >= 0 && index < Array.length fields
            then Right struct (fields.[index], world)
            else Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Structure does not contain element at index " + string index + ".", originOpt), world)
        | _ -> Left struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires an indexed value for its second argument.", originOpt), world)

    let evalIndexKeywordInner name fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> Left struct (violation, world)
        | Table map ->
            match Map.tryFind (Keyword name) map with
            | Some value -> Right struct (value, world)
            | None -> Left struct (Violation (["InvalidIndex"; String.capitalize fnName], "Table does not contain entry with key '" + name + "'.", originOpt), world)
        | Record (_, map, fields) ->
            match Map.tryFind name map with
            | Some index ->
                if index >= 0 && index < Array.length fields
                then Right struct (fields.[index], world)
                else Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt), world)
            | None ->
                Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt), world)
        | _ -> Left struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a name-indexed value for its second argument.", originOpt), world)

    let evalIndexInner fnName argEvaled arg2Evaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> Left struct (violation, world)
        | Int index -> evalIndexIntInner index fnName arg2Evaled originOpt world
        | Keyword str -> evalIndexKeywordInner str fnName arg2Evaled originOpt world
        | _ ->
            match arg2Evaled with
            | Table map ->
                match Map.tryFind argEvaled map with
                | Some value -> Right struct (value, world)
                | None -> Left struct (Violation (["InvalidIndex"; String.capitalize fnName], "Table does not contain entry with key '" + scstring argEvaled + "'.", originOpt), world)
            | _ -> Left struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " with non-String / non-Keyword indexes only applicable on Tables.", originOpt), world)

    let evalTryIndex fnName argEvaled arg2Evaled originOpt world =
        match evalIndexInner fnName argEvaled arg2Evaled originOpt world with
        | Right struct (evaled, world) -> struct (Option (Some evaled), world)
        | Left struct (_, world) -> struct (Option None, world)

    let evalHasIndex fnName argEvaled arg2Evaled originOpt world =
        match evalIndexInner fnName argEvaled arg2Evaled originOpt world with
        | Right struct (_, world) -> struct (Bool true, world)
        | Left struct (_, world) -> struct (Bool false, world)

    let evalIndexInt index fnName argEvaled originOpt world =
        let eir = evalIndexIntInner index fnName argEvaled originOpt world
        Either.amb eir

    let evalIndexKeyword index fnName argEvaled originOpt world =
        let eir = evalIndexKeywordInner index fnName argEvaled originOpt world
        Either.amb eir

    let evalIndex fnName argEvaled arg2Evaled originOpt world =
        match evalIndexInner fnName argEvaled arg2Evaled originOpt world with
        | Right success -> success
        | Left error -> error

    let evalNth fnName argEvaled arg2Evaled originOpt world =
        match argEvaled with
        | Violation _ as error -> struct (error, world)
        | Int index -> evalIndexInt index fnName arg2Evaled originOpt world
        | _ -> struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Application of '" + fnName + "'requires an Int as its first argument.", originOpt), world)

    let evalGetTypeName _ argEvaled _ world =
        match argEvaled with
        | Violation _ as error -> struct (error, world)
        | Unit _ -> struct (String "Unit", world)
        | Bool _ -> struct (String "Bool", world)
        | Int _ -> struct (String "Int", world)
        | Int64 _ -> struct (String "Int64", world)
        | Single _ -> struct (String "Single", world)
        | Double _ -> struct (String "Double", world)
        | String _ -> struct (String "String", world)
        | Keyword _ -> struct (String "Keyword", world)
        | Pluggable pluggable -> struct (String pluggable.TypeName, world)
        | Tuple _ -> struct (String "Tuple", world)
        | Union _ -> struct (String "Union", world)
        | Option _ -> struct (String "Option", world)
        | Codata _ -> struct (String "Codata", world)
        | List _ -> struct (String "List", world)
        | Ring _ -> struct (String "Ring", world)
        | Table _ -> struct (String "Table", world)
        | Record _ -> struct (String "Record", world)
        | Fun _ -> struct (String "Function", world)
        | Quote _ -> struct (String "Quote", world)
        | _ -> failwithumf ()

    let evalGetName fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Union (name, _) -> struct (String name, world)
        | Record (name, _, _) -> struct (String name, world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Union or Record value.", originOpt), world)

    let evalPure fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|Violation _ as violation; _|] -> struct (violation, world)
        | [|_; Violation _ as violation|] -> struct (violation, world)
        | [|String _; value|] ->
            match value with
            | String str as string when str.Length = 1 -> struct (string, world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " for String must be a String of length 1.", originOpt), world)
        | [|Option _; value|] -> struct (Option (Some value), world)
        | [|Codata _; value|] -> struct (Codata (Conversion [value]), world)
        | [|List _; value|] -> struct (List [value], world)
        | [|Ring _; value|] -> struct (Ring (Set.singleton value), world)
        | [|Table _; value|] ->
            match value with
            | Tuple [|v1; v2|] -> struct (Table (Map.singleton v1 v2), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " for Table must be a 2-value Tuple.", originOpt), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for String, Option, Codata, Ring, Table or List.", originOpt), world)

    let evalApp evalApply fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|Violation _ as violation; _|] -> struct (violation, world)
        | [|_; Violation _ as violation|] -> struct (violation, world)
        | [|Option fnOpt; Option valueOpt|] ->
            match (fnOpt, valueOpt) with
            | (Some fn, Some value) -> evalApply [|fn; value|] originOpt world
            | (_, _) -> struct (Option None, world)
        | [|List fns; List values|] ->
            let (resultsRev, world) =
              List.fold
                (fun (resultsRev, world) value ->
                    let (result, world) =
                        List.fold
                            (fun (result, world) fn ->
                                let struct (result', world) = evalApply [|fn; result|] originOpt world
                                (result', world))
                            (value, world)
                            fns
                    (result :: resultsRev, world))
                ([], world)
                values
            struct (List (List.rev resultsRev), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for Option or List.", originOpt), world)

    let evalBind evalApply fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|Violation _ as violation; _|] -> struct (violation, world)
        | [|_; Violation _ as violation|] -> struct (violation, world)
        | [|Option valueOpt; fn|] ->
            match valueOpt with
            | Some value -> evalApply [|fn; value|] originOpt world
            | None -> struct (Option None, world)
        | [|List values; fn|] ->
            let (resultsRev, world) =
              List.fold
                (fun (resultsRev, world) value ->
                    let struct (result, world) = evalApply [|fn; value|] originOpt world
                    (result :: resultsRev, world))
                ([], world)
                values
            struct (List (List.rev resultsRev), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for Option or List.", originOpt), world)

    let evalTuple _ argsEvaled _ world =
        struct (Tuple argsEvaled, world)

    let evalPair _ (_ : string) argEvaled arg2Evaled world =
        struct (Tuple [|argEvaled; arg2Evaled|], world)

    let evalSome _ argEvaled _ world =
        struct (Option (Some argEvaled), world)
    
    let evalIsNone fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Option evaled -> struct (Bool (Option.isNone evaled), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-Option.", originOpt), world)
    
    let evalIsSome fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Option evaled -> struct (Bool (Option.isSome evaled), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-Option.", originOpt), world)

    let evalCodata fnName argEvaled arg2Evaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Binding _ as binding -> struct (Codata (Unfold (binding, arg2Evaled)), world) // evaled expr to binding implies extrinsic or intrinsic function
        | Fun _ as fn -> struct (Codata (Unfold (fn, arg2Evaled)), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "First argument to " + fnName + " must be a Function.", originOpt), world)

    let rec evalCodataTryUncons evalApply fnName originOpt codata world =
        match codata with
        | Empty -> Right (Left world)
        | Add (left, right) ->
            match evalCodataTryUncons evalApply fnName originOpt left world with
            | Right (Right struct (_, _, _)) as success -> success
            | Right (Left world) -> evalCodataTryUncons evalApply fnName originOpt right world
            | Left _ as error -> error
        | Unfold (unfolder, state) ->
            match evalApply [|unfolder; state|] originOpt world with
            | struct (Option (Some state), world) -> Right (Right struct (state, Unfold (unfolder, state), world))
            | struct (Option None, world) -> Right (Left world)
            | struct (Violation _, _) as error -> Left error
            | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt), world)
        | Conversion (head :: []) -> Right (Right struct (head, Empty, world))
        | Conversion (head :: tail) -> Right (Right struct (head, Conversion tail, world))
        | Conversion [] -> Right (Left world)

    let rec evalCodataIsEmpty evalApply fnName originOpt codata world =
        match evalCodataTryUncons evalApply fnName originOpt codata world with
        | Right (Right struct (_, _, world)) -> Right struct (false, world)
        | Right (Left world) -> Right struct (true, world)
        | Left error -> Left error

    let evalIsEmpty evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Bool bool -> struct (Bool (not bool), world)
        | Int int -> struct (Bool (int = 0), world)
        | Int64 int64 -> struct (Bool (int64 = 0L), world)
        | Single single -> struct (Bool (single = 0.0f), world)
        | Double double -> struct (Bool (double = 0.0), world)
        | String str -> struct (Bool (String.isEmpty str), world)
        | Keyword str -> struct (Bool (String.isEmpty str), world)
        | Union (str, _) -> struct (Bool (String.isEmpty str), world)
        | Option opt -> struct (Bool (Option.isNone opt), world)
        | Codata codata ->
            match evalCodataIsEmpty evalApply fnName originOpt codata world with
            | Right struct (empty, world) -> struct (Bool empty, world)
            | Left error -> error
        | List list -> struct (Bool (List.isEmpty list), world)
        | Ring set -> struct (Bool (Set.isEmpty set), world)
        | Table map -> struct (Bool (Map.isEmpty map), world)
        | Record (str, _, _) -> struct (Bool (String.isEmpty str), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalNotEmpty evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Bool bool -> struct (Bool bool, world)
        | Int int -> struct (Bool (int <> 0), world)
        | Int64 int64 -> struct (Bool (int64 <> 0L), world)
        | Single single -> struct (Bool (single <> 0.0f), world)
        | Double double -> struct (Bool (double <> 0.0), world)
        | String str -> struct (Bool (String.notEmpty str), world)
        | Keyword str -> struct (Bool (String.notEmpty str), world)
        | Union (str, _) -> struct (Bool (String.notEmpty str), world)
        | Option opt -> struct (Bool (Option.isSome opt), world)
        | Codata codata ->
            match evalCodataIsEmpty evalApply fnName originOpt codata world with
            | Right struct (empty, world) -> struct (Bool (not empty), world)
            | Left error -> error
        | List list -> struct (Bool (List.notEmpty list), world)
        | Ring set -> struct (Bool (Set.notEmpty set), world)
        | Table map -> struct (Bool (Map.notEmpty map), world)
        | Record (str, _, _) -> struct (Bool (String.notEmpty str), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalTryUnconsInner evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> Left struct (violation, world)
        | String str ->
            if String.notEmpty str
            then Right (Right struct (String (string str.[0]), String (str.Substring 1), world))
            else Right (Left world)
        | Option opt ->
            match opt with
            | Some value -> Right (Right struct (value, NoneValue, world))
            | None -> Right (Left world)
        | Codata codata ->
            match evalCodataTryUncons evalApply fnName originOpt codata world with
            | Right (Right struct (head, tail, world)) -> Right (Right struct (head, Codata tail, world))
            | Right (Left world) -> Right (Left world)
            | Left error -> Left error
        | List list ->
            match list with
            | [] -> Right (Left world)
            | head :: tail -> Right (Right struct (head, List tail, world))
        | Ring set ->
            match Seq.tryHead set with
            | Some head -> Right (Right struct (head, Ring (Set.remove head set), world))
            | None -> Right (Left world)
        | Table map ->
            match Seq.tryHead map with
            | Some kvp -> Right (Right struct (Tuple [|kvp.Key; kvp.Value|], Table (Map.remove kvp.Key map), world))
            | None -> Right (Left world)
        | _ -> Left struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalTryUncons evalApply fnName argEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argEvaled originOpt world with
        | Right (Right struct (head, tail, world)) -> struct (Option (Some (Tuple [|head; tail|])), world)
        | Right (Left world) -> struct (Option None, world)
        | Left error -> error

    let evalUncons evalApply fnName argEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argEvaled originOpt world with
        | Right (Right struct (head, tail, world)) -> struct (Tuple [|head; tail|], world)
        | Right (Left world) -> struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Cannot apply " + fnName + " to an empty container.", originOpt), world)
        | Left error -> error

    let evalCons fnName argEvaled arg2Evaled originOpt world =
        match (argEvaled, arg2Evaled) with
        | (argEvaled, String str) ->
            match argEvaled with
            | Violation _ as violation -> struct (violation, world)
            | String head when String.length head = 1 -> struct (String (head + str), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 string arguments required where the first is of length 1.", originOpt), world)
        | (argEvaled, Option opt) ->
            match opt with
            | Some _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot cons onto some value.", originOpt), world)
            | None -> struct (Option (Some argEvaled), world)
        | (argEvaled, List list) ->
            struct (List (argEvaled :: list), world)
        | (argEvaled, Codata codata) ->
            match codata with
            | Empty -> struct (Codata (Conversion [argEvaled]), world)
            | Add _ -> struct (Codata (Add (Conversion [argEvaled], codata)), world)
            | Unfold _ -> struct (Codata (Add (Conversion [argEvaled], codata)), world)
            | Conversion list -> struct (Codata (Conversion (argEvaled :: list)), world)
        | (argEvaled, Ring set) ->
            struct (Ring (Set.add argEvaled set), world)
        | (argEvaled, Table map) ->
            match argEvaled with
            | Violation _ as violation -> struct (violation, world)
            | Tuple elems when Array.length elems = 2 -> struct (Table (Map.add elems.[0] elems.[1] map), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Table entry must consist of a pair.", originOpt), world)
        | (Violation _ as violation, _) -> struct (violation, world)
        | (_, (Violation _ as violation)) -> struct (violation, world)
        | (_, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-List.", originOpt), world)

    let evalCommit fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Option _ -> struct (argEvaled, world)
        | Codata _ -> struct (argEvaled, world)
        | List list -> struct (List (List.rev list), world)
        | Ring _ -> struct (argEvaled, world)
        | Table _ -> struct (argEvaled, world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalTryHead evalApply fnName argsEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argsEvaled originOpt world with
        | Right (Right struct (head, _, world)) -> struct (head, world)
        | Right (Left world) -> struct (Option None, world)
        | Left error -> error

    let evalHead evalApply fnName argsEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argsEvaled originOpt world with
        | Right (Right struct (head, _, world)) -> struct (head, world)
        | Right (Left world) -> struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Cannot apply " + fnName + " to a container with no elements.", originOpt), world)
        | Left error -> error

    let evalTryTail evalApply fnName argsEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argsEvaled originOpt world with
        | Right (Right struct (_, tail, world)) -> struct (tail, world)
        | Right (Left world) -> struct (Option None, world)
        | Left error -> error

    let evalTail evalApply fnName argsEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argsEvaled originOpt world with
        | Right (Right struct (_, tail, world)) -> struct (tail, world)
        | Right (Left world) -> struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Cannot apply " + fnName + " to a container with no elements.", originOpt), world)
        | Left error -> error

    let rec evalScanWhileCodata evalApply fnName originOpt scanner state codata world =
        match codata with
        | Empty ->
            Right struct (state, [], world)
        | Add (left, right) ->
            match evalScanWhileCodata evalApply fnName originOpt scanner state left world with
            | Right struct (state, statesLeft, world) ->
                match evalScanWhileCodata evalApply fnName originOpt scanner state right world with
                | Right struct (state, statesRight, world) -> Right struct (state, statesRight @ statesLeft, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | struct (Option (Some costate), world) ->
                match evalApply [|scanner; state; costate|] originOpt world with
                | struct (Option (Some state), world) -> evalScanWhileCodata evalApply fnName originOpt scanner state (Unfold (unfolder, costate)) world
                | struct (Option None, world) -> Right struct (state, [], world)
                | struct (Violation _, _) as error -> Left error
                | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt), world)
            | struct (Option None, world) -> Right struct (state, [], world)
            | struct (Violation _, _) as error -> Left error
            | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (state, states, world) elem ->
                match evalApply [|scanner; state; elem|] originOpt world with
                | struct (Option (Some state), world) -> (Right struct (state, state :: states, world))
                | struct (Option None, world) -> Left struct (List states, world)
                | struct (Violation _, _) as error -> Left error
                | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt), world))
                (Right struct (state, [], world))
                list

    let rec evalScaniCodata evalApply fnName originOpt i scanner state codata world =
        match codata with
        | Empty ->
            Right struct (i, state, [], world)
        | Add (left, right) ->
            match evalScaniCodata evalApply fnName originOpt (inc i) scanner state left world with
            | Right struct (i, state, statesLeft, world) ->
                match evalScaniCodata evalApply fnName originOpt (inc i) scanner state right world with
                | Right struct (i, state, statesRight, world) -> Right struct (i, state, statesRight @ statesLeft, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | struct (Option (Some costate), world) ->
                match evalApply [|scanner; Int i; state; costate|] originOpt world with
                | struct (Violation _, _) as error -> Left error
                | struct (state, world) -> evalScaniCodata evalApply fnName originOpt (inc i) scanner state (Unfold (unfolder, costate)) world
            | struct (Option None, world) -> Right struct (i, state, [], world)
            | struct (Violation _, _) as error -> Left error
            | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (i, state, states, world) elem ->
                match evalApply [|scanner; Int i; state; elem|] originOpt world with
                | struct (Option (Some state), world) -> (Right struct (inc i, state, state :: states, world))
                | struct (Option None, world) -> Left struct (List states, world)
                | struct (Violation _, _) as error -> Left error
                | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt), world))
                (Right struct (i, state, [], world))
                list

    let rec evalScanCodata evalApply fnName originOpt scanner state codata world =
        match codata with
        | Empty ->
            Right struct (state, [], world)
        | Add (left, right) ->
            match evalScanCodata evalApply fnName originOpt scanner state left world with
            | Right struct (state, statesLeft, world) ->
                match evalScanCodata evalApply fnName originOpt scanner state right world with
                | Right struct (state, statesRight, world) -> Right struct (state, statesRight @ statesLeft, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | struct (Option (Some costate), world) ->
                match evalApply [|scanner; state; costate|] originOpt world with
                | struct (Violation _, _) as error -> Left error
                | struct (state, world) -> evalScanCodata evalApply fnName originOpt scanner state (Unfold (unfolder, costate)) world
            | struct (Option None, world) -> Right struct (state, [], world)
            | struct (Violation _, _) as error -> Left error
            | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (state, states, world) elem ->
                match evalApply [|scanner; state; elem|] originOpt world with
                | struct (Option (Some state), world) -> (Right struct (state, state :: states, world))
                | struct (Option None, world) -> Left struct (List states, world)
                | struct (Violation _, _) as error -> Left error
                | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt), world))
                (Right struct (state, [], world))
                list

    let evalScanWhile evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (scanner, state, String str) ->
            match
                Seq.foldWhileRight (fun struct (state, states, world) elem ->
                    match evalApply [|scanner; state; String (string elem)|] originOpt world with
                    | struct (Option (Some state), world) -> (Right struct (state, state :: states, world))
                    | struct (Option None, world) -> Left struct (List states, world)
                    | struct (Violation _, _) as error -> Left error
                    | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt), world))
                    (Right struct (state, [], world))
                    str with
            | Right struct (_, states, world) -> struct (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, Codata codata) ->
            match evalScanWhileCodata evalApply fnName originOpt scanner state codata world with
            | Right struct (_, states, world) -> struct (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, List list) ->
            match
                Seq.foldWhileRight (fun struct (state, states, world) elem ->
                    match evalApply [|scanner; state; elem|] originOpt world with
                    | struct (Option (Some state), world) -> (Right struct (state, state :: states, world))
                    | struct (Option None, world) -> Left struct (List states, world)
                    | struct (Violation _, _) as error -> Left error
                    | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt), world))
                    (Right struct (state, [], world))
                    list with
            | Right struct (_, states, world) -> struct (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, Ring set) ->
            match
                Seq.foldWhileRight (fun struct (state, states, world) elem ->
                    match evalApply [|scanner; state; elem|] originOpt world with
                    | struct (Option (Some state), world) -> (Right struct (state, state :: states, world))
                    | struct (Option None, world) -> Left struct (List states, world)
                    | struct (Violation _, _) as error -> Left error
                    | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt), world))
                    (Right struct (state, [], world))
                    set with
            | Right struct (_, states, world) -> struct (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, Table map) ->
            match
                Seq.foldWhileRight (fun struct (state, states, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    match evalApply [|scanner; state; entry|] originOpt world with
                    | struct (Option (Some state), world) -> (Right struct (state, state :: states, world))
                    | struct (Option None, world) -> Left struct (List states, world)
                    | struct (Violation _, _) as error -> Left error
                    | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt), world))
                    (Right struct (state, [], world))
                    (Map.toList map) with
            | Right struct (_, states, world) -> struct (List (List.rev states), world)
            | Left error -> error
        | (Violation _ as error, _, _) -> struct (error, world)
        | (_, (Violation _ as error), _) -> struct (error, world)
        | (_, _, (Violation _ as error)) -> struct (error, world)
        | (_, _, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalScani evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (scanner, state, String str) ->
            let struct (_, states, world) =
                Seq.foldi (fun i struct (state, states, world) elem ->
                    let struct (state, world) = evalApply [|scanner; Int i; state; String (string elem)|] originOpt world
                    struct (state, state :: states, world))
                    struct (state, [], world)
                    str
            struct (List (List.rev states), world)
        | (scanner, state, Codata codata) ->
            match evalScaniCodata evalApply fnName originOpt 0 scanner state codata world with
            | Right struct (_, _, states, world) -> struct (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, List list) ->
            let struct (_, states, world) =
                Seq.foldi (fun i struct (state, states, world) elem ->
                    let struct (state, world) = evalApply [|scanner; Int i; state; elem|] originOpt world
                    struct (state, state :: states, world))
                    struct (state, [], world)
                    list
            struct (List (List.rev states), world)
        | (scanner, state, Ring set) ->
            let struct (_, states, world) =
                Seq.foldi (fun i struct (state, states, world) elem ->
                    let struct (state, world) = evalApply [|scanner; Int i; state; elem|] originOpt world
                    struct (state, state :: states, world))
                    struct (state, [], world)
                    set
            struct (List (List.rev states), world)
        | (scanner, state, Table map) ->
            let struct (_, states, world) =
                Seq.foldi (fun i struct (state, states, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    let struct (state, world) = evalApply [|scanner; Int i; state; entry|] originOpt world
                    struct (state, state :: states, world))
                    struct (state, [], world)
                    (Map.toList map)
            struct (List (List.rev states), world)
        | (Violation _ as error, _, _) -> struct (error, world)
        | (_, (Violation _ as error), _) -> struct (error, world)
        | (_, _, (Violation _ as error)) -> struct (error, world)
        | (_, _, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalScan evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (scanner, state, String str) ->
            let struct (_, states, world) =
                Seq.fold (fun struct (state, states, world) elem ->
                    let struct (state, world) = evalApply [|scanner; state; String (string elem)|] originOpt world
                    struct (state, state :: states, world))
                    struct (state, [], world)
                    str
            struct (List (List.rev states), world)
        | (scanner, state, Codata codata) ->
            match evalScanCodata evalApply fnName originOpt scanner state codata world with
            | Right struct (_, states, world) -> struct (List (List.rev states), world)
            | Left error -> error
        | (scanner, state, List list) ->
            let struct (_, states, world) =
                Seq.fold (fun struct (state, states, world) elem ->
                    let struct (state, world) = evalApply [|scanner; state; elem|] originOpt world
                    struct (state, state :: states, world))
                    struct (state, [], world)
                    list
            struct (List (List.rev states), world)
        | (scanner, state, Ring set) ->
            let struct (_, states, world) =
                Seq.fold (fun struct (state, states, world) elem ->
                    let struct (state, world) = evalApply [|scanner; state; elem|] originOpt world
                    struct (state, state :: states, world))
                    struct (state, [], world)
                    set
            struct (List (List.rev states), world)
        | (scanner, state, Table map) ->
            let struct (_, states, world) =
                Seq.fold (fun struct (state, states, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    let struct (state, world) = evalApply [|scanner; state; entry|] originOpt world
                    struct (state, state :: states, world))
                    struct (state, [], world)
                    (Map.toList map)
            struct (List (List.rev states), world)
        | (Violation _ as error, _, _) -> struct (error, world)
        | (_, (Violation _ as error), _) -> struct (error, world)
        | (_, _, (Violation _ as error)) -> struct (error, world)
        | (_, _, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let rec evalFoldWhileCodata evalApply fnName originOpt folder state codata world =
        match codata with
        | Empty ->
            Right struct (state, world)
        | Add (left, right) ->
            match evalFoldWhileCodata evalApply fnName originOpt folder state left world with
            | Right struct (state, world) -> evalFoldWhileCodata evalApply fnName originOpt folder state right world
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | struct (Option (Some costate), world) ->
                match evalApply [|folder; state; costate|] originOpt world with
                | struct (Option (Some state), world) -> evalFoldWhileCodata evalApply fnName originOpt folder state (Unfold (unfolder, costate)) world
                | struct (Option None, world) -> Right struct (state, world)
                | struct (Violation _, _) as error -> Left error
                | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt), world)
            | struct (Option None, world) -> Right struct (state, world)
            | struct (Violation _, _) as error -> Left error
            | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (state, world) elem ->
                match evalApply [|folder; state; elem|] originOpt world with
                | struct (Option (Some state), world) -> Right struct (state, world)
                | struct (Option None, world) -> Left struct (state, world)
                | struct (Violation _, _) as error -> Left error
                | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt), world))
                (Right struct (state, world))
                list

    let rec evalFoldiCodata evalApply fnName originOpt i folder state codata world =
        match codata with
        | Empty ->
            Right struct (i, state, world)
        | Add (left, right) ->
            match evalFoldiCodata evalApply fnName originOpt (inc i) folder state left world with
            | Right struct (i, state, world) ->
                match evalFoldiCodata evalApply fnName originOpt (inc i) folder state right world with
                | Right struct (i, state, world) -> Right struct (i, state, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | struct (Option (Some costate), world) ->
                match evalApply [|folder; Int i; state; costate|] originOpt world with
                | struct (Violation _, _) as error -> Left error
                | struct (state, world) -> evalFoldiCodata evalApply fnName originOpt (inc i) folder state (Unfold (unfolder, costate)) world
            | struct (Option None, world) -> Right struct (i, state, world)
            | struct (Violation _, _) as error -> Left error
            | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (i, state, world) elem ->
                match evalApply [|folder; Int i; state; elem|] originOpt world with
                | struct (Option (Some state), world) -> (Right struct (inc i, state, world))
                | struct (Option None, world) -> Left struct (state, world)
                | struct (Violation _, _) as error -> Left error
                | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt), world))
                (Right struct (i, state, world))
                list

    let rec evalFoldCodata evalApply fnName originOpt folder state codata world =
        match codata with
        | Empty ->
            Right struct (state, world)
        | Add (left, right) ->
            match evalFoldCodata evalApply fnName originOpt folder state left world with
            | Right struct (state, world) ->
                match evalFoldCodata evalApply fnName originOpt folder state right world with
                | Right struct (state, world) -> Right struct (state, world)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | struct (Option (Some costate), world) ->
                match evalApply [|folder; state; costate|] originOpt world with
                | struct (Violation _, _) as error -> Left error
                | struct (state, world) -> evalFoldCodata evalApply fnName originOpt folder state (Unfold (unfolder, costate)) world
            | struct (Option None, world) -> Right struct (state, world)
            | struct (Violation _, _) as error -> Left error
            | struct (_, world) -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt), world)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (state, world) elem ->
                match evalApply [|folder; state; elem|] originOpt world with
                | struct (Option (Some state), world) -> (Right struct (state, world))
                | struct (Option None, world) -> Left struct (state, world)
                | struct (Violation _, _) as error -> Left error
                | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt), world))
                (Right struct (state, world))
                list

    let evalFoldWhile evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (folder, state, String str) ->
            let eir =
                Seq.foldWhileRight (fun struct (state, world) elem ->
                    match evalApply [|folder; state; String (string elem)|] originOpt world with
                    | struct (Option (Some state), world) -> (Right struct (state, world))
                    | struct (Option None, world) -> Left struct (state, world)
                    | struct (Violation _, _) as error -> Left error
                    | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt), world))
                    (Right struct (state, world))
                    str
            Either.amb eir
        | (folder, state, Codata codata) ->
            match evalFoldWhileCodata evalApply fnName originOpt folder state codata world with
            | Right success -> success
            | Left error -> error
        | (folder, state, List list) ->
            let eir =
                Seq.foldWhileRight (fun struct (state, world) elem ->
                    match evalApply [|folder; state; elem|] originOpt world with
                    | struct (Option (Some state), world) -> (Right struct (state, world))
                    | struct (Option None, world) -> Left struct (state, world)
                    | struct (Violation _, _) as error -> Left error
                    | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt), world))
                    (Right struct (state, world))
                    list
            Either.amb eir
        | (folder, state, Ring set) ->
            let eir =
                Seq.foldWhileRight (fun struct (state, world) elem ->
                    match evalApply [|folder; state; elem|] originOpt world with
                    | struct (Option (Some state), world) -> (Right struct (state, world))
                    | struct (Option None, world) -> Left struct (state, world)
                    | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt), world))
                    (Right struct (state, world))
                    set
            Either.amb eir
        | (folder, state, Table map) ->
            let eir =
                Seq.foldWhileRight (fun struct (state, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    match evalApply [|folder; state; entry|] originOpt world with
                    | struct (Option (Some state), world) -> (Right struct (state, world))
                    | struct (Option None, world) -> Left struct (state, world)
                    | _ -> Left struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt), world))
                    (Right struct (state, world))
                    (Map.toList map)
            Either.amb eir
        | (Violation _ as error, _, _) -> struct (error, world)
        | (_, (Violation _ as error), _) -> struct (error, world)
        | (_, _, (Violation _ as error)) -> struct (error, world)
        | (_, _, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalFoldi evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (folder, state, String str) -> Seq.foldi (fun i struct (state, world) elem -> evalApply [|folder; Int i; state; String (string elem)|] originOpt world) struct (state, world) str
        | (folder, state, Codata codata) ->
            match evalFoldiCodata evalApply fnName originOpt 0 folder state codata world with
            | Right struct (_, state, world) -> struct (state, world)
            | Left error -> error
        | (folder, state, List list) -> Seq.foldi (fun i struct (state, world) elem -> evalApply [|folder; Int i; state; elem|] originOpt world) struct (state, world) list
        | (folder, state, Ring set) -> Seq.foldi (fun i struct (state, world) elem -> evalApply [|folder; Int i; state; elem|] originOpt world) struct (state, world) set
        | (folder, state, Table map) ->
            Seq.foldi (fun i struct (state, world) (key, value) ->
                let entry = Tuple [|key; value|]
                evalApply [|folder; Int i; state; entry|] originOpt world)
                struct (state, world)
                (Map.toList map)
        | (_, _, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalFold evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (folder, state, String str) -> Seq.fold (fun struct (state, world) elem -> evalApply [|folder; state; String (string elem)|] originOpt world) struct (state, world) str
        | (folder, state, Codata codata) ->
            match evalFoldCodata evalApply fnName originOpt folder state codata world with
            | Right struct (state, world) -> struct (state, world)
            | Left error -> error
        | (folder, state, List list) -> List.fold (fun struct (state, world) elem -> evalApply [|folder; state; elem|] originOpt world) struct (state, world) list
        | (folder, state, Ring set) -> Set.fold (fun struct (state, world) elem -> evalApply [|folder; state; elem|] originOpt world) struct (state, world) set
        | (folder, state, Table map) ->
            Map.fold (fun struct (state, world) key value ->
                let entry = Tuple [|key; value|]
                evalApply [|folder; state; entry|] originOpt world)
                struct (state, world)
                map
        | (Violation _ as error, _, _) -> struct (error, world)
        | (_, (Violation _ as error), _) -> struct (error, world)
        | (_, _, (Violation _ as error)) -> struct (error, world)
        | (_, _, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let rec evalMapCodata evalApply originOpt mapper codata (world : 'w) : struct (Codata * 'w) =
        match codata with
        | Empty ->
            struct (codata, world)
        | Add (left, right) ->
            let struct (leftMapped, world) = evalMapCodata evalApply originOpt mapper left world
            let struct (rightMapped, world) = evalMapCodata evalApply originOpt mapper right world
            struct (Add (leftMapped, rightMapped), world)
        | Unfold (unfolder, codata) ->
            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
            let args = [|unfolder; Binding ("state", ref UncachedBinding, ref Environmental, originOpt)|]
            let unfolder = Unfold (Fun ([|"state"|], 1, Apply (args, breakpoint, originOpt), false, None, originOpt), codata)
            struct (unfolder, world)
        | Conversion list ->
            let struct (mapped, world) =
                List.fold (fun struct (elems, world) elem ->
                    let struct (elem, world) = evalApply [|mapper; elem|] originOpt world
                    struct (elem :: elems, world))
                    struct ([], world)
                    list
            struct (Conversion (List.rev mapped), world)

    let evalMapi evalApply fnName argEvaled arg2Evaled originOpt world =
        match (argEvaled, arg2Evaled) with
        | (mapper, (Option opt as option)) ->
            match opt with
            | Some value -> evalApply [|mapper; Int 0; value|] originOpt world
            | None -> struct (option, world)
        | (mapper, String str) ->
            let (list, world) =
                str |>
                Seq.foldi (fun i (elems, world) elem ->
                    let elem = String (string elem)
                    let struct (elem, world) = evalApply [|mapper; Int i; elem|] originOpt world
                    (elem :: elems, world))
                    ([], world)
            if List.forall (function String str when String.length str = 1 -> true | _ -> false) list
            then struct (String (list |> List.rev |> List.map (function String str -> str.[0] | _ -> failwithumf ()) |> String.implode), world)
            else struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s mapper must return a String of length 1.", originOpt), world)
        | (mapper, Codata codata) ->
            let struct (codata, world) = evalMapCodata evalApply originOpt mapper codata world
            struct (Codata codata, world)
        | (mapper, List list) ->
            let struct (list, world) =
                Seq.foldi (fun i struct (elems, world) elem ->
                    let struct (elem, world) = evalApply [|mapper; Int i; elem|] originOpt world
                    struct (elem :: elems, world))
                    struct ([], world)
                    list
            struct (List (List.rev list), world)
        | (mapper, Ring set) ->
            let struct (set, world) =
                Seq.foldi (fun i struct (elems, world) elem ->
                    let struct (elem, world) = evalApply [|mapper; Int i; elem|] originOpt world
                    struct (Set.add elem elems, world))
                    struct (Set.empty, world)
                    set
            struct (Ring set, world)
        | (mapper, Table map) ->
            let struct (map, world) =
                Seq.foldi (fun i struct (elems, world) (key, value) ->
                    let entry = Tuple [|key; value|]
                    let struct (entry, world) = evalApply [|mapper; Int i; entry|] originOpt world
                    match entry with
                    | Tuple elems' when Array.length elems' = 2 -> struct (Map.add elems'.[0] elems'.[1] elems, world)
                    | _ -> struct (elems, world))
                    struct (Map.empty, world)
                    (Map.toList map)
            struct (Table map, world)
        | (Violation _ as error, _) -> struct (error, world)
        | (_, (Violation _ as error)) -> struct (error, world)
        | (_, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalMap evalApply fnName argEvaled arg2Evaled originOpt world =
        match (argEvaled, arg2Evaled) with
        | (mapper, (Option opt as option)) ->
            match opt with
            | Some value -> evalApply [|mapper; value|] originOpt world
            | None -> struct (option, world)
        | (mapper, String str) ->
            let struct (list, world) =
                str |>
                Seq.fold (fun struct (elems, world) elem ->
                    let elem = String (string elem)
                    let struct (elem, world) = evalApply [|mapper; elem|] originOpt world
                    struct (elem :: elems, world))
                    struct ([], world)
            if List.forall (function String str when String.length str = 1 -> true | _ -> false) list
            then struct (String (list |> List.rev |> List.map (function String str -> str.[0] | _ -> failwithumf ()) |> String.implode), world)
            else struct (Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s mapper must return a String of length 1.", originOpt), world)
        | (mapper, Codata codata) ->
            let struct (codata, world) = evalMapCodata evalApply originOpt mapper codata world
            struct (Codata codata, world)
        | (mapper, List list) ->
            let struct (list, world) =
                List.fold (fun struct (elems, world) elem ->
                    let struct (elem, world) = evalApply [|mapper; elem|] originOpt world
                    struct (elem :: elems, world))
                    struct ([], world)
                    list
            struct (List (List.rev list), world)
        | (mapper, Ring set) ->
            let struct (set, world) =
                Set.fold (fun struct (elems, world) elem ->
                    let struct (elem, world) = evalApply [|mapper; elem|] originOpt world
                    struct (Set.add elem elems, world))
                    struct (Set.empty, world)
                    set
            struct (Ring set, world)
        | (mapper, Table map) ->
            let struct (map, world) =
                Map.fold (fun struct (elems, world) key value ->
                    let entry = Tuple [|key; value|]
                    let struct (entry, world) = evalApply [|mapper; entry|] originOpt world
                    match entry with
                    | Tuple elems' when Array.length elems' = 2 -> struct (Map.add elems'.[0] elems'.[1] elems, world)
                    | _ -> struct (elems, world))
                    struct (Map.empty, world)
                    map
            struct (Table map, world)
        | (Violation _ as error, _) -> struct (error, world)
        | (_, (Violation _ as error)) -> struct (error, world)
        | (_, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let rec evalContainsCodata evalApply fnName argEvaled originOpt codata world =
        match codata with
        | Empty -> Right struct (false, world)
        | Add (left, right) ->
            match evalContainsCodata evalApply fnName argEvaled originOpt left world with
            | Right struct (false, world) -> evalContainsCodata evalApply fnName argEvaled originOpt right world
            | Right struct (true, _) as success -> success
            | Left _ as error -> error
        | Unfold (unfolder, state) ->
            match evalApply [|unfolder; state|] originOpt world with
            | struct (Option (Some state), world) ->
                if state <> argEvaled then
                    let codata = Unfold (unfolder, state)
                    evalContainsCodata evalApply fnName argEvaled originOpt codata world
                else Right struct (true, world)
            | struct (Option None, world) -> Right struct (false, world)
            | error -> Left error
        | Conversion list ->
            Right struct (List.contains argEvaled list, world)

    let evalContains evalApply fnName argEvaled arg2Evaled originOpt world =
        match (argEvaled, arg2Evaled) with
        | (argEvaled, String str) ->
            match argEvaled with
            | String str' -> struct (Bool (str.Contains str'), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "First argument to " + fnName + " for a String must also be a String.", originOpt), world)
        | (argEvaled, Option opt) -> struct (Bool (match opt with Some value -> value = argEvaled | None -> false), world)
        | (argEvaled, Codata codata) ->
            match evalContainsCodata evalApply fnName argEvaled originOpt codata world with
            | Right struct (bool, world) -> struct (Bool bool, world)
            | Left error -> error
        | (argEvaled, List list) -> struct (Bool (List.contains argEvaled list), world)
        | (argEvaled, Ring set) -> struct (Bool (Set.contains argEvaled set), world)
        | (Violation _ as error, _) -> struct (error, world)
        | (_, (Violation _ as error)) -> struct (error, world)
        | (_, _) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalToString fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as error -> struct (error, world)
        | String _ as str -> struct (str, world)
        | List list ->
            if List.forall (function String str when str.Length = 1 -> true | _ -> false) list then
                let chars = List.map (function String str -> str | _ -> failwithumf ()) list
                struct (String (String.Join ("", chars)), world)
            else struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function " + fnName + " cannot only be applied to single character strings.", originOpt), world)
        | Ring set ->
            if Set.forall (function String str when str.Length = 1 -> true | _ -> false) set then
                let chars = Seq.map (function String str -> str | _ -> failwithumf ()) set
                struct (String (String.Join ("", chars)), world)
            else struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function " + fnName + " cannot only be applied to single character strings.", originOpt), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalToCodata fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as error -> struct (error, world)
        | Option opt -> struct (Codata (Conversion (match opt with Some value -> [value] | None -> [])), world)
        | Codata _ -> struct (argEvaled, world)
        | List list -> struct (Codata (Conversion list), world)
        | Ring set -> struct (Codata (Conversion (Set.toList set)), world)
        | Table map -> struct (Codata (Conversion (Map.toListBy (fun key value -> Tuple [|key; value|]) map)), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalList _ argsEvaled _ world =
        struct (List (List.ofArray argsEvaled), world)

    let rec evalCodataToList evalApply fnName originOpt list codata world =
        match evalCodataTryUncons evalApply fnName originOpt codata world with
        | Right (Right struct (head, tail, world)) -> evalCodataToList evalApply fnName originOpt (head :: list) tail world
        | Right (Left world) -> Right (Left struct (list, world))
        | Left struct (error, world) -> Left (struct (error, world))

    let evalToList evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as error -> struct (error, world)
        | String str -> struct (List (str |> Seq.map (string >> String) |> List.ofSeq), world)
        | Option opt -> struct (List (match opt with Some value -> [value] | None -> []), world)
        | Codata codata ->
            match evalCodataToList evalApply fnName originOpt [] codata world with
            | Right (Right struct (_, _, list, world)) -> struct (List (List.rev list), world)
            | Right (Left struct (list, world)) -> struct (List (List.rev list), world)
            | Left error -> error
        | List _ as list -> struct (list, world)
        | Ring set -> struct (List (List.ofSeq set), world)
        | Table map -> struct (List (map |> Map.toListBy (fun k v -> Tuple [|k; v|])), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalRing _ argsEvaled _ world =
        struct (Ring (Set.ofArray argsEvaled), world)

    let evalToRing evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as error -> struct (error, world)
        | String str -> struct (Ring (str |> Seq.map (string >> String) |> Set.ofSeq), world)
        | Option opt -> struct (Ring (match opt with Some value -> Set.singleton value | None -> Set.empty), world)
        | Codata codata ->
            match evalCodataToList evalApply fnName originOpt [] codata world with
            | Right (Right struct (_, _, list, world)) -> struct (Ring (Set.ofList list), world)
            | Right (Left struct (list, world)) -> struct (Ring (Set.ofList list), world)
            | Left error -> error
        | List list -> struct (Ring (Set.ofList list), world)
        | Ring _ as ring -> struct (ring, world)
        | Table map -> struct (Ring (map |> Map.toSeqBy (fun k v -> Tuple [|k; v|]) |> Set.ofSeq), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt), world)

    let evalRemove fnName argEvaled arg2Evaled originOpt world =
        match (argEvaled, arg2Evaled) with
        | (value, container) ->
            match container with
            | Violation _ as error -> struct (error, world)
            | Ring set -> struct (Ring (Set.remove value set), world)
            | Table map -> struct (Table (Map.remove value map), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", originOpt), world)

    let evalToTable fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as error -> struct (error, world)
        | List list ->
            if List.forall (function Tuple [|_; _|] -> true | _ -> false) list then
                let pairs = List.map (function Tuple [|k; v|] -> (k, v) | _ -> failwithumf ()) list
                struct (Table (Map.ofList pairs), world)
            else struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function " + fnName + " cannot only be applied to container of pairs.", originOpt), world)
        | Ring set ->
            if Set.forall (function Tuple [|_; _|] -> true | _ -> false) set then
                let pairs = Seq.map (function Tuple [|k; v|] -> (k, v) | _ -> failwithumf ()) set
                struct (Table (Map.ofSeq pairs), world)
            else struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function " + fnName + " cannot only be applied to container of pairs.", originOpt), world)
        | Table _ as table -> struct (table, world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-string or non-container.", originOpt), world)
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open Scripting
module ScriptingPrimitives =

    let evalSinglet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled|] -> fn fnName argEvaled originOpt world
        | _ -> Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 1 argument.", originOpt)

    let evalDoublet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled; arg2Evaled|] -> fn fnName argEvaled arg2Evaled originOpt world
        | _ -> Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 2 arguments.", originOpt)

    let evalTriplet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled; arg2Evaled; arg3Evaled|] -> fn fnName argEvaled arg2Evaled arg3Evaled originOpt world
        | _ -> Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 3 arguments.", originOpt)

    let evalQuadlet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled; arg2Evaled; arg3Evaled; arg4Evaled|] -> fn fnName argEvaled arg2Evaled arg3Evaled arg4Evaled originOpt world
        | _ -> Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 4 arguments.", originOpt)

    let evalQuintet fn fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|argEvaled; arg2Evaled; arg3Evaled; arg4Evaled; arg5Evaled|] -> fn fnName argEvaled arg2Evaled arg3Evaled arg4Evaled arg5Evaled originOpt world
        | _ -> Violation (["InvalidArgumentCount"; String.capitalize fnName], "Function '" + fnName + "' requires 5 arguments.", originOpt)

    let evalDereference fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> violation
        | Option opt ->
            match opt with
            | Some value -> value
            | None -> Violation (["InvalidDereference"; String.capitalize fnName], "Function '" + fnName + "' requires some value.", originOpt)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Referent value.", originOpt)

    let evalIndexIntInner index fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> Right violation
        | String str ->
            if index >= 0 && index < String.length str
            then Right $ String (string str.[index])
            else Left $ Violation (["ArgumentOutOfRange"; String.capitalize fnName], "String does not contain element at index " + string index + ".", originOpt)
        | Option opt ->
            match (index, opt) with
            | (0, Some value) -> Right value
            | (_, Some _) -> Left $ Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Option does not contain element at index " + string index + ".", originOpt)
            | (_, None) -> Left $ Violation (["InvalidIndex"; String.capitalize fnName], "Function '" + fnName + "' requires some value.", originOpt)
        | Either eir ->
            match (index, eir) with
            | (0, Right value) -> Right value
            | (1, Left value) -> Right value
            | (_, _) -> Left $ Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Either does not contain element at index " + string index + ".", originOpt)
        | Codata _ ->
            Left $ Violation (["NotImplemented"; String.capitalize fnName], "Function '" + fnName + "' is not implemented for Codata.", originOpt)
        | List list ->
            match List.tryItem index list with
            | Some item -> Right item
            | None -> Left $ Violation (["ArgumentOutOfRange"; String.capitalize fnName], "List does not contain element at index " + string index + ".", originOpt)
        | Table map ->
            match Map.tryFind (Int index) map with
            | Some value -> Right value
            | None -> Left $ Violation (["IndexNotFound"; String.capitalize fnName], "Table does not contain entry at index " + string index + ".", originOpt)
        | Tuple fields
        | Union (_, fields)
        | Record (_, _, fields) ->
            if index >= 0 && index < Array.length fields
            then Right fields.[index]
            else Left $ Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Structure does not contain element at index " + string index + ".", originOpt)
        | _ -> Left $ Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires an indexed value for its second argument.", originOpt)

    let evalIndexKeywordInner name fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> Left violation
        | Table map ->
            match Map.tryFind (Keyword name) map with
            | Some value -> Right value
            | None -> Left $ Violation (["InvalidIndex"; String.capitalize fnName], "Table does not contain entry with key '" + name + "'.", originOpt)
        | Record (_, map, fields) ->
            match Map.tryFind name map with
            | Some index ->
                if index >= 0 && index < Array.length fields
                then Right fields.[index]
                else Left $ Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt)
            | None ->
                Left $ Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt)
        | _ -> Left $ Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a name-indexed value for its second argument.", originOpt)

    let evalIndexInner fnName argEvaled arg2Evaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> Left violation
        | Int index -> evalIndexIntInner index fnName arg2Evaled originOpt world
        | Keyword str -> evalIndexKeywordInner str fnName arg2Evaled originOpt world
        | _ ->
            match arg2Evaled with
            | Table map ->
                match Map.tryFind argEvaled map with
                | Some value -> Right value
                | None -> Left $ Violation (["InvalidIndex"; String.capitalize fnName], "Table does not contain entry with key '" + scstring argEvaled + "'.", originOpt)
            | _ -> Left $ Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " with non-String / non-Keyword indexes only applicable on Tables.", originOpt)

    let evalTryIndex fnName argEvaled arg2Evaled originOpt world =
        match evalIndexInner fnName argEvaled arg2Evaled originOpt world with
        | Right evaled -> Option (Some evaled)
        | Left _ -> NoneValue

    let evalHasIndex fnName argEvaled arg2Evaled originOpt world =
        match evalIndexInner fnName argEvaled arg2Evaled originOpt world with
        | Right _ -> Bool true
        | Left _ -> Bool false

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
        | Violation _ as error -> error
        | Int index -> evalIndexInt index fnName arg2Evaled originOpt world
        | _ -> Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Application of '" + fnName + "'requires an Int as its first argument.", originOpt)

    let evalGetTypeName _ argEvaled _ (_ : 'w) =
        match argEvaled with
        | Violation _ as error -> error
        | Unit -> String "Unit"
        | Bool _ -> String "Bool"
        | Int _ -> String "Int"
        | Int64 _ -> String "Int64"
        | Single _ -> String "Single"
        | Double _ -> String "Double"
        | String _ -> String "String"
        | Keyword _ -> String "Keyword"
        | Pluggable pluggable -> String pluggable.TypeName
        | Tuple _ -> String "Tuple"
        | Union _ -> String "Union"
        | Option _ -> String "Option"
        | Either _ -> String "Either"
        | Codata _ -> String "Codata"
        | List _ -> String "List"
        | Ring _ -> String "Ring"
        | Table _ -> String "Table"
        | Record _ -> String "Record"
        | Fun _ -> String "Function"
        | Quote _ -> String "Quote"
        | _ -> failwithumf ()

    let evalGetName fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> violation
        | Union (name, _) -> String name
        | Record (name, _, _) -> String name
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Union or Record value.", originOpt)

    let evalTuple _ argsEvaled _ (_ : 'w) =
        Tuple argsEvaled

    let evalPair _ (_ : string) argEvaled arg2Evaled (_ : 'w) =
        Tuple [|argEvaled; arg2Evaled|]

    let evalSome _ argEvaled _ (_ : 'w) =
        Option (Some argEvaled)
    
    let evalIsNone fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> violation
        | Option evaled -> Bool (Option.isNone evaled)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-Option.", originOpt)
    
    let evalIsSome fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> violation
        | Option evaled -> Bool (Option.isSome evaled)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-Option.", originOpt)

    let evalLeft _ argEvaled _ (_ : 'w) =
        Either (Left argEvaled)

    let evalRight _ argEvaled _ (_ : 'w) =
        Either (Right argEvaled)

    let evalIsLeft fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> violation
        | Either evaled -> Bool (Either.isLeft evaled)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-Either.", originOpt)

    let evalIsRight fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> violation
        | Either evaled -> Bool (Either.isRight evaled)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-Either.", originOpt)

    let evalCodata fnName argEvaled arg2Evaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> violation
        | Binding _ as binding -> Codata (Unfold (binding, arg2Evaled)) // evaled expr to binding implies extrinsic or intrinsic function
        | Fun _ as fn -> Codata (Unfold (fn, arg2Evaled))
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "First argument to " + fnName + " must be a Function.", originOpt)

    let rec evalCodataTryUncons evalApply fnName originOpt codata world =
        match codata with
        | Empty -> Right (Left ())
        | Add (left, right) ->
            match evalCodataTryUncons evalApply fnName originOpt left world with
            | Right (Right struct (_, _)) as success -> success
            | Right (Left ()) -> evalCodataTryUncons evalApply fnName originOpt right world
            | Left _ as error -> error
        | Unfold (unfolder, state) ->
            match evalApply [|unfolder; state|] originOpt world with
            | Violation _ as error -> Left error
            | Option (Some state) -> Right (Right struct (state, Unfold (unfolder, state)))
            | Option None -> Right (Left ())
            | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt)
        | Conversion (head :: []) -> Right (Right struct (head, Empty))
        | Conversion (head :: tail) -> Right (Right struct (head, Conversion tail))
        | Conversion [] -> Right (Left ())

    let rec evalCodataIsEmpty evalApply fnName originOpt codata world =
        match evalCodataTryUncons evalApply fnName originOpt codata world with
        | Right (Right _) -> Right false
        | Right (Left ()) -> Right true
        | Left error -> Left error

    let evalIsEmpty evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> violation
        | Bool bool -> Bool (not bool)
        | Int int -> Bool (int = 0)
        | Int64 int64 -> Bool (int64 = 0L)
        | Single single -> Bool (single = 0.0f)
        | Double double -> Bool (double = 0.0)
        | String str -> Bool (String.isEmpty str)
        | Keyword str -> Bool (String.isEmpty str)
        | Union (str, _) -> Bool (String.isEmpty str)
        | Option opt -> Bool (Option.isNone opt)
        | Either eir -> Bool (Either.isLeft eir)
        | Codata codata ->
            match evalCodataIsEmpty evalApply fnName originOpt codata world with
            | Right empty -> Bool empty
            | Left error -> error
        | List list -> Bool (List.isEmpty list)
        | Ring set -> Bool (Set.isEmpty set)
        | Table map -> Bool (Map.isEmpty map)
        | Record (str, _, _) -> Bool (String.isEmpty str)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalNotEmpty evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as violation -> violation
        | Bool bool -> Bool bool
        | Int int -> Bool (int <> 0)
        | Int64 int64 -> Bool (int64 <> 0L)
        | Single single -> Bool (single <> 0.0f)
        | Double double -> Bool (double <> 0.0)
        | String str -> Bool (String.notEmpty str)
        | Keyword str -> Bool (String.notEmpty str)
        | Union (str, _) -> Bool (String.notEmpty str)
        | Option opt -> Bool (Option.isSome opt)
        | Either eir -> Bool (Either.isRight eir)
        | Codata codata ->
            match evalCodataIsEmpty evalApply fnName originOpt codata world with
            | Right empty -> Bool (not empty)
            | Left error -> error
        | List list -> Bool (List.notEmpty list)
        | Ring set -> Bool (Set.notEmpty set)
        | Table map -> Bool (Map.notEmpty map)
        | Record (str, _, _) -> Bool (String.notEmpty str)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalTryUnconsInner evalApply fnName argEvaled originOpt world : Either<Expr, Either<unit, struct (Expr * Expr)>> =
        match argEvaled with
        | Violation _ as violation -> Left violation
        | String str ->
            if String.notEmpty str
            then Right (Right struct (String (string str.[0]), String (str.Substring 1)))
            else Right (Left ())
        | Option opt ->
            match opt with
            | Some value -> Right (Right struct (value, NoneValue))
            | None -> Right (Left ())
        | Either eir ->
            match eir with
            | Right value -> Right (Right struct (value, NoneValue))
            | Left _ -> Right (Left ())
        | Codata codata ->
            match evalCodataTryUncons evalApply fnName originOpt codata world with
            | Right (Right struct (head, tail)) -> Right (Right struct (head, Codata tail))
            | Right (Left ()) -> Right (Left ())
            | Left error -> Left error
        | List list ->
            match list with
            | [] -> Right (Left ())
            | head :: tail -> Right (Right struct (head, List tail))
        | Ring set ->
            match Seq.tryHead set with
            | Some head -> Right (Right struct (head, Ring (Set.remove head set)))
            | None -> Right (Left ())
        | Table map ->
            match Seq.tryHead map with
            | Some kvp -> Right (Right struct (Tuple [|kvp.Key; kvp.Value|], Table (Map.remove kvp.Key map)))
            | None -> Right (Left ())
        | _ -> Left $ Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalTryUncons evalApply fnName argEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argEvaled originOpt world with
        | Right (Right struct (head, tail)) -> Option (Some (Tuple [|head; tail|]))
        | Right (Left ()) -> NoneValue
        | Left error -> error

    let evalUncons evalApply fnName argEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argEvaled originOpt world with
        | Right (Right struct (head, tail)) -> Tuple [|head; tail|]
        | Right (Left ()) -> Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Cannot apply " + fnName + " to an empty container.", originOpt)
        | Left error -> error

    let evalCons fnName argEvaled arg2Evaled originOpt (_ : 'w) =
        match (argEvaled, arg2Evaled) with
        | (argEvaled, String str) ->
            match argEvaled with
            | Violation _ as violation -> violation
            | String head when String.length head = 1 -> String (head + str)
            | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 string arguments required where the first is of length 1.", originOpt)
        | (argEvaled, Option opt) ->
            match opt with
            | Some _ -> Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Cannot cons onto a some value.", originOpt)
            | None -> Option (Some argEvaled)
        | (argEvaled, Either eir) ->
            match eir with
            | Right _ -> Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Cannot cons onto a right value.", originOpt)
            | Left _ -> Either (Right argEvaled)
        | (argEvaled, List list) ->
            List (argEvaled :: list)
        | (argEvaled, Codata codata) ->
            match codata with
            | Empty -> Codata (Conversion [argEvaled])
            | Add _ -> Codata (Add (Conversion [argEvaled], codata))
            | Unfold _ -> Codata (Add (Conversion [argEvaled], codata))
            | Conversion list -> Codata (Conversion (argEvaled :: list))
        | (argEvaled, Ring set) ->
            Ring (Set.add argEvaled set)
        | (argEvaled, Table map) ->
            match argEvaled with
            | Violation _ as violation -> violation
            | Tuple elems when Array.length elems = 2 -> Table (Map.add elems.[0] elems.[1] map)
            | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Table entry must consist of a pair.", originOpt)
        | (Violation _ as violation, _) -> violation
        | (_, (Violation _ as violation)) -> violation
        | (_, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-List.", originOpt)

    let evalCommit fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as violation -> violation
        | Option _ -> argEvaled
        | Either _ -> argEvaled
        | Codata _ -> argEvaled
        | List list -> List (List.rev list)
        | Ring _ -> argEvaled
        | Table _ -> argEvaled
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalTryHead evalApply fnName argsEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argsEvaled originOpt world with
        | Right (Right struct (head, _)) -> head
        | Right (Left ()) -> NoneValue
        | Left error -> error

    let evalHead evalApply fnName argsEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argsEvaled originOpt world with
        | Right (Right struct (head, _)) -> head
        | Right (Left ()) -> Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Cannot apply " + fnName + " to a container with no elements.", originOpt)
        | Left error -> error

    let evalTryTail evalApply fnName argsEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argsEvaled originOpt world with
        | Right (Right struct (_, tail)) -> tail
        | Right (Left ()) -> NoneValue
        | Left error -> error

    let evalTail evalApply fnName argsEvaled originOpt world =
        match evalTryUnconsInner evalApply fnName argsEvaled originOpt world with
        | Right (Right struct (_, tail)) -> tail
        | Right (Left ()) -> Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Cannot apply " + fnName + " to a container with no elements.", originOpt)
        | Left error -> error

    let rec evalScanWhileCodata evalApply fnName originOpt scanner state codata world =
        match codata with
        | Empty ->
            Right struct (state, [])
        | Add (left, right) ->
            match evalScanWhileCodata evalApply fnName originOpt scanner state left world with
            | Right struct (state, statesLeft) ->
                match evalScanWhileCodata evalApply fnName originOpt scanner state right world with
                | Right struct (state, statesRight) -> Right struct (state, statesRight @ statesLeft)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | Violation _ as error -> Left error
            | Option (Some costate) ->
                match evalApply [|scanner; state; costate|] originOpt world with
                | Violation _ as error -> Left error
                | Option (Some state) -> evalScanWhileCodata evalApply fnName originOpt scanner state (Unfold (unfolder, costate)) world
                | Option None -> Right struct (state, [])
                | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt)
            | Option None -> Right struct (state, [])
            | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (state, states) elem ->
                match evalApply [|scanner; state; elem|] originOpt world with
                | Violation _ as error -> Left error
                | Option (Some state) -> (Right struct (state, state :: states))
                | Option None -> Left $ List states
                | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt))
                (Right struct (state, []))
                list

    let rec evalScaniCodata evalApply fnName originOpt i scanner state codata world =
        match codata with
        | Empty ->
            Right struct (i, state, [])
        | Add (left, right) ->
            match evalScaniCodata evalApply fnName originOpt (inc i) scanner state left world with
            | Right struct (i, state, statesLeft) ->
                match evalScaniCodata evalApply fnName originOpt (inc i) scanner state right world with
                | Right struct (i, state, statesRight) -> Right struct (i, state, statesRight @ statesLeft)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | Violation _ as error -> Left error
            | Option (Some costate) ->
                match evalApply [|scanner; Int i; state; costate|] originOpt world with
                | Violation _ as error -> Left error
                | state -> evalScaniCodata evalApply fnName originOpt (inc i) scanner state (Unfold (unfolder, costate)) world
            | Option None -> Right struct (i, state, [])
            | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (i, state, states) elem ->
                match evalApply [|scanner; Int i; state; elem|] originOpt world with
                | Violation _ as error -> Left error
                | Option (Some state) -> (Right struct (inc i, state, state :: states))
                | Option None -> Left $ List states
                | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt))
                (Right struct (i, state, []))
                list

    let rec evalScanCodata evalApply fnName originOpt scanner state codata world =
        match codata with
        | Empty ->
            Right struct (state, [])
        | Add (left, right) ->
            match evalScanCodata evalApply fnName originOpt scanner state left world with
            | Right struct (state, statesLeft) ->
                match evalScanCodata evalApply fnName originOpt scanner state right world with
                | Right struct (state, statesRight) -> Right struct (state, statesRight @ statesLeft)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | Violation _ as error -> Left error
            | Option (Some costate) ->
                match evalApply [|scanner; state; costate|] originOpt world with
                | Violation _ as error -> Left error
                | state -> evalScanCodata evalApply fnName originOpt scanner state (Unfold (unfolder, costate)) world
            | Option None -> Right struct (state, [])
            | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (state, states) elem ->
                match evalApply [|scanner; state; elem|] originOpt world with
                | Violation _ as error -> Left error
                | Option (Some state) -> (Right struct (state, state :: states))
                | Option None -> Left $ List states
                | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt))
                (Right struct (state, []))
                list

    let evalScanWhile evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (scanner, state, String str) ->
            match
                Seq.foldWhileRight (fun struct (state, states) elem ->
                    match evalApply [|scanner; state; String (string elem)|] originOpt world with
                    | Violation _ as error -> Left error
                    | Option (Some state) -> (Right struct (state, state :: states))
                    | Option None -> Left $ List states
                    | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt))
                    (Right struct (state, []))
                    str with
            | Right struct (_, states) -> List (List.rev states)
            | Left error -> error
        | (scanner, state, Codata codata) ->
            match evalScanWhileCodata evalApply fnName originOpt scanner state codata world with
            | Right struct (_, states) -> List (List.rev states)
            | Left error -> error
        | (scanner, state, List list) ->
            match
                Seq.foldWhileRight (fun struct (state, states) elem ->
                    match evalApply [|scanner; state; elem|] originOpt world with
                    | Violation _ as error -> Left error
                    | Option (Some state) -> (Right struct (state, state :: states))
                    | Option None -> Left $ List states
                    | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt))
                    (Right struct (state, []))
                    list with
            | Right struct (_, states) -> List (List.rev states)
            | Left error -> error
        | (scanner, state, Ring set) ->
            match
                Seq.foldWhileRight (fun struct (state, states) elem ->
                    match evalApply [|scanner; state; elem|] originOpt world with
                    | Violation _ as error -> Left error
                    | Option (Some state) -> (Right struct (state, state :: states))
                    | Option None -> Left $ List states
                    | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt))
                    (Right struct (state, []))
                    set with
            | Right struct (_, states) -> List (List.rev states)
            | Left error -> error
        | (scanner, state, Table map) ->
            match
                Seq.foldWhileRight (fun struct (state, states) (key, value) ->
                    let entry = Tuple [|key; value|]
                    match evalApply [|scanner; state; entry|] originOpt world with
                    | Violation _ as error -> Left error
                    | Option (Some state) -> (Right struct (state, state :: states))
                    | Option None -> Left $ List states
                    | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s scanner must return an Option.", originOpt))
                    (Right struct (state, []))
                    (Map.toList map) with
            | Right struct (_, states) -> List (List.rev states)
            | Left error -> error
        | (Violation _ as error, _, _) -> error
        | (_, (Violation _ as error), _) -> error
        | (_, _, (Violation _ as error)) -> error
        | (_, _, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalScani evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (scanner, state, String str) ->
            let struct (_, states) =
                Seq.foldi (fun i struct (state, states) elem ->
                    let state = evalApply [|scanner; Int i; state; String (string elem)|] originOpt world
                    struct (state, state :: states))
                    struct (state, [])
                    str
            List (List.rev states)
        | (scanner, state, Codata codata) ->
            match evalScaniCodata evalApply fnName originOpt 0 scanner state codata world with
            | Right struct (_, _, states) -> List (List.rev states)
            | Left error -> error
        | (scanner, state, List list) ->
            let struct (_, states) =
                Seq.foldi (fun i struct (state, states) elem ->
                    let state = evalApply [|scanner; Int i; state; elem|] originOpt world
                    struct (state, state :: states))
                    struct (state, [])
                    list
            List (List.rev states)
        | (scanner, state, Ring set) ->
            let struct (_, states) =
                Seq.foldi (fun i struct (state, states) elem ->
                    let state = evalApply [|scanner; Int i; state; elem|] originOpt world
                    struct (state, state :: states))
                    struct (state, [])
                    set
            List (List.rev states)
        | (scanner, state, Table map) ->
            let struct (_, states) =
                Seq.foldi (fun i struct (state, states) (key, value) ->
                    let entry = Tuple [|key; value|]
                    let state = evalApply [|scanner; Int i; state; entry|] originOpt world
                    struct (state, state :: states))
                    struct (state, [])
                    (Map.toList map)
            List (List.rev states)
        | (Violation _ as error, _, _) -> error
        | (_, (Violation _ as error), _) -> error
        | (_, _, (Violation _ as error)) -> error
        | (_, _, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalScan evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (scanner, state, String str) ->
            let struct (_, states) =
                Seq.fold (fun struct (state, states) elem ->
                    let state = evalApply [|scanner; state; String (string elem)|] originOpt world
                    struct (state, state :: states))
                    struct (state, [])
                    str
            List (List.rev states)
        | (scanner, state, Codata codata) ->
            match evalScanCodata evalApply fnName originOpt scanner state codata world with
            | Right struct (_, states) -> List (List.rev states)
            | Left error -> error
        | (scanner, state, List list) ->
            let struct (_, states) =
                Seq.fold (fun struct (state, states) elem ->
                    let state = evalApply [|scanner; state; elem|] originOpt world
                    struct (state, state :: states))
                    struct (state, [])
                    list
            List (List.rev states)
        | (scanner, state, Ring set) ->
            let struct (_, states) =
                Seq.fold (fun struct (state, states) elem ->
                    let state = evalApply [|scanner; state; elem|] originOpt world
                    struct (state, state :: states))
                    struct (state, [])
                    set
            List (List.rev states)
        | (scanner, state, Table map) ->
            let struct (_, states) =
                Seq.fold (fun struct (state, states) (key, value) ->
                    let entry = Tuple [|key; value|]
                    let state = evalApply [|scanner; state; entry|] originOpt world
                    struct (state, state :: states))
                    struct (state, [])
                    (Map.toList map)
            List (List.rev states)
        | (Violation _ as error, _, _) -> error
        | (_, (Violation _ as error), _) -> error
        | (_, _, (Violation _ as error)) -> error
        | (_, _, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let rec evalFoldWhileCodata evalApply fnName originOpt folder state codata world =
        match codata with
        | Empty ->
            Right state
        | Add (left, right) ->
            match evalFoldWhileCodata evalApply fnName originOpt folder state left world with
            | Right state -> evalFoldWhileCodata evalApply fnName originOpt folder state right world
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | Violation _ as error -> Left error
            | Option (Some costate) ->
                match evalApply [|folder; state; costate|] originOpt world with
                | Violation _ as error -> Left error
                | Option (Some state) -> evalFoldWhileCodata evalApply fnName originOpt folder state (Unfold (unfolder, costate)) world
                | Option None -> Right state
                | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt)
            | Option None -> Right state
            | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt)
        | Conversion list ->
            Seq.foldWhileRight (fun state elem ->
                match evalApply [|folder; state; elem|] originOpt world with
                | Violation _ as error -> Left error
                | Option (Some state) -> Right state
                | Option None -> Left state
                | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt))
                (Right state)
                list

    let rec evalFoldiCodata evalApply fnName originOpt i folder state codata world =
        match codata with
        | Empty ->
            Right struct (i, state)
        | Add (left, right) ->
            match evalFoldiCodata evalApply fnName originOpt (inc i) folder state left world with
            | Right struct (i, state) ->
                match evalFoldiCodata evalApply fnName originOpt (inc i) folder state right world with
                | Right struct (i, state) -> Right struct (i, state)
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | Violation _ as error -> Left error
            | Option (Some costate) ->
                match evalApply [|folder; Int i; state; costate|] originOpt world with
                | Violation _ as error -> Left error
                | state -> evalFoldiCodata evalApply fnName originOpt (inc i) folder state (Unfold (unfolder, costate)) world
            | Option None -> Right struct (i, state)
            | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt)
        | Conversion list ->
            Seq.foldWhileRight (fun struct (i, state) elem ->
                match evalApply [|folder; Int i; state; elem|] originOpt world with
                | Violation _ as error -> Left error
                | Option (Some state) -> (Right struct (inc i, state))
                | Option None -> Left state
                | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt))
                (Right struct (i, state))
                list

    let rec evalFoldCodata evalApply fnName originOpt folder state codata world =
        match codata with
        | Empty ->
            Right state
        | Add (left, right) ->
            match evalFoldCodata evalApply fnName originOpt folder state left world with
            | Right state ->
                match evalFoldCodata evalApply fnName originOpt folder state right world with
                | Right state -> Right state
                | error -> error
            | error -> error
        | Unfold (unfolder, costate) ->
            match evalApply [|unfolder; costate|] originOpt world with
            | Violation _ as error -> Left error
            | Option (Some costate) ->
                match evalApply [|folder; state; costate|] originOpt world with
                | Violation _ as error -> Left error
                | state -> evalFoldCodata evalApply fnName originOpt folder state (Unfold (unfolder, costate)) world
            | Option None -> Right state
            | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s unfolder must return an Option.", originOpt)
        | Conversion list ->
            Seq.foldWhileRight (fun state elem ->
                match evalApply [|folder; state; elem|] originOpt world with
                | Violation _ as error -> Left error
                | Option (Some state) -> (Right state)
                | Option None -> Left state
                | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt))
                (Right state)
                list

    let evalFoldWhile evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (folder, state, String str) ->
            let eir =
                Seq.foldWhileRight (fun state elem ->
                    match evalApply [|folder; state; String (string elem)|] originOpt world with
                    | Violation _ as error -> Left error
                    | Option (Some state) -> (Right state)
                    | Option None -> Left state
                    | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt))
                    (Right state)
                    str
            Either.amb eir
        | (folder, state, Codata codata) ->
            match evalFoldWhileCodata evalApply fnName originOpt folder state codata world with
            | Right success -> success
            | Left error -> error
        | (folder, state, List list) ->
            let eir =
                Seq.foldWhileRight (fun state elem ->
                    match evalApply [|folder; state; elem|] originOpt world with
                    | Violation _ as error -> Left error
                    | Option (Some state) -> (Right state)
                    | Option None -> Left state
                    | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt))
                    (Right state)
                    list
            Either.amb eir
        | (folder, state, Ring set) ->
            let eir =
                Seq.foldWhileRight (fun state elem ->
                    match evalApply [|folder; state; elem|] originOpt world with
                    | Option (Some state) -> (Right state)
                    | Option None -> Left state
                    | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt))
                    (Right state)
                    set
            Either.amb eir
        | (folder, state, Table map) ->
            let eir =
                Seq.foldWhileRight (fun state (key, value) ->
                    let entry = Tuple [|key; value|]
                    match evalApply [|folder; state; entry|] originOpt world with
                    | Option (Some state) -> (Right state)
                    | Option None -> Left state
                    | _ -> Left $ Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s folder must return an Option.", originOpt))
                    (Right state)
                    (Map.toList map)
            Either.amb eir
        | (Violation _ as error, _, _) -> error
        | (_, (Violation _ as error), _) -> error
        | (_, _, (Violation _ as error)) -> error
        | (_, _, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalFoldi evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (folder, state, String str) -> Seq.foldi (fun i state elem -> evalApply [|folder; Int i; state; String (string elem)|] originOpt world) state str
        | (folder, state, Codata codata) ->
            match evalFoldiCodata evalApply fnName originOpt 0 folder state codata world with
            | Right struct (_, state) -> state
            | Left error -> error
        | (folder, state, List list) -> Seq.foldi (fun i state elem -> evalApply [|folder; Int i; state; elem|] originOpt world) state list
        | (folder, state, Ring set) -> Seq.foldi (fun i state elem -> evalApply [|folder; Int i; state; elem|] originOpt world) state set
        | (folder, state, Table map) ->
            Seq.foldi (fun i state (key, value) ->
                let entry = Tuple [|key; value|]
                evalApply [|folder; Int i; state; entry|] originOpt world)
                state
                (Map.toList map)
        | (_, _, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalFold evalApply fnName argEvaled arg2Evaled arg3Evaled originOpt world =
        match (argEvaled, arg2Evaled, arg3Evaled) with
        | (folder, state, String str) -> Seq.fold (fun state elem -> evalApply [|folder; state; String (string elem)|] originOpt world) state str
        | (folder, state, Codata codata) ->
            match evalFoldCodata evalApply fnName originOpt folder state codata world with
            | Right state -> state
            | Left error -> error
        | (folder, state, List list) -> List.fold (fun state elem -> evalApply [|folder; state; elem|] originOpt world) state list
        | (folder, state, Ring set) -> Set.fold (fun state elem -> evalApply [|folder; state; elem|] originOpt world) state set
        | (folder, state, Table map) ->
            Map.fold (fun state key value ->
                let entry = Tuple [|key; value|]
                evalApply [|folder; state; entry|] originOpt world)
                state
                map
        | (Violation _ as error, _, _) -> error
        | (_, (Violation _ as error), _) -> error
        | (_, _, (Violation _ as error)) -> error
        | (_, _, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let rec evalMapCodata evalApply originOpt mapper codata (world : 'w) : Codata =
        match codata with
        | Empty ->
            codata
        | Add (left, right) ->
            let leftMapped = evalMapCodata evalApply originOpt mapper left world
            let rightMapped = evalMapCodata evalApply originOpt mapper right world
            Add (leftMapped, rightMapped)
        | Unfold (unfolder, codata) ->
            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
            let args = [|unfolder; Binding ("state", ref UncachedBinding, ref EnvironmentalBinding, originOpt)|]
            let unfolder = Unfold (Fun ([|"state"|], 1, Apply (args, breakpoint, originOpt), false, None, None, originOpt), codata)
            unfolder
        | Conversion list ->
            let mapped =
                List.fold (fun elems elem ->
                    let elem = evalApply [|mapper; elem|] originOpt world
                    elem :: elems)
                    []
                    list
            Conversion (List.rev mapped)

    let evalMapi evalApply fnName argEvaled arg2Evaled originOpt world =
        match (argEvaled, arg2Evaled) with
        | (mapper, (Option opt as option)) ->
            match opt with
            | Some value -> evalApply [|mapper; Int 0; value|] originOpt world
            | None -> option
        | (mapper, (Either eir as either)) ->
            match eir with
            | Right value -> evalApply [|mapper; Int 0; value|] originOpt world
            | Left _ -> either
        | (mapper, String str) ->
            let list =
                Seq.foldi (fun i elems elem ->
                    let elem = String (string elem)
                    let elem = evalApply [|mapper; Int i; elem|] originOpt world
                    (elem :: elems))
                    [] str
            if List.forall (function String str when String.length str = 1 -> true | _ -> false) list
            then String (list |> List.rev |> List.map (function String str -> str.[0] | _ -> failwithumf ()) |> String.implode)
            else Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s mapper must return a String of length 1.", originOpt)
        | (mapper, Codata codata) ->
            let codata = evalMapCodata evalApply originOpt mapper codata world
            Codata codata
        | (mapper, List list) ->
            let listRev =
                Seq.foldi (fun i elems elem ->
                    let elem = evalApply [|mapper; Int i; elem|] originOpt world
                    elem :: elems)
                    []
                    list
            List (List.rev listRev)
        | (mapper, Ring set) ->
            let set =
                Seq.foldi (fun i elems elem ->
                    let elem = evalApply [|mapper; Int i; elem|] originOpt world
                    Set.add elem elems)
                    Set.empty
                    set
            Ring set
        | (mapper, Table map) ->
            let map =
                Seq.foldi (fun i elems (key, value) ->
                    let entry = Tuple [|key; value|]
                    let entry = evalApply [|mapper; Int i; entry|] originOpt world
                    match entry with
                    | Tuple elems' when Array.length elems' = 2 -> Map.add elems'.[0] elems'.[1] elems
                    | _ -> elems)
                    Map.empty
                    (Map.toList map)
            Table map
        | (Violation _ as error, _) -> error
        | (_, (Violation _ as error)) -> error
        | (_, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalMap evalApply fnName argEvaled arg2Evaled originOpt world =
        match (argEvaled, arg2Evaled) with
        | (mapper, (Option opt as option)) ->
            match opt with
            | Some value -> evalApply [|mapper; value|] originOpt world
            | None -> option
        | (mapper, (Either eir as either)) ->
            match eir with
            | Right value -> evalApply [|mapper; value|] originOpt world
            | Left _ -> either
        | (mapper, String str) ->
            let list =
                Seq.fold (fun elems elem ->
                    let elem = String (string elem)
                    let elem = evalApply [|mapper; elem|] originOpt world
                    elem :: elems)
                    [] str
            if List.forall (function String str when String.length str = 1 -> true | _ -> false) list
            then String (list |> List.rev |> List.map (function String str -> str.[0] | _ -> failwithumf ()) |> String.implode)
            else Violation (["InvalidResult"; String.capitalize fnName], "Function " + fnName + "'s mapper must return a String of length 1.", originOpt)
        | (mapper, Codata codata) ->
            let codata = evalMapCodata evalApply originOpt mapper codata world
            Codata codata
        | (mapper, List list) ->
            let listRev =
                List.fold (fun elems elem ->
                    let elem = evalApply [|mapper; elem|] originOpt world
                    elem :: elems)
                    []
                    list
            List (List.rev listRev)
        | (mapper, Ring set) ->
            let set =
                Set.fold (fun elems elem ->
                    let elem = evalApply [|mapper; elem|] originOpt world
                    Set.add elem elems)
                    Set.empty
                    set
            Ring set
        | (mapper, Table map) ->
            let map =
                Map.fold (fun elems key value ->
                    let entry = Tuple [|key; value|]
                    let entry = evalApply [|mapper; entry|] originOpt world
                    match entry with
                    | Tuple elems' when Array.length elems' = 2 -> Map.add elems'.[0] elems'.[1] elems
                    | _ -> elems)
                    Map.empty
                    map
            Table map
        | (Violation _ as error, _) -> error
        | (_, (Violation _ as error)) -> error
        | (_, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let rec evalContainsCodata evalApply fnName argEvaled originOpt codata world =
        match codata with
        | Empty -> Right false
        | Add (left, right) ->
            match evalContainsCodata evalApply fnName argEvaled originOpt left world with
            | Right false -> evalContainsCodata evalApply fnName argEvaled originOpt right world
            | Right true as success -> success
            | Left _ as error -> error
        | Unfold (unfolder, state) ->
            match evalApply [|unfolder; state|] originOpt world with
            | Option (Some state) ->
                if state <> argEvaled then
                    let codata = Unfold (unfolder, state)
                    evalContainsCodata evalApply fnName argEvaled originOpt codata world
                else Right true
            | Option None -> Right false
            | error -> Left error
        | Conversion list ->
            Right (List.contains argEvaled list)

    let evalContains evalApply fnName argEvaled arg2Evaled originOpt world =
        match (argEvaled, arg2Evaled) with
        | (argEvaled, String str) ->
            match argEvaled with
            | String str' -> Bool (str.Contains str')
            | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "First argument to " + fnName + " for a String must also be a String.", originOpt)
        | (argEvaled, Option opt) -> Bool (match opt with Some value -> value = argEvaled | None -> false)
        | (argEvaled, Either eir) -> Bool (match eir with Right value -> value = argEvaled | Left _ -> false)
        | (argEvaled, Codata codata) ->
            match evalContainsCodata evalApply fnName argEvaled originOpt codata world with
            | Right bool -> Bool bool
            | Left error -> error
        | (argEvaled, List list) -> Bool (List.contains argEvaled list)
        | (argEvaled, Ring set) -> Bool (Set.contains argEvaled set)
        | (Violation _ as error, _) -> error
        | (_, (Violation _ as error)) -> error
        | (_, _) -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalPure fnName argsEvaled originOpt (_ : 'w) =
        match argsEvaled with
        | [|Violation _ as violation; _|] -> violation
        | [|_; Violation _ as violation|] -> violation
        | [|String _; value|] ->
            match value with
            | String str as string when str.Length = 1 -> string
            | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " for String must be a String of length 1.", originOpt)
        | [|Option _; value|] -> Option (Some value)
        | [|Either _; value|] -> Either (Right value)
        | [|Codata _; value|] -> Codata (Conversion [value])
        | [|List _; value|] -> List [value]
        | [|Ring _; value|] -> Ring (Set.singleton value)
        | [|Table _; value|] ->
            match value with
            | Tuple [|v1; v2|] -> Table (Map.singleton v1 v2)
            | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " for Table must be a 2-value Tuple.", originOpt)
        | [|Fun _; value|] -> Fun ([||], 0, value, false, None, None, ValueNone)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for a function, String, Option, Codata, Ring, Table or List.", originOpt)

    let evalApplyScript evalApply fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|Violation _ as violation; _|] -> violation
        | [|_; Violation _ as violation|] -> violation
        | [|Fun (_, 1, _, _, _, _, _) as fn; value|] -> evalApply [|fn; value|] originOpt world
        | [|Option fnOpt; Option valueOpt|] ->
            match (fnOpt, valueOpt) with
            | (Some fn, Some value) -> evalApply [|fn; value|] originOpt world
            | (_, _) -> NoneValue
        | [|Either fnEir; Either valueEir|] ->
            match (fnEir, valueEir) with
            | (Right fn, Right value) -> evalApply [|fn; value|] originOpt world
            | (Left value, _) -> Either (Left value)
            | (_, Left value) -> Either (Left value)
        | [|List fns; List values|] ->
            let resultsRevOpt =
                List.foldWhileRight
                    (fun resultsRev value ->
                        let resultOpt =
                            List.foldWhileRight
                                (fun result fn ->
                                    let result' = evalApply [|fn; result|] originOpt world
                                    match result' with
                                    | Violation _ as v -> Left v
                                    | _ -> Right result')
                                (Right value)
                                fns
                        match resultOpt with
                        | Right result -> Right (result :: resultsRev)
                        | Left violation -> Left violation)
                    (Right [])
                    values
            match resultsRevOpt with
            | Right resultsRev -> List (List.rev resultsRev)
            | Left violation -> violation
        | [|Codata _; Codata _|] -> Violation (["NotImplemented"; String.capitalize fnName], "Function '" + fnName + "' is not implemented for Codata.", originOpt)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for an unary function, two Options, two Eithers, two Codata, or two Lists.", originOpt)

    let evalBind evalApply fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|Violation _ as violation; _|] -> violation
        | [|_; Violation _ as violation|] -> violation
        | [|Option valueOpt; fn|] ->
            match valueOpt with
            | Some value -> evalApply [|fn; value|] originOpt world
            | None -> NoneValue
        | [|Either valueEir as eir; fn|] ->
            match valueEir with
            | Right value -> evalApply [|fn; value|] originOpt world
            | Left _ -> eir
        | [|List values; fn|] ->
            let resultsRevOpt =
                List.foldWhileRight
                    (fun resultsRev value ->
                        let result = evalApply [|fn; value|] originOpt world
                        match result with
                        | Violation _ as v -> Left v
                        | _ -> Right (result :: resultsRev))
                    (Right [])
                    values
            match resultsRevOpt with
            | Right resultsRev -> List (List.rev resultsRev)
            | Left violation -> violation
        | [|Codata _; Codata _|] -> Violation (["NotImplemented"; String.capitalize fnName], "Function '" + fnName + "' is not implemented for Codata.", originOpt)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for two Options, two Eithers, two Codata, or two Lists.", originOpt)

    let evalMap2 evalApply fnName argsEvaled originOpt world =
        match argsEvaled with
        | [|Violation _ as violation; _; _|] -> violation
        | [|_; Violation _ as violation; _|] -> violation
        | [|_; _; Violation _ as violation|] -> violation
        | [|fn; Option leftOpt; Option rightOpt|] ->
            match (leftOpt, rightOpt) with
            | (Some left, Some right) -> evalApply [|fn; left; right|] originOpt world
            | (_, _) -> NoneValue
        | [|fn; Either leftEir; Either rightEir|] ->
            match (leftEir, rightEir) with
            | (Right left, Right right) -> evalApply [|fn; left; right|] originOpt world
            | (_, _) -> Either leftEir 
        | [|fn; List leftList; List rightList|] ->
            let listRev =
                List.fold2 (fun elems elem elem2 ->
                    let elem = evalApply [|fn; elem; elem2|] originOpt world
                    elem :: elems)
                    []
                    leftList
                    rightList
            List (List.rev listRev)
        | [|Codata _; Codata _|] -> Violation (["NotImplemented"; String.capitalize fnName], "Function '" + fnName + "' is not implemented for Codata.", originOpt)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for two Options, two Eithers, two Codata, or two Lists.", originOpt)

    let evalProduct fnName argsEvaled originOpt (_ : 'w) =
        match argsEvaled with
        | [|Violation _ as violation; _|] -> violation
        | [|_; Violation _ as violation|] -> violation
        | [|Option leftOpt; Option rightOpt|] ->
            match (leftOpt, rightOpt) with
            | (Some left, Some right) -> Option (Some (Tuple [|left; right|]))
            | (_, _) -> NoneValue
        | [|Either leftEir; Either rightEir|] ->
            match (leftEir, rightEir) with
            | (Right left, Right right) -> Either (Right (Tuple [|left; right|]))
            | (_, _) -> Either leftEir
        | [|List leftList; List rightList|] ->
            let min = Math.Min (List.length leftList, List.length rightList)
            let leftList = List.truncate min leftList
            let rightList = List.truncate min rightList
            let list = List.map2 (fun left right -> Tuple [|left; right|]) leftList rightList
            List list
        | [|Codata _; Codata _|] -> Violation (["NotImplemented"; String.capitalize fnName], "Function '" + fnName + "' is not implemented for Codata.", originOpt)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for two Options, two Eithers, two Codata, or two Lists.", originOpt)

    let evalSum fnName argsEvaled originOpt (_ : 'w) =
        match argsEvaled with
        | [|Violation _ as violation; _|] -> violation
        | [|_; Violation _ as violation|] -> violation
        | [|Option leftOpt; Option rightOpt|] ->
            match (leftOpt, rightOpt) with
            | (Some value, None) -> Option (Some (Either (Right value)))
            | (None, Some value) -> Option (Some (Either (Right value)))
            | (_, _) -> Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Function '" + fnName + "' requires one and only one some value.", originOpt)
        | [|Either leftEir; Either rightEir|] ->
            match (leftEir, rightEir) with
            | (Right value, Left _) -> Either (Right (Either (Right value)))
            | (Left _, Right value) -> Either (Right (Either (Right value)))
            | (_, _) -> Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Function '" + fnName + "' requires one and only one right value.", originOpt)
        | [|List leftList; List rightList|] ->
            let min = Math.Min (List.length leftList, List.length rightList)
            let leftList = leftList |> List.truncate min |> List.map (fun elem -> Either (Left elem))
            let rightList = rightList |> List.truncate min |> List.map (fun elem -> Either (Right elem))
            let list = leftList @ rightList
            List list
        | [|Codata _; Codata _|] -> Violation (["NotImplemented"; String.capitalize fnName], "Function '" + fnName + "' is not implemented for Codata.", originOpt)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Native application of " + fnName + " must be used for two Options, two Eithers, two Codata, or two Lists.", originOpt)

    let evalToString fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as error -> error
        | String _ as str -> str
        | List list ->
            if List.forall (function String str when str.Length = 1 -> true | _ -> false) list then
                let chars = List.map (function String str -> str | _ -> failwithumf ()) list
                String (String.Join ("", chars))
            else Violation (["InvalidArgumentType"; String.capitalize fnName], "Function " + fnName + " cannot only be applied to single character strings.", originOpt)
        | Ring set ->
            if Set.forall (function String str when str.Length = 1 -> true | _ -> false) set then
                let chars = Seq.map (function String str -> str | _ -> failwithumf ()) set
                String (String.Join ("", chars))
            else Violation (["InvalidArgumentType"; String.capitalize fnName], "Function " + fnName + " cannot only be applied to single character strings.", originOpt)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalToCodata fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as error -> error
        | Option opt -> Codata (Conversion (match opt with Some value -> [value] | None -> []))
        | Either eir -> Codata (Conversion (match eir with Right value -> [value] | Left _ -> []))
        | Codata _ -> argEvaled
        | List list -> Codata (Conversion list)
        | Ring set -> Codata (Conversion (Set.toList set))
        | Table map -> Codata (Conversion (Map.toListBy (fun key value -> Tuple [|key; value|]) map))
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalList _ argsEvaled _ (_ : 'w) =
        List (List.ofArray argsEvaled)

    let rec evalCodataToList evalApply fnName originOpt list codata world =
        match evalCodataTryUncons evalApply fnName originOpt codata world with
        | Right (Right struct (head, tail)) -> evalCodataToList evalApply fnName originOpt (head :: list) tail world
        | Right (Left ()) -> Right (Left list)
        | Left error -> Left (error)

    let evalToList evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as error -> error
        | String str -> List (str |> Seq.map (string >> String) |> List.ofSeq)
        | Option opt -> List (match opt with Some value -> [value] | None -> [])
        | Either eir -> List (match eir with Right value -> [value] | Left _ -> [])
        | Codata codata ->
            match evalCodataToList evalApply fnName originOpt [] codata world with
            | Right (Right struct (_, _, list)) -> List (List.rev list)
            | Right (Left list) -> List (List.rev list)
            | Left error -> error
        | List _ as list -> list
        | Ring set -> List (List.ofSeq set)
        | Table map -> List (map |> Map.toListBy (fun k v -> Tuple [|k; v|]))
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalRing _ argsEvaled _ (_ : 'w) =
        Ring (Set.ofArray argsEvaled)

    let evalToRing evalApply fnName argEvaled originOpt world =
        match argEvaled with
        | Violation _ as error -> error
        | String str -> Ring (str |> Seq.map (string >> String) |> Set.ofSeq)
        | Option opt -> Ring (match opt with Some value -> Set.singleton value | None -> Set.empty)
        | Either eir -> Ring (match eir with Right value -> Set.singleton value | Left _ -> Set.empty)
        | Codata codata ->
            match evalCodataToList evalApply fnName originOpt [] codata world with
            | Right (Right struct (_, _, list)) -> Ring (Set.ofList list)
            | Right (Left list) -> Ring (Set.ofList list)
            | Left error -> error
        | List list -> Ring (Set.ofList list)
        | Ring _ as ring -> ring
        | Table map -> Ring (map |> Map.toSeqBy (fun k v -> Tuple [|k; v|]) |> Set.ofSeq)
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-container.", originOpt)

    let evalRemove fnName argEvaled arg2Evaled originOpt (_ : 'w) =
        match (argEvaled, arg2Evaled) with
        | (value, container) ->
            match container with
            | Violation _ as error -> error
            | String str ->
                match value with
                | Violation _ as error -> error
                | String str2 -> String (str.Replace (str2, ""))
                | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect type of argument for " + fnName + "; argument must be string.", originOpt)
            | Option opt -> Option (match opt with Some value2 -> (if Expr.equals value value2 then None else opt) | None -> None)
            | List list -> List (List.remove ((=) value) list)
            | Ring set -> Ring (Set.remove value set)
            | Table map -> Table (Map.remove value map)
            | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Incorrect type of argument for '" + fnName + "'; target must be a container.", originOpt)

    let evalToTable fnName argEvaled originOpt (_ : 'w) =
        match argEvaled with
        | Violation _ as error -> error
        | List list ->
            if List.forall (function Tuple [|_; _|] -> true | _ -> false) list then
                let pairs = List.map (function Tuple [|k; v|] -> (k, v) | _ -> failwithumf ()) list
                Table (Map.ofList pairs)
            else Violation (["InvalidArgumentType"; String.capitalize fnName], "Function " + fnName + " cannot only be applied to container of pairs.", originOpt)
        | Ring set ->
            if Set.forall (function Tuple [|_; _|] -> true | _ -> false) set then
                let pairs = Seq.map (function Tuple [|k; v|] -> (k, v) | _ -> failwithumf ()) set
                Table (Map.ofSeq pairs)
            else Violation (["InvalidArgumentType"; String.capitalize fnName], "Function " + fnName + " cannot only be applied to container of pairs.", originOpt)
        | Table _ as table -> table
        | _ -> Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-string or non-container.", originOpt)
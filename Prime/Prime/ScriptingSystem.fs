// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open System.Collections.Generic
open System.IO
open Prime
open Prime.Scripting
open Prime.ScriptingUnary
open Prime.ScriptingBinary
open Prime.ScriptingMarshalling
open Prime.ScriptingPrimitives

/// The context in which scripting takes place. Effectively a mix-in for the 'w type, where 'w is a type that
/// represents the client program.
type ScriptingSystem<'w when 'w :> 'w ScriptingSystem> =
    interface
        abstract member GetEnv : unit -> Env
        abstract member TryGetExtrinsic : string -> 'w ScriptingTrinsic FOption
        abstract member TryImport : Type -> obj -> Expr option
        abstract member TryExport : Type -> Expr -> obj option
        end

/// The type for intrinsic and extrinsic scripting functions.
and [<NoEquality; NoComparison>] ScriptingTrinsic<'w when 'w :> 'w ScriptingSystem> =
    { Fn : string -> Expr array -> SymbolOrigin option -> 'w -> struct (Expr * 'w)
      Pars : string array
      DocOpt : string option }

[<RequireQualifiedAccess>]
module ScriptingSystem =

    let mutable private Intrinsics =
        Unchecked.defaultof<obj>

    let private toOverloadName fnName typeName =
        // TODO: can we blit this into a global mutable string for most cases in order to avoid allocation?
        fnName + "_" + typeName

    let inline annotateWorld<'w when 'w :> 'w ScriptingSystem> (_ : 'w) =
        () // NOTE: simply infers that a type is a world.

    let tryGetBinding<'w when 'w :> 'w ScriptingSystem> name cachedBinding bindingType (world : 'w) =
        Env.tryGetBinding name cachedBinding bindingType (world.GetEnv ())

    let tryAddDeclarationBinding<'w when 'w :> 'w ScriptingSystem> name value (world : 'w) =
        Env.tryAddDeclarationBinding name value (world.GetEnv ())

    let addProceduralBinding<'w when 'w :> 'w ScriptingSystem> appendType name value (world : 'w) =
        Env.addProceduralBinding appendType name value (world.GetEnv ())

    let addProceduralBindings<'w when 'w :> 'w ScriptingSystem> appendType bindings (world : 'w) =
        Env.addProceduralBindings appendType bindings (world.GetEnv ())

    let removeProceduralBindings<'w when 'w :> 'w ScriptingSystem> (world : 'w) =
        Env.removeProceduralBindings (world.GetEnv ())

    let getProceduralFrames<'w when 'w :> 'w ScriptingSystem> (world : 'w) =
        Env.getProceduralFrames (world.GetEnv ())

    let setProceduralFrames<'w when 'w :> 'w ScriptingSystem> proceduralFrames (world : 'w) =
        Env.setProceduralFrames proceduralFrames (world.GetEnv ())

    let getGlobalFrame<'w when 'w :> 'w ScriptingSystem> (world : 'w) =
        Env.getGlobalFrame (world.GetEnv ())

    let getLocalFrame<'w when 'w :> 'w ScriptingSystem> (world : 'w) =
        Env.getLocalFrame (world.GetEnv ())

    let setLocalFrame<'w when 'w :> 'w ScriptingSystem> localFrame (world : 'w) =
        Env.setLocalFrame localFrame (world.GetEnv ())

    let tryImport<'w when 'w :> 'w ScriptingSystem> ty value (world : 'w) =
        tryImport world.TryImport ty value

    let tryExport<'w when 'w :> 'w ScriptingSystem> ty value (world : 'w) =
        tryExport world.TryExport ty value

    let log expr =
        match expr with
        | Violation (names, error, originOpt) ->
            Log.info
                ("Unexpected Violation: " + String.concat Constants.Scripting.ViolationSeparatorStr names + "\n" +
                 "Due to: " + error + "\n" +
                 SymbolOrigin.tryPrint originOpt + "\n")
        | _ -> ()

    let rec getIntrinsics<'w when 'w :> 'w ScriptingSystem> () =
        if isNull Intrinsics then
            let intrinsics =
                [("=", { Fn = evalBinary EqFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine equality." })
                 ("<>", { Fn = evalBinary NotEqFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine inequality." })
                 ("<", { Fn = evalBinary LtFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine ascending relationship." })
                 (">", { Fn = evalBinary GtFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine descending relationship." })
                 ("<=", { Fn = evalBinary LtEqFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine ascending relationship or equality." })
                 (">=", { Fn = evalBinary GtEqFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine descending relationship or equality." })
                 ("+", { Fn = evalBinary AddFns; Pars = [|"a"; "b"|]; DocOpt = Some "Add." })
                 ("-", { Fn = evalBinary SubFns; Pars = [|"a"; "b"|]; DocOpt = Some "Subtract." })
                 ("*", { Fn = evalBinary MulFns; Pars = [|"a"; "b"|]; DocOpt = Some "Multiply." })
                 ("/", { Fn = evalBinary DivFns; Pars = [|"a"; "b"|]; DocOpt = Some "Divide." })
                 ("%", { Fn = evalBinary ModFns; Pars = [|"a"; "b"|]; DocOpt = Some "Modulate." })
                 ("!", { Fn = evalSinglet evalDereference; Pars = [|"a"|]; DocOpt = Some "Dereference." })
                 ("not", { Fn = evalBoolUnary not; Pars = [|"a"|]; DocOpt = Some "Invert." })
                 ("hash", { Fn = evalUnary HashFns; Pars = [|"a"|]; DocOpt = Some "Hash." })
                 ("empty", { Fn = evalUnary EmptyFns; Pars = [|"typeIndicator"; "a"|]; DocOpt = Some "Construct empty value." })
                 ("identity", { Fn = evalUnary IdentityFns; Pars = [|"typeIndicator"|]; DocOpt = Some "Construct an identity value." })
                 ("minimum", { Fn = evalUnary MinimumFns; Pars = [|"typeIndicator"|]; DocOpt = Some "Construct a minimum value." })
                 ("maximum", { Fn = evalUnary MaximumFns; Pars = [|"typeIndicator"|]; DocOpt = Some "Construct a maximum value." })
                 ("inc", { Fn = evalUnary IncFns; Pars = [|"a"|]; DocOpt = Some "Increment." })
                 ("dec", { Fn = evalUnary DecFns; Pars = [|"a"|]; DocOpt = Some "Decrement." })
                 ("negate", { Fn = evalUnary NegateFns; Pars = [|"a"|]; DocOpt = Some "Negate." })
                 ("pow", { Fn = evalBinary PowFns; Pars = [|"a"; "b"|]; DocOpt = Some "Raise to power." })
                 ("root", { Fn = evalBinary RootFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine root." })
                 ("sqr", { Fn = evalUnary SqrFns; Pars = [|"a"|]; DocOpt = Some "Square." })
                 ("sqrt", { Fn = evalUnary SqrtFns; Pars = [|"a"|]; DocOpt = Some "Determine square root." })
                 ("floor", { Fn = evalUnary FloorFns; Pars = [|"a"|]; DocOpt = Some "Determine floor." })
                 ("ceiling", { Fn = evalUnary CeilingFns; Pars = [|"a"|]; DocOpt = Some "Determine ceiling." })
                 ("truncate", { Fn = evalUnary TruncateFns; Pars = [|"a"|]; DocOpt = Some "Truncate." })
                 ("round", { Fn = evalUnary RoundFns; Pars = [|"a"|]; DocOpt = Some "Round." })
                 ("exp", { Fn = evalUnary ExpFns; Pars = [|"a"|]; DocOpt = Some "Exponentiate." })
                 ("log", { Fn = evalUnary LogFns; Pars = [|"a"|]; DocOpt = Some "Determine log." })
                 ("sin", { Fn = evalUnary SinFns; Pars = [|"a"|]; DocOpt = Some "Determine sine." })
                 ("cos", { Fn = evalUnary CosFns; Pars = [|"a"|]; DocOpt = Some "Determine cosine." })
                 ("tan", { Fn = evalUnary TanFns; Pars = [|"a"|]; DocOpt = Some "Determine tangent." })
                 ("asin", { Fn = evalUnary AsinFns; Pars = [|"a"|]; DocOpt = Some "Determine sine derivative." })
                 ("acos", { Fn = evalUnary AcosFns; Pars = [|"a"|]; DocOpt = Some "Determine cosine derivative." })
                 ("atan", { Fn = evalUnary AtanFns; Pars = [|"a"|]; DocOpt = Some "Determine tangent derivative." })
                 ("length", { Fn = evalUnary LengthFns; Pars = [|"a"|]; DocOpt = Some "Determine length." })
                 ("normal", { Fn = evalUnary NormalFns; Pars = [|"a"|]; DocOpt = Some "Normalize." })
                 ("cross", { Fn = evalBinary CrossFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine cross product." })
                 ("dot", { Fn = evalBinary DotFns; Pars = [|"a"; "b"|]; DocOpt = Some "Determine dot product." })
                 ("bool", { Fn = evalUnary BoolFns; Pars = [|"a"|]; DocOpt = Some "Convert to Bool." })
                 ("int", { Fn = evalUnary IntFns; Pars = [|"a"|]; DocOpt = Some "Convert to Int." })
                 ("int64", { Fn = evalUnary Int64Fns; Pars = [|"a"|]; DocOpt = Some "Convert to Int64." })
                 ("single", { Fn = evalUnary SingleFns; Pars = [|"a"|]; DocOpt = Some "Convert to Single." })
                 ("double", { Fn = evalUnary DoubleFns; Pars = [|"a"|]; DocOpt = Some "Convert to Double." })
                 ("string", { Fn = evalUnary StringFns; Pars = [|"a"|]; DocOpt = Some "Convert to String." })
                 ("getTypeName", { Fn = evalSinglet evalGetTypeName; Pars = [|"a"|]; DocOpt = Some "Determine the name of a value's type." })
                 ("tryIndex", { Fn = evalDoublet evalTryIndex; Pars = [|"index"; "context"|]; DocOpt = Some "Attempt to index into a context." })
                 ("hasIndex", { Fn = evalDoublet evalHasIndex; Pars = [|"index"; "context"|]; DocOpt = Some "Determine that a context contain an index." })
                 ("index", { Fn = evalDoublet evalIndex; Pars = [|"index"; "context"|]; DocOpt = Some "Index into a context." })
                 ("getName", { Fn = evalSinglet evalGetName; Pars = [|"a"|]; DocOpt = Some "Determine the name of a Union or Record." })
                 ("tuple", { Fn = evalTuple; Pars = [|"..."|]; DocOpt = Some "Construct a Tuple." })
                 ("pair", { Fn = evalTuple; Pars = [|"a"; "b"|]; DocOpt = Some "Construct a 2-Tuple." })
                 ("fst", { Fn = evalSinglet (evalIndexInt 0); Pars = [|"context"|]; DocOpt = Some "Index the first value of a context." })
                 ("snd", { Fn = evalSinglet (evalIndexInt 1); Pars = [|"context"|]; DocOpt = Some "Index the second value of a context." })
                 ("thd", { Fn = evalSinglet (evalIndexInt 2); Pars = [|"context"|]; DocOpt = Some "Index the third value of a context." })
                 ("fth", { Fn = evalSinglet (evalIndexInt 3); Pars = [|"context"|]; DocOpt = Some "Index the fourth value of a context." })
                 ("fif", { Fn = evalSinglet (evalIndexInt 4); Pars = [|"context"|]; DocOpt = Some "Index the fifth value of a context." })
                 ("nth", { Fn = evalDoublet evalNth; Pars = [|"index"; "context"|]; DocOpt = Some "Index the nth value of a context." })
                 ("some", { Fn = evalSinglet evalSome; Pars = [|"a"|]; DocOpt = Some "Construct an inhabited Option." })
                 ("isNone", { Fn = evalSinglet evalIsNone; Pars = [|"opt"|]; DocOpt = Some "Determine if an Option is uninhabited." })
                 ("isSome", { Fn = evalSinglet evalIsSome; Pars = [|"opt"|]; DocOpt = Some "Determine if an Option is inhabited." })
                 ("left", { Fn = evalSinglet evalLeft; Pars = [|"a"|]; DocOpt = Some "Construct a left-inhabited Either." })
                 ("right", { Fn = evalSinglet evalRight; Pars = [|"a"|]; DocOpt = Some "Construct a right-inhabited Either." })
                 ("isLeft", { Fn = evalSinglet evalIsLeft; Pars = [|"eir"|]; DocOpt = Some "Determine if an Either is left-inhabited." })
                 ("isRight", { Fn = evalSinglet evalIsRight; Pars = [|"eir"|]; DocOpt = Some "Determine if an Either is right-inhabited." })
                 ("isEmpty", { Fn = evalSinglet (evalIsEmpty evalApply); Pars = [|"context"|]; DocOpt = Some "Determine if a context is uninhabited." })
                 ("notEmpty", { Fn = evalSinglet (evalNotEmpty evalApply); Pars = [|"context"|]; DocOpt = Some "Determine if a context is inhabited." })
                 ("tryUncons", { Fn = evalSinglet (evalTryUncons evalApply); Pars = [|"context"|]; DocOpt = Some "Attempt to separate the first index of a context from its remainder." })
                 ("uncons", { Fn = evalSinglet (evalUncons evalApply); Pars = [|"context"|]; DocOpt = Some "Separate the first index of a context from its remainder." })
                 ("cons", { Fn = evalDoublet evalCons; Pars = [|"a"; "context"|]; DocOpt = Some "Prepend an inhabitant to a context." })
                 ("commit", { Fn = evalSinglet evalCommit; Pars = [|"context"|]; DocOpt = Some "Finalize the construction of a context (such as reversing it when a construction process flips its inhabitants)." })
                 ("tryHead", { Fn = evalSinglet (evalTryHead evalApply); Pars = [|"context"|]; DocOpt = Some "Attempt to determine the first index of a context." })
                 ("head", { Fn = evalSinglet (evalHead evalApply); Pars = [|"context"|]; DocOpt = Some "Determine the first index of a context." })
                 ("tryTail", { Fn = evalSinglet (evalTryTail evalApply); Pars = [|"context"|]; DocOpt = Some "Attempt to determine the non-first indices of a context." })
                 ("tail", { Fn = evalSinglet (evalTail evalApply); Pars = [|"context"|]; DocOpt = Some "Determine the non-first indices of a context." })
                 ("scanWhile", { Fn = evalTriplet (evalScanWhile evalApply); Pars = [|"scanner"; "state"; "context"|]; DocOpt = Some "Scan over the inhabitants of a context until scanner returns none." })
                 ("scani", { Fn = evalTriplet (evalScani evalApply); Pars = [|"scanneri"; "state"; "context"|]; DocOpt = Some "Scani over the inhabitants of a context." })
                 ("scan", { Fn = evalTriplet (evalScan evalApply); Pars = [|"scanner"; "state"; "context"|]; DocOpt = Some "Scan over the inhabitants of a context." })
                 ("foldWhile", { Fn = evalTriplet (evalFoldWhile evalApply); Pars = [|"folder"; "state"; "context"|]; DocOpt = Some "Fold over the inhabitants of a context until scanner returns none." })
                 ("foldi", { Fn = evalTriplet (evalFoldi evalApply); Pars = [|"folderi"; "state"; "context"|]; DocOpt = Some "Foldi over the inhabitants of a context." })
                 ("fold", { Fn = evalTriplet (evalFold evalApply); Pars = [|"folder"; "state"; "context"|]; DocOpt = Some "Fold over the inhabitants of a context." })
                 ("mapi", { Fn = evalDoublet (evalMapi evalApply); Pars = [|"mapperi"; "context"|]; DocOpt = Some "Map over a context with the given mapperi function." })
                 ("map", { Fn = evalDoublet (evalMap evalApply); Pars = [|"mapper"; "context"|]; DocOpt = Some "Map over a context with the given mapper function." })
                 ("pure", { Fn = evalPure; Pars = [|"typeIndicator"; "a"|]; DocOpt = Some "Construct a pure (AKA, singleton) context." })
                 ("apply", { Fn = evalApplyScript evalApply; Pars = [|"fnContext"; "context"|]; DocOpt = Some "Sequential application." })
                 ("bind", { Fn = evalBind evalApply; Pars = [|"context"; "binder"|]; DocOpt = Some "Sequential composition." })
                 ("map2", { Fn = evalMap2 evalApply; Pars = [|"mapper"; "context"; "context2"|]; DocOpt = Some "Simultaneously map over two contexts." })
                 ("product", { Fn = evalProduct; Pars = [|"context"; "context2"|]; DocOpt = Some "Construct a product context from the inhabitants of two other contexts." })
                 ("sum", { Fn = evalSum; Pars = [|"context"; "context2"|]; DocOpt = Some "Construct a sum context from the inhabitants of two other contexts." })
                 ("contains", { Fn = evalDoublet (evalContains evalApply); Pars = [|"a"; "context"|]; DocOpt = Some "Determine if a context contains a given inhabitant." })
                 ("toString", { Fn = evalSinglet evalToString; Pars = [|"context"|]; DocOpt = Some "Convert a context to a String." })
                 ("codata", { Fn = evalDoublet evalCodata; Pars = [|"unfolder"; "seed"|]; DocOpt = Some "Construct Codata from an unfolder function and a seed." })
                 ("toCodata", { Fn = evalSinglet evalToCodata; Pars = [|"context"|]; DocOpt = Some "Convert a context to Codata." })
                 ("list", { Fn = evalList; Pars = [|"..."|]; DocOpt = Some "Construct a List." })
                 ("toList", { Fn = evalSinglet (evalToList evalApply); Pars = [|"context"|]; DocOpt = Some "Convert a context to a List." })
                 ("ring", { Fn = evalRing; Pars = [|"..."|]; DocOpt = Some "Construct a Ring." })
                 ("toRing", { Fn = evalSinglet (evalToRing evalApply); Pars = [|"context"|]; DocOpt = Some "Convert a context to Ring." })
                 ("add", { Fn = evalDoublet evalCons; Pars = [|"a"; "context"|]; DocOpt = Some "Introduce an inhabitant to a context." })
                 ("remove", { Fn = evalDoublet evalRemove; Pars = [|"a"; "context"|]; DocOpt = Some "Remove an inhabitant from a context." })
                 ("toTable", { Fn = evalSinglet evalToTable; Pars = [|"context"|]; DocOpt = Some "Convert a context to a Table." })
                 ("info", { Fn = evalSinglet evalInfo; Pars = [|"f"|]; DocOpt = Some "Get information on a function's metadata." })] |>
                dictPlus
            Intrinsics <- intrinsics
            intrinsics
        else Intrinsics :?> Dictionary<string, 'w ScriptingTrinsic>

    and internal evalIntrinsicInner<'w when 'w :> 'w ScriptingSystem> fnName argsEvaled originOpt (world : 'w) =
        let intrinsics = getIntrinsics ()
        match intrinsics.TryGetValue fnName with
        | (true, intrinsic) -> intrinsic.Fn fnName argsEvaled originOpt world
        | (false, _) -> struct (Violation (["InvalidFunctionTargetBinding"], "Cannot apply the non-existent binding '" + fnName + "'.", originOpt), world)

    and evalOverload fnName argsEvaled originOpt world =
        if Array.notEmpty argsEvaled then
            match Array.last argsEvaled with
            | Violation _ as error -> struct (error, world)
            | Pluggable pluggable ->
                let pluggableTypeName = pluggable.TypeName
                let xfnName = toOverloadName fnName pluggableTypeName
                let xfnBinding = Binding (xfnName, ref UncachedBinding, ref UnknownBindingType, None)
                let evaleds = Array.cons xfnBinding argsEvaled
                evalApply evaleds originOpt world
            | Union (name, _)
            | Record (name, _, _) ->
                let xfnName = toOverloadName fnName name
                let xfnBinding = Binding (xfnName, ref UncachedBinding, ref UnknownBindingType, None)
                let evaleds = Array.cons xfnBinding argsEvaled
                evalApply evaleds originOpt world
            | _ -> struct (Violation (["InvalidOverload"], "Could not find overload for '" + fnName + "' for target.", originOpt), world)
        else struct (Violation (["InvalidFunctionTargetBinding"], "Cannot apply the non-existent binding '" + fnName + "'.", originOpt), world)

    and evalUnionUnevaled name exprs world =
        let struct (evaleds, world) = evalMany exprs world
        struct (Union (name, evaleds), world)

    and evalTableUnevaled exprPairs world =
        let struct (evaledPairs, world) =
            List.fold (fun struct (evaledPairs, world) (exprKey, exprValue) ->
                let struct (evaledKey, world) = eval exprKey world
                let struct (evaledValue, world) = eval exprValue world
                struct ((evaledKey, evaledValue) :: evaledPairs, world))
                struct ([], world)
                exprPairs
        let evaledPairs = List.rev evaledPairs
        struct (Table (Map.ofList evaledPairs), world)

    and evalRecordUnevaled name exprPairs world =
        let struct (evaledPairs, world) =
            List.fold (fun struct (evaledPairs, world) (fieldName, expr) ->
                let struct (evaledValue, world) = eval expr world
                struct ((fieldName, evaledValue) :: evaledPairs, world))
                struct ([], world)
                exprPairs
        let evaledPairs = List.rev evaledPairs
        let map = evaledPairs |> List.mapi (fun i (fieldName, _) -> (fieldName, i)) |> Map.ofList
        let fields = evaledPairs |> List.map snd |> Array.ofList
        struct (Record (name, map, fields), world)

    and evalBinding<'w when 'w :> 'w ScriptingSystem> expr name cachedBinding bindingType originOpt (world : 'w) =
        match tryGetBinding name cachedBinding bindingType world with
        | None ->
            match !bindingType with
            | UnknownBindingType ->
                if (getIntrinsics<'w> ()).ContainsKey name then bindingType := IntrinsicBinding; struct (expr, world)
                elif FOption.isSome (world.TryGetExtrinsic name) then bindingType := ExtrinsicBinding; struct (expr, world)
                else struct (Violation (["NonexistentBinding"], "Non-existent binding '" + name + "'.", originOpt), world)
            | IntrinsicBinding -> struct (expr, world)
            | ExtrinsicBinding -> struct (expr, world)
            | EnvironmentalBinding -> struct (Violation (["NonexistentBinding"], "Non-existent binding '" + name + "'.", originOpt), world)
        | Some binding -> struct (binding, world)

    and evalInfo fnName argEvaled originOpt (world : 'w) =
        match argEvaled with
        | Violation _ as violation -> struct (violation, world)
        | Binding (name, _, _, _) ->
            match Dictionary.tryFind name (getIntrinsics<'w> ()) with
            | Some trinsic -> struct (String ("[fun [" + String.Join (" ", trinsic.Pars) + "] '" + (Option.getOrDefault "" trinsic.DocOpt) + "']"), world)
            | None ->
                match world.TryGetExtrinsic name |> FOption.toOpt with
                | Some trinsic -> struct (String ("[fun [" + String.Join (" ", trinsic.Pars) + "] '" + (Option.getOrDefault "" trinsic.DocOpt) + "']"), world)
                | None -> struct (Violation (["NonExistentBinding/Function"], "Could not find function binding '" + name + "' for use with '" + fnName + "'.", originOpt), world)
        | Fun (args, _, _, _, _, _, _) -> struct (String ("[fun [" + String.Join (" ", args) + "] ...]"), world)
        | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Cannot apply " + fnName + " to a non-function.", originOpt), world)

    and evalAlterIntInner fnName index target value originOpt world =
        match target with
        | String str ->
            if index >= 0 && index < String.length str then
                match value with
                | String str2 when str2.Length = 1 ->
                    let left = str.Substring (0, index)
                    let right = str.Substring (index, str.Length)
                    Right struct (String (left + str2 + right), world)
                | _ -> Left struct (Violation (["InvalidArgumentValue"; String.capitalize fnName], "String alter value must be a String of length 1.", originOpt), world)
            else Left struct (Violation (["ArgumentOutOfRange"; String.capitalize fnName], "String does not contain element at index " + string index + ".", originOpt), world)
        | Option opt ->
            match (index, opt) with
            | (0, Some value) -> Right struct (value, world)
            | (_, _) -> Left struct (Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Could not alter at index " + string index + ".", originOpt), world)
        | List _ -> Left struct (Violation (["NotImplemented"; String.capitalize fnName], "Updating lists by index is not yet implemented.", originOpt), world) // TODO: implement
        | Table map -> Right struct (Table (Map.add (Int index) value map), world)
        | Tuple elements
        | Union (_, elements)
        | Record (_, _, elements) ->
            if index < elements.Length then
                let elements' = Array.copy elements
                elements'.[index] <- value
                match target with
                | Tuple _ -> Right struct (Tuple elements', world)
                | Union (name, _) -> Right struct (Union (name, elements'), world)
                | Record (name, map, _) -> Right struct (Record (name, map, elements'), world)
                | _ -> failwithumf ()
            else Left struct (Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Could not alter structure at index " + string index + ".", originOpt), world)
        | _ ->
            match evalOverload fnName [|Int index; value; target|] originOpt world with
            | struct (Violation _, _) as error -> Left error
            | struct (_, _) as success -> Right success

    and evalAlterKeywordInner fnName keyword target value originOpt world =
        match target with
        | Violation _ as violation -> Left struct (violation, world)
        | Table map ->
            Right struct (Table (Map.add (Keyword keyword) value map), world)
        | Record (name, map, fields) ->
            match Map.tryFind keyword map with
            | Some index ->
                if index < fields.Length then
                    let fields' = Array.copy fields
                    fields'.[index] <- value
                    Right struct (Record (name, map, fields'), world)
                else Left struct (Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt), world)
            | None ->
                Left struct (Violation (["ArgumentOutOfRange"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt), world)
        | _ ->
            match evalOverload fnName [|Keyword keyword; value; target|] originOpt world with
            | struct (Violation _, _) as error -> Left error
            | struct (_, _) as success -> Right success

    and evalAlterInner fnName indexerExpr targetExpr valueExpr originOpt world =
        let struct (indexer, world) = eval indexerExpr world
        let struct (target, world) = eval targetExpr world
        let struct (value, world) = eval valueExpr world
        match indexer with
        | Violation _ as v -> Left struct (v, world)
        | Int index -> evalAlterIntInner fnName index target value originOpt world
        | Keyword keyword -> evalAlterKeywordInner fnName keyword target value originOpt world
        | _ ->
            match target with
            | Table map -> Right struct (Table (Map.add indexer valueExpr map), world)
            | _ ->
                match evalOverload fnName [|indexer; value; target|] originOpt world with
                | struct (Violation _, _) as error -> Left error
                | struct (_, _) as success -> Right success

    and evalTryAlter indexerExpr targetExpr valueExpr originOpt world =
        match evalAlterInner "tryAlter" indexerExpr targetExpr valueExpr originOpt world with
        | Right struct (evaled, world) -> struct (Option (Some evaled), world)
        | Left struct (_, world) -> struct (NoneValue, world)

    and evalAlter indexerExpr targetExpr valueExpr originOpt world =
        match evalAlterInner "alter" indexerExpr targetExpr valueExpr originOpt world with
        | Right success -> success
        | Left error -> error

    and evalApplyBody (pars : string array) parsCount (argsEvaled : Expr array) (body : Expr) (framesOpt : obj option) originOpt (world : 'w) : struct (Expr * 'w) =
        let struct (framesCurrentOpt, world) =
            match framesOpt with
            | Some frames ->
                let framesCurrent = getProceduralFrames world
                setProceduralFrames (frames :?> ProceduralFrame list) world
                struct (Some framesCurrent, world)
            | None -> struct (None, world)
        let struct (evaled, world) =
            if Array.length argsEvaled = parsCount then
                let bindings = Array.map2 (fun par argEvaled -> struct (par, argEvaled)) pars argsEvaled
                addProceduralBindings (AddToNewFrame parsCount) bindings world
                let struct (evaled, world) = eval body world
                removeProceduralBindings world
                struct (evaled, world)
            else struct (Violation (["MalformedApplication"], "Wrong number of arguments.", originOpt), world)
        match framesCurrentOpt with
        | Some framesCurrent ->
            setProceduralFrames framesCurrent world
            struct (evaled, world)
        | None -> struct (evaled, world)

    and evalApply<'w when 'w :> 'w ScriptingSystem> (exprs : Expr array) (originOpt : SymbolOrigin option) (world : 'w) : struct (Expr * 'w) =
        if Array.notEmpty exprs then
            let (exprsHead, exprsTail) = (Array.head exprs, Array.tail exprs)
            let struct (headEvaled, world) = eval exprsHead world in annotateWorld world // force the type checker to see the world as it is
            match headEvaled with
            | Violation _ as error -> struct (error, world)
            | Keyword keyword ->
                let struct (tailEvaled, world) = evalMany exprsTail world
                let union = Union (keyword, tailEvaled)
                struct (union, world)
            | Binding (fnName, _, bindingType, originOpt) ->
                // NOTE: when evaluation leads here, we infer that we have either an extrinsic or intrinsic function,
                // otherwise it would have led to the Fun case... Also, binding type should be decided by this point.
                match bindingType.Value with
                | UnknownBindingType ->
                    failwithumf ()
                | IntrinsicBinding ->
                    let struct (argsEvaled, world) = evalMany exprsTail world
                    match evalIntrinsicInner fnName argsEvaled originOpt world with
                    | struct (Violation _, world) -> evalOverload fnName argsEvaled originOpt world
                    | success -> success
                | ExtrinsicBinding -> 
                    let args = Array.tail exprs
                    let extrinsicOpt = world.TryGetExtrinsic fnName
                    if FOption.isSome extrinsicOpt
                    then extrinsicOpt.Value.Fn fnName args originOpt world
                    else failwithumf ()
                | EnvironmentalBinding ->
                    failwithumf ()
            | Fun (pars, parsCount, body, _, framesOpt, _, originOpt) ->
                let struct (tailEvaled, world) = evalMany exprsTail world
                evalApplyBody pars parsCount tailEvaled body framesOpt originOpt world
            | _ -> struct (Violation (["MalformedApplication"], "Cannot apply the non-binding '" + scstring headEvaled + "'.", originOpt), world)
        else struct (Unit, world)

    and evalApplyAnd exprs originOpt world =
        match exprs with
        | [|left; right|] ->
            match eval left world with
            | struct (Violation _, _) as error -> error
            | struct (Bool false, _) as never -> never
            | struct (Bool true, world) ->
                match eval right world with
                | struct (Violation _, _) as error -> error
                | struct (Bool _, _) as result -> result
                | _ -> struct (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
        | _ -> struct (Violation (["InvalidArgumentCount"; "&&"], "Incorrect number of arguments for '&&'; 2 arguments required.", originOpt), world)

    and evalApplyOr exprs originOpt world =
        match exprs with
        | [|left; right|] ->
            match eval left world with
            | struct (Violation _, _) as error -> error
            | struct (Bool true, _) as always -> always
            | struct (Bool false, world) ->
                match eval right world with
                | struct (Violation _, _) as error -> error
                | struct (Bool _, _) as result -> result
                | _ -> struct (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
        | _ -> struct (Violation (["InvalidArgumentCount"; "&&"], "Incorrect number of arguments for '&&'; 2 arguments required.", originOpt), world)

    and evalLet4 binding body originOpt world =
        let world =
            match binding with
            | VariableBinding (name, body) ->
                let struct (evaled, world) = eval body world
                addProceduralBinding (AddToNewFrame 1) name evaled world
                world
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, None, originOpt)
                addProceduralBinding (AddToNewFrame 1) name fn world
                world
        let struct (evaled, world) = eval body world
        removeProceduralBindings world
        struct (evaled, world)

    and evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world =
        let world =
            match bindingsHead with
            | VariableBinding (name, body) ->
                let struct (bodyValue, world) = eval body world
                addProceduralBinding (AddToNewFrame bindingsCount) name bodyValue world
                world
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, None, originOpt)
                addProceduralBinding (AddToNewFrame bindingsCount) name fn world
                world
        let world =
            List.foldi (fun i world binding ->
                match binding with
                | VariableBinding (name, body) ->
                    let struct (bodyValue, world) = eval body world
                    addProceduralBinding (AddToHeadFrame (inc i)) name bodyValue world
                    world
                | FunctionBinding (name, args, body) ->
                    let frames = getProceduralFrames world :> obj
                    let fn = Fun (args, args.Length, body, true, Some frames, None, originOpt)
                    addProceduralBinding (AddToHeadFrame (inc i)) name fn world
                    world)
                world
                bindingsTail
        let struct (evaled, world) = eval body world
        removeProceduralBindings world
        struct (evaled, world)
        
    and evalLet binding body originOpt world =
        evalLet4 binding body originOpt world
        
    and evalLetMany bindings body originOpt world =
        match bindings with
        | bindingsHead :: bindingsTail ->
            let bindingsCount = List.length bindingsTail + 1
            evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world
        | [] -> struct (Violation (["MalformedLetOperation"], "Let operation must have at least 1 binding.", originOpt), world)

    and evalIntrinsic name pars parsCount body _ (world : 'w) =
        let intrinsics = getIntrinsics<'w> ()
        let evalIntrinsic4 _ argsEvaled originOpt world = evalApplyBody pars parsCount argsEvaled body None originOpt world
        let intrinsic = { Fn = evalIntrinsic4; Pars = pars; DocOpt = None }
        intrinsics.Add (name, intrinsic)
        struct (Unit, world)

    and evalFun fn pars parsCount body framesPushed framesOpt originOpt world =
        if not framesPushed then
            if Option.isNone framesOpt then
                let frames = getProceduralFrames world :> obj
                struct (Fun (pars, parsCount, body, true, Some frames, None, originOpt), world)
            else struct (Fun (pars, parsCount, body, true, framesOpt, None, originOpt), world)
        else struct (fn, world)

    and evalIf condition consequent alternative originOpt world =
        match eval condition world with
        | struct (Violation _ as evaled, world) -> struct (evaled, world)
        | struct (Bool bool, world) -> if bool then eval consequent world else eval alternative world
        | struct (_, world) -> struct (Violation (["InvalidIfCondition"], "Must provide an expression that evaluates to a Bool in an if condition.", originOpt), world)

    and evalMatch input (cases : (Expr * Expr) array) originOpt world =
        let struct (input, world) = eval input world
        let resultEir =
            Seq.foldUntilRight (fun world (condition, consequent) ->
                let struct (evaledInput, world) = eval condition world
                match evalBinaryInner EqFns "=" input evaledInput originOpt world with
                | struct (Violation _, world) -> Right struct (evaledInput, world)
                | struct (Bool true, world) -> Right (eval consequent world)
                | struct (Bool false, world) -> Left world
                | _ -> failwithumf ())
                (Left world)
                cases
        match resultEir with
        | Right success -> success
        | Left world -> struct (Violation (["InexhaustiveMatch"], "A match expression failed to satisfy any of its cases.", originOpt), world)

    and evalSelect exprPairs originOpt world =
        let resultEir =
            Seq.foldUntilRight (fun world (condition, consequent) ->
                match eval condition world with
                | struct (Violation _ as evaled, world) -> Right struct (evaled, world)
                | struct (Bool bool, world) -> if bool then Right (eval consequent world) else Left world
                | struct (_, world) -> Right struct (Violation (["InvalidSelectCondition"], "Must provide an expression that evaluates to a Bool in a case condition.", originOpt), world))
                (Left world)
                exprPairs
        match resultEir with
        | Right success -> success
        | Left world -> struct (Violation (["InexhaustiveSelect"], "A select expression failed to satisfy any of its cases.", originOpt), world)

    and evalTry body handlers _ world =
        match eval body world with
        | struct (Violation (categories, _, _) as evaled, world) ->
            match
                List.foldUntilRight (fun world (handlerCategories, handlerBody) ->
                    let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                    if categoriesTrunc = handlerCategories then Right (eval handlerBody world) else Left world)
                    (Left world)
                    handlers with
            | Right success -> success
            | Left world -> struct (evaled, world)
        | success -> success

    and evalDo exprs _ world =
        let evaledEir =
            List.foldWhileRight (fun struct (_, world) expr ->
                match eval expr world with
                | struct (Violation _, _) as error -> Left error
                | success -> Right success)
                (Right struct (Unit, world))
                exprs
        Either.amb evaledEir

    and evalDefine binding originOpt world =
        let struct (bound, world) =
            match binding with
            | VariableBinding (name, body) ->
                let struct (evaled, world) = eval body world
                struct (tryAddDeclarationBinding name evaled world, world)
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, None, originOpt)
                struct (tryAddDeclarationBinding name fn world, world)
        if bound
        then struct (Unit, world)
        else struct (Violation (["InvalidDeclaration"], "Can make declarations only at the top-level.", None), world)

    /// Evaluate an expression.
    and eval expr world =
        match expr with
        | Violation _
        | Unit _
        | Bool _
        | Int _
        | Int64 _
        | Single _
        | Double _
        | String _
        | Keyword _
        | Tuple _
        | Union _
        | Pluggable _
        | Option _
        | Either _
        | Codata _
        | List _
        | Ring _
        | Table _
        | Record _ -> struct (expr, world)
        | UnionUnevaled (name, exprs) -> evalUnionUnevaled name exprs world
        | TableUnevaled exprPairs -> evalTableUnevaled exprPairs world
        | RecordUnevaled (name, exprPairs) -> evalRecordUnevaled name exprPairs world
        | Binding (name, cachedBinding, bindingType, originOpt) as expr -> evalBinding expr name cachedBinding bindingType originOpt world
        | TryAlter (expr, expr2, expr3, _, originOpt) -> evalTryAlter expr expr2 expr3 originOpt world
        | Alter (expr, expr2, expr3, _, originOpt) -> evalAlter expr expr2 expr3 originOpt world
        | Apply (exprs, _, originOpt) -> evalApply exprs originOpt world
        | ApplyAnd (exprs, _, originOpt) -> evalApplyAnd exprs originOpt world
        | ApplyOr (exprs, _, originOpt) -> evalApplyOr exprs originOpt world
        | Let (binding, body, originOpt) -> evalLet binding body originOpt world
        | LetMany (bindings, body, originOpt) -> evalLetMany bindings body originOpt world
        | Intrinsic (name, pars, parsCount, body, originOpt) -> evalIntrinsic name pars parsCount body originOpt world
        | Fun (pars, parsCount, body, framesPushed, framesOpt, _, originOpt) as fn -> evalFun fn pars parsCount body framesPushed framesOpt originOpt world
        | If (condition, consequent, alternative, originOpt) -> evalIf condition consequent alternative originOpt world
        | Match (input, cases, originOpt) -> evalMatch input cases originOpt world
        | Select (exprPairs, originOpt) -> evalSelect exprPairs originOpt world
        | Try (body, handlers, originOpt) -> evalTry body handlers originOpt world
        | Do (exprs, originOpt) -> evalDo exprs originOpt world
        | Quote _ as quote -> struct (quote, world)
        | Define (binding, originOpt) -> evalDefine binding originOpt world

    /// Evaluate a sequence of expressions.
    and evalMany (exprs : Expr array) world =
        let evaleds = Array.zeroCreate exprs.Length
        let world =
            Seq.foldi
                (fun i world expr ->
                    let struct (evaled, world) = eval expr world
                    evaleds.[i] <- evaled
                    world)
                world
                exprs
        struct (evaleds, world)

    /// Evaluate an expression, with logging on violation result.
    let evalWithLogging expr world =
        let struct (evaled, world) = eval expr world
        log evaled
        struct (evaled, world)

    /// Evaluate a series of expressions, with logging on violation result.
    let evalManyWithLogging exprs world =
        let struct (evaleds, world) = evalMany exprs world
        Array.iter log evaleds
        struct (evaleds, world)

    /// Attempt to evaluate a script.
    let tryEvalScript choose scriptFilePath world =
        Log.info ("Evaluating script '" + scriptFilePath + "'...")
        try let scriptStr =
                scriptFilePath |>
                File.ReadAllText |>
                String.unescape
            let script =
                scriptStr |>
                (fun str -> Symbol.OpenSymbolsStr + str + Symbol.CloseSymbolsStr) |>
                scvalue<Expr array>
            let struct (evaleds, world) = evalMany script world
            Log.info ("Successfully evaluated script '" + scriptFilePath + "'.")
            Right struct (scriptStr, evaleds, world)
        with exn ->
            let error = "Failed to evaluate script '" + scriptFilePath + "' due to: " + scstring exn
            Log.info error
            Left struct (error, choose world)
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections.Generic
open System.ComponentModel
open FSharp.Reflection
open Prime
module Scripting =

    type Pluggable =
        inherit IComparable
        abstract member TypeName : string
        abstract member FSharpType : Type
        abstract member ToSymbol : unit -> Symbol

    and [<NoComparison>] CachedBinding =
        | UncachedBinding
        | DeclarationBinding of Expr
        | ProceduralBinding of int * int

    and [<NoComparison>] BindingType =
        | UnknownBindingType
        | IntrinsicBinding
        | ExtrinsicBinding
        | EnvironmentalBinding

    and [<NoComparison>] Binding =
        | VariableBinding of VarName : string * VarValue : Expr
        | FunctionBinding of FunName : string * FunArgs : string array * FunLambda : Expr

    and [<NoComparison>] Breakpoint =
        { mutable BreakEnabled : bool
          mutable BreakCondition : Expr }

    and [<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>] Codata =
        | Empty
        | Add of Codata * Codata
        | Unfold of Expr * Expr
        | Conversion of Expr list

    and [<Syntax
            ((* Built-in Identifiers *)
             "true false nil " +
             "not hash empty identity minimum maximum " +
             "inc dec negate " +
             "pow root sqr sqrt " +
             "floor ceiling truncate round exp log " +
             "sin cos tan asin acos atan " +
             "length normal cross dot " +
             "violation bool int int64 single double string " +
             "getTypeName " +
             "tryIndex hasIndex index tryAlter alter getName " +
             "record " +
             "tuple pair unit fst snd thd fth fif " +
             "some none isSome isNone isEmpty notEmpty " +
             "isLeft isRight left right " +
             "tryUncons uncons cons commit tryHead head tryTail tail " +
             "scanWhile scani scan foldWhile foldi fold mapi map contains " +
             "pure apply bind " +
             "map2 product sum " +
             "toString " +
             "codata toCodata coempty " +
             "list toList " +
             "ring toRing add remove " +
             "table toTable " +
             "info " +
             "let intrinsic fun if match select try do break get set update define " +

             (* Prelude Identifiers *)
             "-u- -b- -i- -L- -f- -d- -s- -K- -T- -U- -o- -e- -l- -r- -t- -R- -F- " +
             "isUnit isBool isInt isInt64 isSingle isDouble isString " +
             "isKeyword isTuple isUnion isOption isEither isList isRing isTable isRecord isFunction " +
             "id flip isIdentity isPositive isNegative isPositiveInfinity isNegativeInfinity isNaN " +
             "min max compare sign abs fst! snd! rev foldBackWhile foldBacki foldBack " +
             "reduceWhile reducei reduce definitize filter takeWhile take skipWhile skip " +
             // TODO: "substring curry compose sort replace slice split " +
             "countBy count tally notContains exists notExists zipBy zip pi e",

             (* Unions *)
             "Gt Lt Eq Positive Negative Zero",

             (* Title Words *)
             "record if match",

             (* Header Words *)
             "define fun",

             (* Detail Words *)
             "get set update",
             Constants.PrettyPrinter.DefaultThresholdMin,
             Constants.PrettyPrinter.DefaultThresholdMax);
          TypeConverter (typeof<ExprConverter>);
          CustomEquality;
          CustomComparison>]
        Expr =

        (* Primitive Value Types *)
        | Violation of string list * string * SymbolOrigin option
        | Unit
        | Bool of bool
        | Int of int
        | Int64 of int64
        | Single of single
        | Double of double
        | String of string
        | Keyword of string
        | Pluggable of Pluggable

        (* Primitive Data Structures *)
        | Tuple of Expr array
        | Union of string * Expr array
        | Option of Expr option
        | Either of Either<Expr, Expr>
        | Codata of Codata
        | List of Expr list
        | Ring of Set<Expr>
        | Table of Map<Expr, Expr>
        | Record of string * Map<string, int> * Expr array

        (* Intermediate Data Structures *)
        | UnionUnevaled of string * Expr array
        | TableUnevaled of (Expr * Expr) list
        | RecordUnevaled of string * (string * Expr) list

        (* Special Forms *)
        | Binding of string * CachedBinding ref * BindingType ref * SymbolOrigin option
        | TryAlter of Expr * Expr * Expr * Breakpoint * SymbolOrigin option
        | Alter of Expr * Expr * Expr * Breakpoint * SymbolOrigin option
        | Apply of Expr array * Breakpoint * SymbolOrigin option
        | ApplyAnd of Expr array * Breakpoint * SymbolOrigin option
        | ApplyOr of Expr array * Breakpoint * SymbolOrigin option
        | Let of Binding * Expr * SymbolOrigin option
        | LetMany of Binding list * Expr * SymbolOrigin option
        | Intrinsic of string * string array * int * Expr option * SymbolOrigin option
        | Fun of string array * int * Expr * bool * obj option * string option * SymbolOrigin option
        | If of Expr * Expr * Expr * SymbolOrigin option
        | Match of Expr * (Expr * Expr) array * SymbolOrigin option
        | Select of (Expr * Expr) array * SymbolOrigin option
        | Try of Expr * (string list * Expr) list * SymbolOrigin option
        | Do of Expr list * SymbolOrigin option
        | Quote of Expr * SymbolOrigin option

        (* Declarations - only work at the top level. *)
        | Define of Binding * SymbolOrigin option

        static member tryGetOrigin expr =
            match expr with
            | Violation (_, _, originOpt) -> originOpt
            | Unit
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
            | Record _
            | UnionUnevaled _
            | TableUnevaled _
            | RecordUnevaled _ -> None
            | Binding (_, _, _, originOpt)
            | TryAlter (_, _, _, _, originOpt)
            | Alter (_, _, _, _, originOpt)
            | Apply (_, _, originOpt)
            | ApplyAnd (_, _, originOpt)
            | ApplyOr (_, _, originOpt)
            | Let (_, _, originOpt)
            | LetMany (_, _, originOpt)
            | Intrinsic (_, _, _, _, originOpt)
            | Fun (_, _, _, _, _, _, originOpt)
            | If (_, _, _, originOpt)
            | Match (_, _, originOpt)
            | Select (_, originOpt)
            | Try (_, _, originOpt)
            | Do (_, originOpt)
            | Quote (_, originOpt)
            | Define (_, originOpt) -> originOpt

        static member equals left right =
            match (left, right) with
            | (Violation (leftNames, leftError, _), Violation (rightNames, rightError, _)) -> (leftNames, leftError) = (rightNames, rightError)
            | (Unit, Unit) -> true
            | (Bool left, Bool right) -> left = right
            | (Int left, Int right) -> left = right
            | (Int64 left, Int64 right) -> left = right
            | (Single left, Single right) -> left = right
            | (Double left, Double right) -> left = right
            | (String left, String right) -> left = right
            | (Keyword left, Keyword right) -> left = right
            | (Pluggable left, Pluggable right) -> left = right
            | (Tuple left, Tuple right) -> left = right
            | (Union (leftName, leftExprs), Union (rightName, rightExprs)) -> (leftName, leftExprs) = (rightName, rightExprs)
            | (Option left, Option right) -> left = right
            | (Either left, Either right) -> left = right
            | (Codata left, Codata right) -> left = right
            | (List left, List right) -> left = right
            | (Ring left, Ring right) -> left = right
            | (Table left, Table right) -> left = right
            | (Record (leftName, leftMap, leftExprs), Record (rightName, rightMap, rightExprs)) -> (leftName, leftMap, leftExprs) = (rightName, rightMap, rightExprs)
            | (UnionUnevaled (leftName, leftExprs), UnionUnevaled (rightName, rightExprs)) -> (leftName, leftExprs) = (rightName, rightExprs)
            | (TableUnevaled left, TableUnevaled right) -> left = right
            | (RecordUnevaled (leftName, leftExprs), RecordUnevaled (rightName, rightExprs)) -> (leftName, leftExprs) = (rightName, rightExprs)
            | (Binding (left, _, _, _), Binding (right, _, _, _)) -> left = right
            | (TryAlter (leftExpr, leftExpr2, leftExpr3, _, _), TryAlter (rightExpr, rightExpr2, rightExpr3, _, _)) -> (leftExpr, leftExpr2, leftExpr3) = (rightExpr, rightExpr2, rightExpr3)
            | (Alter (leftExpr, leftExpr2, leftExpr3, _, _), TryAlter (rightExpr, rightExpr2, rightExpr3, _, _)) -> (leftExpr, leftExpr2, leftExpr3) = (rightExpr, rightExpr2, rightExpr3)
            | (Apply (left, _, _), Apply (right, _, _)) -> left = right
            | (ApplyAnd (left, _, _), ApplyAnd (right, _, _)) -> left = right
            | (ApplyOr (left, _, _), ApplyOr (right, _, _)) -> left = right
            | (Let (leftBinding, leftBody, _), Let (rightBinding, rightBody, _)) -> (leftBinding, leftBody) = (rightBinding, rightBody)
            | (LetMany (leftBindings, leftBody, _), LetMany (rightBindings, rightBody, _)) -> (leftBindings, leftBody) = (rightBindings, rightBody)
            | (Intrinsic (leftName, leftPars, _, leftBody, _), Intrinsic (rightName, rightPars, _, rightBody, _)) -> (leftName, leftPars, leftBody) = (rightName, rightPars, rightBody)
            | (Fun (leftPars, _, leftBody, _, _, _, _), Fun (rightPars, _, rightBody, _, _, _, _)) -> (leftPars, leftBody) = (rightPars, rightBody)
            | (If (leftConditional, leftConsequent, leftAlternative, _), If (rightConditional, rightConsequent, rightAlternative, _)) -> (leftConditional, leftConsequent, leftAlternative) = (rightConditional, rightConsequent, rightAlternative)
            | (Match (leftInput, leftCases, _), Match (rightInput, rightCases, _)) -> (leftInput, leftCases) = (rightInput, rightCases)
            | (Select (left, _), Select (right, _)) -> left = right
            | (Try (leftInput, leftCases, _), Try (rightInput, rightCases, _)) -> (leftInput, leftCases) = (rightInput, rightCases)
            | (Do (left, _), Do (right, _)) -> left = right
            | (Quote (left, _), Quote (right, _)) -> left = right
            | (Define (left, _), Define (right, _)) -> left = right
            | (_, _) -> false

        static member compare left right =
            match (left, right) with
            | (Violation (leftNames, leftError, _), Violation (rightNames, rightError, _)) -> compare (leftNames, leftError) (rightNames, rightError)
            | (Unit, Unit) -> 0
            | (Bool left, Bool right) -> compare left right
            | (Int left, Int right) -> compare left right
            | (Int64 left, Int64 right) -> compare left right
            | (Single left, Single right) -> compare left right
            | (Double left, Double right) -> compare left right
            | (String left, String right) -> compare left right
            | (Keyword left, Keyword right) -> compare left right
            | (Pluggable left, Pluggable right) -> compare left right
            | (Tuple left, Tuple right) -> compare left right
            | (Union (leftName, leftExprs), Union (rightName, rightExprs)) -> compare (leftName, leftExprs) (rightName, rightExprs)
            | (Option left, Option right) -> compare left right
            | (Either left, Either right) -> compare left right
            | (Codata left, Codata right) -> compare left right
            | (List left, List right) -> compare left right
            | (Ring left, Ring right) -> compare left right
            | (Table left, Table right) -> compare left right
            | (Record (leftName, leftMap, leftExprs), Record (rightName, rightMap, rightExprs)) -> compare (leftName, leftMap, leftExprs) (rightName, rightMap, rightExprs)
            | (UnionUnevaled (leftName, leftExprs), UnionUnevaled (rightName, rightExprs)) -> compare (leftName, leftExprs) (rightName, rightExprs)
            | (TableUnevaled left, TableUnevaled right) -> compare left right
            | (RecordUnevaled (leftName, leftExprs), RecordUnevaled (rightName, rightExprs)) -> compare (leftName, leftExprs) (rightName, rightExprs)
            | (_, _) -> -1

        /// Attempt to get the F# type that is used to represent the expression.
        static member toFSharpTypeOpt expr =
            match expr with
            | Violation _ -> None
            | Unit -> Some typeof<unit>
            | Bool _ -> Some typeof<bool>
            | Int _ -> Some typeof<int>
            | Int64 _ -> Some typeof<int64>
            | Single _ -> Some typeof<single>
            | Double _ -> Some typeof<double>
            | String _ -> Some typeof<string>
            | Keyword keyword
            | Union (keyword, _) ->
                let typeOpt =
                    // TODO: try caching the union types to speed this up
                    AppDomain.CurrentDomain.GetAssemblies () |>
                    Array.map (fun asm -> Array.filter FSharpType.IsUnion (asm.GetTypes ())) |>
                    Array.concat |>
                    Array.filter (fun ty -> ty.Name = keyword) |>
                    Array.filter (fun ty -> ty.GenericTypeArguments |> Array.notExists (fun arg -> arg.IsGenericParameter)) |> // constrained generics not currently supported...
                    Array.map (fun ty ->
                        if ty.IsGenericType
                        then ty.MakeGenericType (Array.create ty.GenericTypeArguments.Length typeof<obj>)
                        else ty) |>
                    Array.tryHead // just take the first found type for now...
                typeOpt
            | Record (keyword, _, _) ->
                let typeOpt =
                    // TODO: try caching the record types to speed this up
                    AppDomain.CurrentDomain.GetAssemblies () |>
                    Array.map (fun asm -> Array.filter FSharpType.IsRecord (asm.GetTypes ())) |>
                    Array.concat |>
                    Array.filter (fun ty -> ty.Name = keyword) |>
                    Array.filter (fun ty -> ty.GenericTypeArguments |> Array.notExists (fun arg -> arg.IsGenericParameter)) |> // constrained generics not currently supported...
                    Array.map (fun ty ->
                        if ty.IsGenericType
                        then ty.MakeGenericType (Array.create ty.GenericTypeArguments.Length typeof<obj>)
                        else ty) |>
                    Array.tryHead // just take the first found type for now...
                typeOpt
            | Pluggable value ->
                Some value.FSharpType
            | Tuple value ->
                let typeOpts = Array.map Expr.toFSharpTypeOpt value
                match Array.definitizePlus typeOpts with
                | (true, types) -> Some (FSharpType.MakeTupleType types)
                | (false, _) -> None
            | Option opt ->
                match opt with
                | Some value ->
                    match Expr.toFSharpTypeOpt value with
                    | Some ty -> Some (typedefof<_ option>.MakeGenericType [|ty|])
                    | None -> None
                | None -> None
            | Either eir ->
                // NOTE: no instance of Either can ever hold full type information, so we force use of obj here. This
                // may not be sensible in at least some cases, however.
                match eir with
                | Right value ->
                    match Expr.toFSharpTypeOpt value with
                    | Some ty -> Some (typedefof<Either<_, _>>.MakeGenericType [|typeof<obj>; ty|])
                    | None -> None
                | Left value ->
                    match Expr.toFSharpTypeOpt value with
                    | Some ty -> Some (typedefof<Either<_, _>>.MakeGenericType [|ty; typeof<obj>|])
                    | None -> None
            | List values ->
                let typeOpts = List.map Expr.toFSharpTypeOpt values
                match List.definitizePlus typeOpts with
                | (true, types) ->
                    match types with
                    | head :: tail ->
                        if List.notExists (fun item -> item <> head) tail
                        then Some (typedefof<_ list>.MakeGenericType [|head|])
                        else None
                    | [] -> None
                | (false, _) -> None
            | Ring values ->
                let typeOpts = List.map Expr.toFSharpTypeOpt (List.ofSeq values)
                match List.definitizePlus typeOpts with
                | (true, types) ->
                    match types with
                    | head :: tail ->
                        if List.notExists (fun item -> item <> head) tail
                        then Some (typedefof<_ Set>.MakeGenericType [|head|])
                        else None
                    | [] -> None
                | (false, _) -> None
            | Table values ->
                let keyOpts = List.map Expr.toFSharpTypeOpt (Map.toKeyList values)
                match List.definitizePlus keyOpts with
                | (true, keyTypes) ->
                    match keyTypes with
                    | keyHead :: keyTail ->
                        if List.notExists (fun item -> item <> keyHead) keyTail then
                            let valOpts = List.map Expr.toFSharpTypeOpt (Map.toValueList values)
                            match List.definitizePlus valOpts with
                            | (true, valTypes) ->
                                match valTypes with
                                | valHead :: valTail ->
                                    if List.notExists (fun item -> item <> valHead) valTail
                                    then Some (typedefof<Map<_, _>>.MakeGenericType [|keyHead; valHead|])
                                    else None
                                | [] -> None
                            | (false, _) -> None
                        else None
                    | [] -> None
                | (false, _) -> None
            | _ -> None

        static member internal isKeyword (str : string) =
            str.Length > 0 && str.[0] = Constants.Relation.Slot || Char.IsUpper str.[0]

        static member internal isBinding (str : string) =
            str.Length > 0 && not (Expr.isKeyword str)

        override this.GetHashCode () =
            match this with
            | Violation (names, error, _) -> hash names ^^^ hash error
            | Unit -> 0
            | Bool value -> hash value
            | Int value -> hash value
            | Int64 value -> hash value
            | Single value -> hash value
            | Double value -> hash value
            | String value -> hash value
            | Keyword value -> hash value
            | Pluggable value -> hash value
            | Tuple value -> hash value
            | Union (name, fields) -> hash (name, fields)
            | Option value -> hash value
            | Either value -> hash value
            | Codata value -> hash value
            | List value -> hash value
            | Ring value -> hash value
            | Table value -> hash value
            | Record (name, map, fields) -> hash (name, map, fields)
            | UnionUnevaled (name, fields) -> hash (name, fields)
            | TableUnevaled value -> hash value
            | RecordUnevaled (name, fields) -> hash (name, fields)
            | _ -> -1

        override this.Equals that =
            match that with
            | :? Expr as that -> Expr.equals this that
            | _ -> failwithumf ()

        interface Expr IComparable with
            member this.CompareTo that =
                Expr.compare this that

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Expr as that -> (this :> Expr IComparable).CompareTo that
                | _ -> failwithumf ()

    /// Converts Expr types.
    and ExprConverter () =
        inherit TypeConverter ()

        member this.SymbolToExpr (symbol : Symbol) =
            this.ConvertFrom symbol :?> Expr

        member this.SymbolsToExpr (symbols : Symbol list) =
            List.map this.SymbolToExpr symbols

        member this.BindingToSymbols (binding : Binding) =
            match binding with
            | VariableBinding (name, value) ->
                let nameSymbol = Atom (name, None)
                let valueSymbol = this.ExprToSymbol value
                [nameSymbol; valueSymbol]
            | FunctionBinding (name, pars, body) ->
                let nameSymbol = Atom (name, None)
                let parSymbols = Array.map (fun par -> Atom (par, None)) pars
                let parsSymbol = Symbols (List.ofArray parSymbols, None)
                let bodySymbol = this.ExprToSymbol body
                [nameSymbol; parsSymbol; bodySymbol]

        member this.BindingToSymbol binding =
            Symbols (this.BindingToSymbols binding, None)

        member this.CodataToSymbol codata =
            match codata with
            | Empty -> Atom ("coempty", None)
            | Add (left, right) -> Symbols ([Atom ("+", None); this.CodataToSymbol left; this.CodataToSymbol right], None)
            | Unfold (unfolder, state) -> Symbols ([Atom ("codata", None); this.ExprToSymbol unfolder; this.ExprToSymbol state], None)
            | Conversion source -> Symbols ([Atom ("toCodata", None); this.ExprsToSymbol source], None)

        member this.ExprToSymbol (expr : Expr) =
            this.ConvertTo (expr, typeof<Symbol>) :?> Symbol

        member this.ExprsToSymbol exprs =
            Symbols (List.map this.ExprToSymbol exprs, None)

        member this.ExprsToIndex expr expr2 =
            let indexSymbol = Atom ("Index", None)
            Symbols ([indexSymbol; this.ExprToSymbol expr; this.ExprToSymbol expr2], None)

        member this.IndexToExprs indices =
            match indices with
            | Symbols ([Atom ("Index", _); target; indexer], _) -> Some (target, indexer)
            | _ -> None
            
        member this.SymbolsToBindingOpt bindingSymbols =
            match bindingSymbols with
            | [Atom (bindingName, _); bindingBody] when Expr.isBinding bindingName ->
                let binding = VariableBinding (bindingName, this.SymbolToExpr bindingBody)
                Some binding
            | [Atom (bindingName, _); Symbols (bindingArgs, _); bindingBody] when Expr.isBinding bindingName ->
                let (bindingArgs, bindingErrors) = List.split (function Atom _ -> true | _ -> false) bindingArgs
                if List.isEmpty bindingErrors then
                    let bindingArgs = List.map (function Atom (arg, _) -> arg | _ -> failwithumf ()) bindingArgs
                    let binding = FunctionBinding (bindingName, Array.ofList bindingArgs, this.SymbolToExpr bindingBody)
                    Some binding
                else None
            | _ -> None

        override this.CanConvertTo (_, destType) =
            destType = typeof<Symbol> ||
            destType = typeof<Expr>

        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<Symbol> then
                let expr = source :?> Expr
                match expr with
                | Violation (names, error, originOpt) ->
                    let violationSymbol = Atom ("violation", None)
                    let namesSymbol = Atom (String.concat Constants.Scripting.ViolationSeparatorStr names, None)
                    let errorSymbol = Atom (error, None)
                    Symbols ([violationSymbol; namesSymbol; errorSymbol], originOpt) :> obj
                | Unit -> Symbols ([], None) :> obj
                | Bool bool -> Atom (String.boolToCodeString bool, None) :> obj
                | Int int -> Number (string int, None) :> obj
                | Int64 int64 -> Number (String.int64ToCodeString int64, None) :> obj
                | Single single -> Number (String.singleToCodeString single, None) :> obj
                | Double double -> Number (String.doubleToCodeString double, None) :> obj
                | String string -> Symbol.Text (string, None) :> obj
                | Keyword string -> Atom ((if String.isEmpty string then "nil" else string), None) :> obj
                | Pluggable pluggable -> pluggable.ToSymbol () :> obj
                | Tuple elems ->
                    let headingSymbol = Atom ((if Array.length elems = 2 then "pair" else "tuple"), None)
                    let elemSymbols = elems |> Array.map (fun elem -> this.ExprToSymbol elem) |> List.ofArray
                    Symbols (headingSymbol :: elemSymbols, None) :> obj
                | Union (name, fields) ->
                    let nameSymbol = Atom (name, None)
                    let elemSymbols = fields |> Array.map this.ExprToSymbol |> List.ofArray
                    match elemSymbols with
                    | [] -> nameSymbol :> obj
                    | _ :: _ -> Symbols (nameSymbol :: elemSymbols, None) :> obj
                | Option option ->
                    match option with
                    | Some value -> Symbols ([Atom ("some", None); this.ExprToSymbol value], None) :> obj
                    | None -> Atom ("none", None) :> obj
                | Either either ->
                    match either with
                    | Right value -> Symbols ([Atom ("right", None); this.ExprToSymbol value], None) :> obj
                    | Left value -> Symbols ([Atom ("left", None); this.ExprToSymbol value], None) :> obj
                | Codata codata ->
                    this.CodataToSymbol codata :> obj
                | List elems ->
                    let listSymbol = Atom ("list", None)
                    let elemSymbols = List.map this.ExprToSymbol elems
                    Symbols (listSymbol :: elemSymbols, None) :> obj
                | Ring set ->
                    let ringSymbol = Atom ("ring", None)
                    let elemSymbols = List.map this.ExprToSymbol (Set.toList set)
                    Symbols (ringSymbol :: elemSymbols, None) :> obj
                | Table map ->
                    let tableSymbol = Atom ("table", None)
                    let elemSymbols =
                        List.map (fun (key, value) ->
                            let keySymbol = this.ExprToSymbol key
                            let valueSymbol = this.ExprToSymbol value
                            Symbols ([keySymbol; valueSymbol], None))
                            (Map.toList map)
                    Symbols (tableSymbol :: elemSymbols, None) :> obj
                | Record (name, map, fields) ->
                    let recordSymbol = Atom ("record", None)
                    let nameSymbol = Atom (name, None)
                    let mapSwap = Map.ofSeqBy (fun (kvp : KeyValuePair<_, _>) -> (kvp.Value, kvp.Key)) map
                    let fieldSymbols =
                        Seq.map (fun (kvp : KeyValuePair<_, _>) ->
                            let key = kvp.Value
                            let value = fields.[kvp.Key]
                            let keySymbol = Atom (key, None)
                            let valueSymbol = this.ExprToSymbol value
                            Symbols ([keySymbol; valueSymbol], None))
                            mapSwap
                    Symbols (recordSymbol :: nameSymbol :: List.ofSeq fieldSymbols, None) :> obj
                | UnionUnevaled (name, fields) ->
                    let nameSymbol = Atom (name, None)
                    let elemSymbols = fields |> Array.map this.ExprToSymbol |> List.ofArray
                    Symbols (nameSymbol :: elemSymbols, None) :> obj
                | TableUnevaled entries ->
                    let tableSymbol = Atom ("table", None)
                    let elemSymbols =
                        List.map (fun (key, value) ->
                            let keySymbol = this.ExprToSymbol key
                            let valueSymbol = this.ExprToSymbol value
                            Symbols ([keySymbol; valueSymbol], None))
                            entries
                    Symbols (tableSymbol :: elemSymbols, None) :> obj
                | RecordUnevaled (name, fields) ->
                    let recordSymbol = Atom ("record", None)
                    let nameSymbol = Atom (name, None)
                    let fieldSymbols = List.map (fun (name, field) -> Symbols ([Atom (name, None); this.ExprToSymbol field], None)) fields
                    Symbols (recordSymbol :: nameSymbol :: fieldSymbols, None) :> obj
                | Binding (name, _, _, originOpt) ->
                    if name = "index" then Atom ("Index", originOpt) :> obj
                    else Atom (name, originOpt) :> obj
                | TryAlter (expr, expr2, expr3, _, originOpt) ->
                    let index = this.ExprsToIndex expr expr2
                    let tryAlterSymbol = Atom ("tryAlter", None)
                    Symbols ([tryAlterSymbol; index; this.ExprToSymbol expr3], originOpt) :> obj
                | Alter (expr, expr2, expr3, _, originOpt) ->
                    let index = this.ExprsToIndex expr expr2
                    let alterSymbol = Atom ("alter", None)
                    Symbols ([alterSymbol; index; this.ExprToSymbol expr3], originOpt) :> obj
                | Apply (exprs, _, originOpt) ->
                    let exprSymbols = Array.map this.ExprToSymbol exprs
                    Symbols (List.ofArray exprSymbols, originOpt) :> obj
                | ApplyAnd (exprs, _, originOpt) ->
                    let logicSymbol = Atom ("&&", None)
                    let exprSymbols = List.map this.ExprToSymbol (List.ofArray exprs)
                    Symbols (logicSymbol :: exprSymbols, originOpt) :> obj
                | ApplyOr (exprs, _, originOpt) ->
                    let logicSymbol = Atom ("||", None)
                    let exprSymbols = List.map this.ExprToSymbol (List.ofArray exprs)
                    Symbols (logicSymbol :: exprSymbols, originOpt) :> obj
                | Let (binding, body, originOpt) ->
                    let letSymbol = Atom ("let", None)
                    let bindingSymbol = this.BindingToSymbol binding
                    let bodySymbol = this.ExprToSymbol body
                    Symbols ([letSymbol; bindingSymbol; bodySymbol], originOpt) :> obj
                | LetMany (bindings, body, originOpt) ->
                    let letSymbol = Atom ("let", None)
                    let bindingSymbols = List.map (fun binding -> this.BindingToSymbol binding) bindings
                    let bodySymbol = this.ExprToSymbol body
                    Symbols (letSymbol :: bindingSymbols @ [bodySymbol], originOpt) :> obj
                | Intrinsic (name, pars, _, bodyOpt, originOpt) ->
                    let intrinsicSymbol = Atom ("intrinsic", None)
                    let nameSymbol = Atom (name, None)
                    let parSymbols = Array.map (fun par -> Atom (par, None)) pars
                    let parsSymbol = Symbols (List.ofArray parSymbols, None)
                    match bodyOpt with
                    | Some body -> Symbols ([intrinsicSymbol; nameSymbol; parsSymbol; this.ExprToSymbol body], originOpt) :> obj
                    | None -> Symbols ([intrinsicSymbol; nameSymbol; parsSymbol], originOpt) :> obj
                | Fun (pars, _, body, _, _, _, originOpt) ->
                    let funSymbol = Atom ("fun", None)
                    let parSymbols = Array.map (fun par -> Atom (par, None)) pars
                    let parsSymbol = Symbols (List.ofArray parSymbols, None)
                    let bodySymbol = this.ExprToSymbol body
                    Symbols ([funSymbol; parsSymbol; bodySymbol], originOpt) :> obj
                | If (condition, consequent, alternative, originOpt) ->
                    let ifSymbol = Atom ("if", None)
                    let conditionSymbol = this.ExprToSymbol condition
                    let consequentSymbol = this.ExprToSymbol consequent
                    let alternativeSymbol = this.ExprToSymbol alternative
                    Symbols ([ifSymbol; conditionSymbol; consequentSymbol; alternativeSymbol], originOpt) :> obj
                | Match (input, cases, originOpt) ->
                    let matchSymbol = Atom ("match", None)
                    let inputSymbol = this.ExprToSymbol input
                    let caseSymbols =
                        List.map (fun (condition, consequent) ->
                            let conditionSymbol = this.ExprToSymbol condition
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbols ([conditionSymbol; consequentSymbol], None))
                            (List.ofArray cases)
                    Symbols (matchSymbol :: inputSymbol :: caseSymbols, originOpt) :> obj
                | Select (cases, originOpt) ->
                    let selectSymbol = Atom ("select", None)
                    let caseSymbols =
                        List.map (fun (condition, consequent) ->
                            let conditionSymbol = this.ExprToSymbol condition
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbols ([conditionSymbol; consequentSymbol], None))
                            (List.ofArray cases)
                    Symbols (selectSymbol :: caseSymbols, originOpt) :> obj
                | Try (input, cases, originOpt) ->
                    let trySymbol = Atom ("try", None)
                    let inputSymbol = this.ExprToSymbol input
                    let caseSymbols =
                        List.map (fun ((tagNames : string list), consequent) ->
                            let tagSymbol = Atom (String.concat Constants.Scripting.ViolationSeparatorStr tagNames, None)
                            let consequentSymbol = this.ExprToSymbol consequent
                            Symbols ([tagSymbol; consequentSymbol], None))
                            cases
                    Symbols (trySymbol :: inputSymbol :: caseSymbols, originOpt) :> obj
                | Do (exprs, originOpt) ->
                    let doSymbol = Atom ("do", None)
                    let exprSymbols = List.map this.ExprToSymbol exprs
                    Symbols (doSymbol :: exprSymbols, originOpt) :> obj
                | Quote (expr, originOpt) ->
                    Symbol.Quote (this.ExprToSymbol expr, originOpt) :> obj
                | Define (binding, originOpt) ->
                    let defineSymbol = Atom ("define", None)
                    Symbols (defineSymbol :: this.BindingToSymbols binding, originOpt) :> obj
            elif destType = typeof<Expr> then source
            else failconv "Invalid ExprConverter conversion to source." None

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Symbol> ||
            sourceType = typeof<Expr>

        override this.ConvertFrom (_, _, source) =
            match source with
            | :? Symbol as symbol ->
                match symbol with
                | Atom (str, originOpt) ->
                    match str with
                    | "true" | "True" -> Bool true :> obj
                    | "false" | "False" -> Bool false :> obj
                    | "none" | "None" -> Option None :> obj
                    | "nil" -> Keyword String.Empty :> obj
                    | "coempty" -> Codata Empty :> obj
                    | "Index" -> Binding ("index", ref UncachedBinding, ref UnknownBindingType, originOpt) :> obj
                    | "NaN" -> Single Single.NaN :> obj // NOTE: can't tell the difference between a single NaN and a double NaN!
                    | "Infinity" -> Double Double.PositiveInfinity :> obj
                    | "-Infinity" -> Double Double.NegativeInfinity :> obj
                    | "Infinityf" -> Single Single.PositiveInfinity :> obj
                    | "-Infinityf" -> Single Single.NegativeInfinity :> obj
                    | _ ->
                        if Expr.isKeyword str
                        then Keyword str :> obj
                        else Binding (str, ref UncachedBinding, ref UnknownBindingType, originOpt) :> obj
                | Number (str, originOpt) ->
                    match Int32.TryParse str with
                    | (false, _) ->
                        let str = if str.EndsWith "l" || str.EndsWith "L" then str.Substring(0, str.Length - 1) else str
                        match Int64.TryParse str with
                        | (false, _) ->
                            if str.EndsWith "f" || str.EndsWith "F" then
                                let str = str.Substring(0, str.Length - 1)
                                match Single.TryParse str with
                                | (true, single) -> Single single :> obj
                                | (false, _) -> Violation (["InvalidForm"; "Number"], "Unexpected numeric parse failure.", originOpt) :> obj
                            else
                                let str = if str.EndsWith "d" || str.EndsWith "D" then str.Substring(0, str.Length - 1) else str
                                match Double.TryParse (str, Globalization.NumberStyles.Float, Globalization.CultureInfo.CurrentCulture) with
                                | (true, double) -> Double double :> obj
                                | (false, _) -> Violation (["InvalidForm"; "Number"], "Unexpected numeric parse failure.", originOpt) :> obj
                        | (true, int64) -> Int64 int64 :> obj
                    | (true, int) -> Int int :> obj
                | Prime.Text (str, _) -> String str :> obj
                | Prime.Quote (quoted, originOpt) -> Quote (this.SymbolToExpr quoted, originOpt) :> obj
                | Prime.Symbols (symbols, originOpt) ->
                    match symbols with
                    | [] -> Unit :> obj
                    | Atom (name, _) :: tail ->
                        match name with
                        | "&&" ->
                            let args = this.SymbolsToExpr tail
                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                            ApplyAnd (Array.ofList args, breakpoint, originOpt) :> obj
                        | "||" ->
                            let args = this.SymbolsToExpr tail
                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                            ApplyOr (Array.ofList args, breakpoint, originOpt) :> obj
                        | "violation" ->
                            match tail with
                            | [Atom (tagStr, _)]
                            | [Prime.Text (tagStr, _)] ->
                                try let tagName = tagStr in Violation (tagName.Split Constants.Scripting.ViolationSeparator |> List.ofArray, "User-defined Violation.", originOpt) :> obj
                                with exn -> Violation (["InvalidForm"; "Violation"], "Invalid Violation form. Violation tag must be composed of 1 or more valid names.", originOpt) :> obj
                            | [Atom (tagStr, _); Prime.Text (errorMsg, _)]
                            | [Prime.Text (tagStr, _); Prime.Text (errorMsg, _)] ->
                                try let tagName = tagStr in Violation (tagName.Split Constants.Scripting.ViolationSeparator |> List.ofArray, errorMsg, originOpt) :> obj
                                with exn -> Violation (["InvalidForm"; "Violation"], "Invalid Violation form. Violation tag must be composed of 1 or more valid names.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Violation"], "Invalid Violation form. Requires 1 tag.", originOpt) :> obj
                        | "table" ->
                            if List.forall (function Symbols ([_; _], _) -> true | _ -> false) tail then
                                let entries = List.map (function Symbols ([key; value], _) -> (this.SymbolToExpr key, this.SymbolToExpr value) | _ -> failwithumf ()) tail
                                TableUnevaled entries :> obj
                            else Violation (["InvalidForm"; "Table"], "Invalid Table form. Requires 1 or more field definitions.", originOpt) :> obj
                        | "record" ->
                            match tail with
                            | Atom (name, _) :: cases ->
                                if List.forall (function Symbols ([Atom _; _], _) -> true | _ -> false) cases then
                                    let definitions = List.map (function Symbols ([Atom (fieldName, _); fieldValue], _) -> (fieldName, fieldValue) | _ -> failwithumf ()) cases
                                    let definitions = List.map (fun (fieldName, fieldValue) -> (fieldName, this.SymbolToExpr fieldValue)) definitions
                                    RecordUnevaled (name, definitions) :> obj
                                else Violation (["InvalidForm"; "Record"], "Invalid Record form. Requires 1 or more field definitions.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Record"], "Invalid Record form. Requires 1 name and 1 or more field definitions.", originOpt) :> obj
                        | "tryAlter" ->
                            match tail with
                            | [index; body] ->
                                match this.IndexToExprs index with
                                | Some (indexer, target) ->
                                    let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                                    TryAlter (this.SymbolToExpr indexer, this.SymbolToExpr target, this.SymbolToExpr body, breakpoint, originOpt) :> obj
                                | None ->
                                    Violation (["InvalidForm"; "TryAlter"], "Invalid tryAlter form. Requires an index for the first argument.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "TryAlter"], "Invalid tryAlter form. Requires 1 index and 1 value.", originOpt) :> obj
                        | "alter" ->
                            match tail with
                            | [index; body] ->
                                match this.IndexToExprs index with
                                | Some (indexer, target) ->
                                    let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                                    Alter (this.SymbolToExpr indexer, this.SymbolToExpr target, this.SymbolToExpr body, breakpoint, originOpt) :> obj
                                | None ->
                                    Violation (["InvalidForm"; "Alter"], "Invalid alter form. Requires an index for the first argument.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Alter"], "Invalid alter form. Requires 1 index and 1 value.", originOpt) :> obj
                        | "let" ->
                            match tail with
                            | [] -> Violation (["InvalidForm"; "Let"], "Invalid let form. Requires both a binding and a body.", originOpt) :> obj
                            | [_] -> Violation (["InvalidForm"; "Let"], "Invalid let form. Requires both a binding and a body.", originOpt) :> obj
                            | [binding; body] ->
                                match binding with
                                | Symbols (bindingSymbols, _) ->
                                    match this.SymbolsToBindingOpt bindingSymbols with
                                    | Some binding -> Let (binding, this.SymbolToExpr body, originOpt) :> obj
                                    | None -> Violation (["InvalidForm"; "Let"], "Invalid let form. Bindings require both a valid name and an expression.", originOpt) :> obj
                                | _ -> Violation (["InvalidForm"; "Let"], "Invalid let form. Bindings require both a valid name and an expression.", originOpt) :> obj
                            | bindingsAndBody ->
                                let (bindings, body) = (List.allButLast bindingsAndBody, List.last bindingsAndBody)
                                let (bindings, bindingsErrored) = List.split (function Symbols ([_; _], _) -> true | _ -> false) bindings
                                if List.isEmpty bindingsErrored then
                                    let bindings = List.map (function Symbols ([_; _] as binding, _) -> binding | _ -> failwithumf ()) bindings
                                    let bindingOpts = List.map this.SymbolsToBindingOpt bindings
                                    let (bindingOpts, bindingErrors) = List.split Option.isSome bindingOpts
                                    if List.isEmpty bindingErrors then
                                        let bindings = List.definitize bindingOpts
                                        LetMany (bindings, this.SymbolToExpr body, originOpt) :> obj
                                    else Violation (["InvalidForm"; "Let"], "Invalid let form. Bindings require both a valid name and an expression.", originOpt) :> obj
                                else Violation (["InvalidForm"; "Let"], "Invalid let form. Bindings require both a valid name and an expression.", originOpt) :> obj
                        | "intrinsic" ->
                            match tail with
                            | [nameSymbol; parsSymbol]
                            | [nameSymbol; parsSymbol; _] ->
                                match nameSymbol with
                                | Atom (name, _) ->
                                    match parsSymbol with
                                    | Symbols (parSymbols, _) ->
                                        let (pars, parErrors) = List.split (function Atom _ -> true | _ -> false) parSymbols
                                        if List.isEmpty parErrors then
                                            let pars = pars |> List.map (function Atom (str, _) -> str | _ -> failwithumf ()) |> List.toArray
                                            let bodyOpt = match tail with [_; _; bodySymbol] -> Some (this.SymbolToExpr bodySymbol) | _ -> None
                                            Intrinsic (name, pars, Array.length pars, bodyOpt, originOpt) :> obj
                                        else Violation (["InvalidForm"; "Intrinsic"], "Invalid intrinsic form. Intrinsics require both a valid name and a parameter list.", originOpt) :> obj
                                    | _ -> Violation (["InvalidForm"; "Intrinsic"], "Invalid intrinsic form. Intrinsics require both a valid name and a parameter list.", originOpt) :> obj
                                | _ -> Violation (["InvalidForm"; "Intrinsic"], "Invalid intrinsic form. Intrinsics require both a valid name and a parameter list.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Intrinsic"], "Invalid intrinsic form. Intrinsics require both a valid name and a parameter list.", originOpt) :> obj
                        | "fun" ->
                            match tail with
                            | [args; body] ->
                                match args with
                                | Symbols (args, _) ->
                                    if List.forall (function Atom _ -> true | _ -> false) args then
                                        let args = Array.map (function Atom (arg, _) -> arg | _ -> failwithumf ()) (Array.ofList args)
                                        Fun (args, Array.length args, this.SymbolToExpr body, false, None, None, originOpt) :> obj
                                    else Violation (["InvalidForm"; "Function"], "Invalid fun form. Each argument must be a single name.", originOpt) :> obj
                                | _ -> Violation (["InvalidForm"; "Function"], "Invalid fun form. Arguments must be enclosed in brackets.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Function"], "Invalid fun form. Fun requires 1 argument list and 1 body.", originOpt) :> obj
                        | "if" ->
                            match tail with
                            | [condition; consequent; alternative] -> If (this.SymbolToExpr condition, this.SymbolToExpr consequent, this.SymbolToExpr alternative, originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "If"], "Invalid if form. Requires 3 arguments.", originOpt) :> obj
                        | "match" ->
                            match tail with
                            | input :: cases ->
                                let input = this.SymbolToExpr input
                                if List.forall (function Symbols ([_; _], _) -> true | _ -> false) cases then
                                    let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                    let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                    Match (input, Array.ofList cases, originOpt) :> obj
                                else Violation (["InvalidForm"; "Match"], "Invalid match form. Requires 1 or more cases.", originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Match"], "Invalid match form. Requires 1 input and 1 or more cases.", originOpt) :> obj
                        | "select" ->
                            let cases = tail
                            if List.forall (function Symbols ([_; _], _) -> true | _ -> false) cases then
                                let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                Select (Array.ofList cases, originOpt) :> obj
                            else Violation (["InvalidForm"; "Select"], "Invalid select form. Requires 1 or more cases.", originOpt) :> obj
                        | "try" ->
                            match tail with
                            | [body; Symbols (handlers, _)] ->
                                let handlerEirs =
                                    List.mapi
                                        (fun i handler ->
                                            match handler with
                                            | Symbols ([Atom (categoriesStr, _); handlerBody], _) ->
                                                Right (categoriesStr.Split Constants.Scripting.ViolationSeparator |> List.ofArray, handlerBody)
                                            | _ ->
                                                Left ("Invalid try handler form for handler #" + scstring (inc i) + ". Requires 1 path and 1 body."))
                                        handlers
                                let (errors, handlers) = Either.split handlerEirs
                                match errors with
                                | [] -> Try (this.SymbolToExpr body, List.map (mapSnd this.SymbolToExpr) handlers, originOpt) :> obj
                                | error :: _ -> Violation (["InvalidForm"; "Try"], error, originOpt) :> obj
                            | _ -> Violation (["InvalidForm"; "Try"], "Invalid try form. Requires 1 body and a handler list.", originOpt) :> obj
                        | "do" ->
                            match tail with
                            | [] -> Violation (["InvalidForm"; "Do"], "Invalid do form. Requires 1 or more sub-expressions.", originOpt) :> obj
                            | symbols ->
                                let exprs = this.SymbolsToExpr symbols
                                Do (exprs, originOpt) :> obj
                        | "define" ->
                            let bindingSymbols = tail
                            match this.SymbolsToBindingOpt bindingSymbols with
                            | Some binding -> Define (binding, originOpt) :> obj
                            | None -> Violation (["InvalidForm"; "Define"], "Invalid define form. Invalid binding.", originOpt) :> obj
                        | _ ->
                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                            Apply (Array.ofList (this.SymbolsToExpr symbols), breakpoint, originOpt) :> obj
                    | _ ->
                        let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                        Apply (Array.ofList (this.SymbolsToExpr symbols), breakpoint, originOpt) :> obj
            | :? Expr -> source
            | _ -> failconv "Invalid ExprConverter conversion from source." None

    /// The true value in scripting.
    let TrueValue = Bool true
    
    /// The false value in scripting.
    let FalseValue = Bool false
    
    /// The none value in scripting.
    let NoneValue = Option None
    
    /// A declaration bindings frame in a scripting environment.
    type DeclarationFrame = Dictionary<string, Expr>
    
    /// A declaration bindings frame in a scripting environment.
    type ProceduralFrame = (struct (string * Expr)) array
    
    /// The manner in which bindings are added to a frame.
    type AddType =
        | AddToNewFrame of Size : int
        | AddToHeadFrame of Offset : int

    [<AutoOpen>]
    module Env =
    
        /// The execution environment for scripts.
        type [<NoEquality; NoComparison>] Env =
            private
                { GlobalFrame : DeclarationFrame
                  mutable LocalFrame : DeclarationFrame
                  mutable ProceduralFrames : ProceduralFrame list }
    
        [<RequireQualifiedAccess>]
        module Env =
    
            let private BottomBinding =
                struct (String.Empty, Violation (["BottomAccess"], "Accessed a Bottom value.", None))

            let getLocalFrame env =
                env.LocalFrame

            let setLocalFrame localFrame env =
                env.LocalFrame <- localFrame

            let getGlobalFrame env =
                env.GlobalFrame

            let private makeProceduralFrame size =
                Array.create size BottomBinding

            let private addProceduralFrame frame env =
                env.ProceduralFrames <- frame :: env.ProceduralFrames

            let private tryGetDeclarationBinding name env =
                match env.LocalFrame.TryGetValue name with
                | (false, _) ->
                    match env.GlobalFrame.TryGetValue name with
                    | (false, _) -> None
                    | (true, value) -> Some value
                | (true, value) -> Some value
    
            let private tryGetProceduralBinding name env =
                let offsetRef = ref -1
                let indexOptRef = ref None
                let optBinding =
                    List.tryFindPlus
                        (fun frame ->
                            offsetRef := !offsetRef + 1
                            indexOptRef := Array.tryFindIndexBack (fun struct (bindingName, _) -> name.Equals bindingName) frame // OPTIMIZATION: faster than (=) here
                            match !indexOptRef with
                            | Some index -> Some frame.[index]
                            | None -> None)
                        env.ProceduralFrames
                match optBinding with
                | Some struct (_, binding) -> Some (struct (binding, !offsetRef, (!indexOptRef).Value))
                | None -> None

            let tryGetBinding name cachedBinding (_ : BindingType ref) env =
                match !cachedBinding with
                | UncachedBinding ->
                    match tryGetProceduralBinding name env with
                    | None ->
                        match tryGetDeclarationBinding name env with
                        | Some binding as bindingOpt ->
#if DEBUG
                            // NOTE: when debugging, we allow declaration bindings to be redefined, thus we can't cache...
                            ignore binding
#else
                            // ...otherwise we can cache since bindings will be immutable
                            cachedBinding := DeclarationBinding binding
#endif
                            bindingOpt
                        | None -> None
                    | Some struct (binding, offset, index) ->
                        cachedBinding := ProceduralBinding (offset, index)
                        Some binding
                | DeclarationBinding binding ->
                    Some binding
                | ProceduralBinding (offset, index) ->
                    let frame = (List.skip offset env.ProceduralFrames).Head
                    let struct (_, binding) = frame.[index]
                    Some binding

            let tryAddDeclarationBinding name value env =
                let isTopLevel = List.isEmpty env.ProceduralFrames
                if isTopLevel then
#if DEBUG
                    env.LocalFrame.ForceAdd (name, value); true
#else
                    env.LocalFrame.TryAdd (name, value)
#endif
                else false
    
            let addProceduralBinding appendType name value env =
                match appendType with
                | AddToNewFrame size ->
                    let frame = makeProceduralFrame size
                    frame.[0] <- struct (name, value)
                    addProceduralFrame frame env
                | AddToHeadFrame offset ->
                    match env.ProceduralFrames with
                    | frame :: _ -> frame.[offset] <- struct (name, value)
                    | [] -> failwithumf ()
    
            let addProceduralBindings appendType bindings env =
                match appendType with
                | AddToNewFrame size ->
                    let frame = makeProceduralFrame size
                    let mutable index = 0
                    for binding in bindings do
                        frame.[index] <- binding
                        index <- index + 1
                    addProceduralFrame frame env
                | AddToHeadFrame start ->
                    match env.ProceduralFrames with
                    | frame :: _ ->
                        let mutable index = start
                        for binding in bindings do
                            frame.[index] <- binding
                            index <- index + 1
                    | [] -> failwithumf ()

            let removeProceduralBindings env =
                match env.ProceduralFrames with
                | [] -> failwithumf ()
                | _ :: tail -> env.ProceduralFrames <- tail

            let getProceduralFrames env =
                env.ProceduralFrames

            let setProceduralFrames proceduralFrames env =
                env.ProceduralFrames <- proceduralFrames

            let make () =
                // NOTE: local frame starts out the same as the global frame so that prelude
                // functions are defined globally
                let globalFrame = DeclarationFrame HashIdentity.Structural
                { GlobalFrame = globalFrame
                  LocalFrame = globalFrame
                  ProceduralFrames = [] }

    /// The execution environment for scripts.
    type Env = Env.Env

    /// Attempting to expose Env module contents as well, but does not seem to work...
    module Env = Env.Env
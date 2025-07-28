// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open Scripting
module ScriptingBinary =

    type [<ReferenceEquality>] BinaryFns =
        { Bool : bool -> bool -> SymbolOrigin ValueOption -> Expr
          Int : int -> int -> SymbolOrigin ValueOption -> Expr
          Int64 : int64 -> int64 -> SymbolOrigin ValueOption -> Expr
          Single : single -> single -> SymbolOrigin ValueOption -> Expr
          Double : double -> double -> SymbolOrigin ValueOption -> Expr
          String : string -> string -> SymbolOrigin ValueOption -> Expr
          Keyword : string -> string -> SymbolOrigin ValueOption -> Expr
          Tuple : Expr array -> Expr array -> SymbolOrigin ValueOption -> Expr
          Union : string -> Expr array -> string -> Expr array -> SymbolOrigin ValueOption -> Expr
          Option : Expr option -> Expr option -> SymbolOrigin ValueOption -> Expr
          Codata : Codata -> Codata -> SymbolOrigin ValueOption -> Expr
          List : Expr list -> Expr list -> SymbolOrigin ValueOption -> Expr
          Ring : Expr Set -> Expr Set -> SymbolOrigin ValueOption -> Expr
          Table : Map<Expr, Expr> -> Map<Expr, Expr> -> SymbolOrigin ValueOption -> Expr
          Record : string -> Map<string, int> -> Expr array -> string -> Map<string, int> -> Expr array -> SymbolOrigin ValueOption -> Expr }

    let EqFns =
        { Bool = fun left right _ -> Bool (left = right)
          Int = fun left right _ -> Bool (left = right)
          Int64 = fun left right _ -> Bool (left = right)
          Single = fun left right _ -> Bool (left = right)
          Double = fun left right _ -> Bool (left = right)
          String = fun left right _ -> Bool (left = right)
          Keyword = fun left right _ -> Bool (left = right)
          Tuple = fun left right _ -> Bool (left = right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) = (keywordRight, fieldsRight))
          Option = fun left right _ -> Bool (left = right)
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "Eq"], "Equality not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left = right)
          Ring = fun left right _ -> Bool (left = right)
          Table = fun left right _ -> Bool (left = right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) = (keywordRight, mapRight, fieldsRight)) }

    let NotEqFns =
        { Bool = fun left right _ -> Bool (left <> right)
          Int = fun left right _ -> Bool (left <> right)
          Int64 = fun left right _ -> Bool (left <> right)
          Single = fun left right _ -> Bool (left <> right)
          Double = fun left right _ -> Bool (left <> right)
          String = fun left right _ -> Bool (left <> right)
          Keyword = fun left right _ -> Bool (left <> right)
          Tuple = fun left right _ -> Bool (left <> right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) <> (keywordRight, fieldsRight))
          Option = fun left right _ -> Bool (left <> right)
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "NotEq"], "Equality not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left <> right)
          Ring = fun left right _ -> Bool (left <> right)
          Table = fun left right _ -> Bool (left <> right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) <> (keywordRight, mapRight, fieldsRight)) }

    let LtFns =
        { Bool = fun left right _ -> Bool (left < right)
          Int = fun left right _ -> Bool (left < right)
          Int64 = fun left right _ -> Bool (left < right)
          Single = fun left right _ -> Bool (left < right)
          Double = fun left right _ -> Bool (left < right)
          String = fun left right _ -> Bool (left < right)
          Keyword = fun left right _ -> Bool (left < right)
          Tuple = fun left right _ -> Bool (left < right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) < (keywordRight, fieldsRight))
          Option = fun left right _ -> Bool (left < right)
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "Lt"], "Comparison not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left < right)
          Ring = fun left right _ -> Bool (left < right)
          Table = fun left right _ -> Bool (left < right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) < (keywordRight, mapRight, fieldsRight)) }

    let GtFns =
        { Bool = fun left right _ -> Bool (left > right)
          Int = fun left right _ -> Bool (left > right)
          Int64 = fun left right _ -> Bool (left > right)
          Single = fun left right _ -> Bool (left > right)
          Double = fun left right _ -> Bool (left > right)
          String = fun left right _ -> Bool (left > right)
          Keyword = fun left right _ -> Bool (left > right)
          Tuple = fun left right _ -> Bool (left > right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) > (keywordRight, fieldsRight))
          Option = fun left right _ -> Bool (left > right)
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "Gt"], "Comparison not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left > right)
          Ring = fun left right _ -> Bool (left > right)
          Table = fun left right _ -> Bool (left > right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) > (keywordRight, mapRight, fieldsRight)) }

    let LtEqFns =
        { Bool = fun left right _ -> Bool (left <= right)
          Int = fun left right _ -> Bool (left <= right)
          Int64 = fun left right _ -> Bool (left <= right)
          Single = fun left right _ -> Bool (left <= right)
          Double = fun left right _ -> Bool (left <= right)
          String = fun left right _ -> Bool (left <= right)
          Keyword = fun left right _ -> Bool (left <= right)
          Tuple = fun left right _ -> Bool (left <= right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) <= (keywordRight, fieldsRight))
          Option = fun left right _ -> Bool (left <= right)
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "LtEq"], "Comparison not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left <= right)
          Ring = fun left right _ -> Bool (left <= right)
          Table = fun left right _ -> Bool (left <= right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) <= (keywordRight, mapRight, fieldsRight)) }

    let GtEqFns =
        { Bool = fun left right _ -> Bool (left >= right)
          Int = fun left right _ -> Bool (left >= right)
          Int64 = fun left right _ -> Bool (left >= right)
          Single = fun left right _ -> Bool (left >= right)
          Double = fun left right _ -> Bool (left >= right)
          String = fun left right _ -> Bool (left >= right)
          Keyword = fun left right _ -> Bool (left >= right)
          Tuple = fun left right _ -> Bool (left >= right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) >= (keywordRight, fieldsRight))
          Option = fun left right _ -> Bool (left >= right)
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "GtEq"], "Comparison not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left >= right)
          Ring = fun left right _ -> Bool (left >= right)
          Table = fun left right _ -> Bool (left >= right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) >= (keywordRight, mapRight, fieldsRight)) }

    let AddFns =
        { Bool = fun left right _ -> Bool (if left && right then false elif left then true elif right then true else false)
          Int = fun left right _ -> Int (left + right)
          Int64 = fun left right _ -> Int64 (left + right)
          Single = fun left right _ -> Single (left + right)
          Double = fun left right _ -> Double (left + right)
          String = fun left right _ -> String (left + right)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add Unions.", originOpt)
          Option = fun left right originOpt ->
            match (left, right) with
            | (Some _, Some _) -> Violation (["ArgumentOutOfRange"; "Add"], "Cannot add two some values.", originOpt)
            | (Some left, None) -> left
            | (None, Some right) -> right
            | (None, None) -> NoneValue
          Codata = fun left right _ -> Codata (Add (left, right))
          List = fun left right _ -> List (left @ right)
          Ring = fun left right _ -> Ring (Set.union left right)
          Table = fun left right _ -> Table (left @@ right)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add Records.", originOpt) }

    let SubFns =
        { Bool = fun left right _ -> Bool (if left && right then false elif left then true elif right then true else false)
          Int = fun left right _ -> Int (left - right)
          Int64 = fun left right _ -> Int64 (left - right)
          Single = fun left right _ -> Single (left - right)
          Double = fun left right _ -> Double (left - right)
          String = fun left right _ -> String (left.Replace (right, String.Empty))
          Keyword = fun left right _ -> String (left.Replace (right, String.Empty))
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Unions.", originOpt)
          Option = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Options.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Lists.", originOpt)
          Ring = fun left right _ -> Ring (Set.difference left right)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Records.", originOpt) }

    let MulFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Keyword.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Unions.", originOpt)
          Option = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Options.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Records.", originOpt) }

    let DivFns =
        { Bool = fun left right originOpt -> if right = false then Violation (["ArgumentOutOfRange"; "Div"], "Cannot divide by a false Bool.", originOpt) else Bool (if left && right then true else false)
          Int = fun left right originOpt -> if right = 0 then Violation (["ArgumentOutOfRange"; "Div"], "Cannot divide by a zero Int.", originOpt) else Int (left / right)
          Int64 = fun left right originOpt -> if right = 0L then Violation (["ArgumentOutOfRange"; "Div"], "Cannot divide by a zero Int64.", originOpt) else Int64 (left / right)
          Single = fun left right _ -> Single (left / right)
          Double = fun left right _ -> Double (left / right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Unions.", originOpt)
          Option = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Options.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Records.", originOpt) }

    let ModFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Bools.", originOpt)
          Int = fun left right originOpt -> if right = 0 then Violation (["ArgumentOutOfRange"; "Mod"], "Cannot modulate by a zero Int.", originOpt) else Int (left % right)
          Int64 = fun left right originOpt -> if right = 0L then Violation (["ArgumentOutOfRange"; "Mod"], "Cannot divide by a zero Int64.", originOpt) else Int64 (left % right)
          Single = fun left right _ -> Single (left % right)
          Double = fun left right _ -> Double (left % right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Unions.", originOpt)
          Option = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Options.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Records.", originOpt) }

    let PowFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Bools.", originOpt)
          Int = fun left right _ -> Int (int $ Math.Pow (double left, double right))
          Int64 = fun left right _ -> Int64 (int64 $ Math.Pow (double left, double right))
          Single = fun left right _ -> Single (single $ Math.Pow (double left, double right))
          Double = fun left right _ -> Double (Math.Pow (double left, double right))
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Unions.", originOpt)
          Option = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Options.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Records.", originOpt) }

    let RootFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Bools.", originOpt)
          Int = fun left right _ -> Int (int $ Math.Pow (double left, 1.0 / double right))
          Int64 = fun left right _ -> Int64 (int64 $ Math.Pow (double left, 1.0 / double right))
          Single = fun left right _ -> Single (single $ Math.Pow (double left, 1.0 / double right))
          Double = fun left right _ -> Double (Math.Pow (double left, 1.0 / double right))
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Unions.", originOpt)
          Option = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Options.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Records.", originOpt) }

    let CrossFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Unions.", originOpt)
          Option = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Options.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Records.", originOpt) }

    let DotFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Unions.", originOpt)
          Option = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Options.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Records.", originOpt) }

    let evalBinaryInner (fns : BinaryFns) fnName evaledLeft evaledRight originOpt (_ : 'w) =
        match (evaledLeft, evaledRight) with
        | (Bool boolLeft, Bool boolRight) -> fns.Bool boolLeft boolRight originOpt
        | (Int intLeft, Int intRight) -> fns.Int intLeft intRight originOpt
        | (Int64 int64Left, Int64 int64Right) -> fns.Int64 int64Left int64Right originOpt
        | (Single singleLeft, Single singleRight) -> fns.Single singleLeft singleRight originOpt
        | (Double doubleLeft, Double doubleRight) -> fns.Double doubleLeft doubleRight originOpt
        | (String stringLeft, String stringRight) -> fns.String stringLeft stringRight originOpt
        | (Keyword keywordLeft, Keyword keywordRight) -> fns.String keywordLeft keywordRight originOpt
        | (Tuple tupleLeft, Tuple tupleRight) -> fns.Tuple tupleLeft tupleRight originOpt
        | (Union (nameLeft, fieldsLeft), Union (nameRight, fieldsRight)) -> fns.Union nameLeft fieldsLeft nameRight fieldsRight originOpt
        | (Codata codataLeft, Codata codataRight) -> fns.Codata codataLeft codataRight originOpt
        | (Option optionLeft, Option optionRight) -> fns.Option optionLeft optionRight originOpt
        | (List listLeft, List listRight) -> fns.List listLeft listRight originOpt
        | (Ring ringLeft, Ring ringRight) -> fns.Ring ringLeft ringRight originOpt
        | (Table tableLeft, Table tableRight) -> fns.Table tableLeft tableRight originOpt
        | (Violation _ as violation, _) -> violation
        | (_, (Violation _ as violation)) -> violation
        | _ -> Violation (["InvalidArgumentType"; (String.capitalize fnName)], "Cannot apply a binary function on unlike or incompatible values.", originOpt)

    let evalBinary fns fnName argsEvaled originOpt (world : 'w) =
        match argsEvaled with
        | [|evaledLeft; evaledRight|] -> evalBinaryInner fns fnName evaledLeft evaledRight originOpt world
        | _ -> Violation (["InvalidArgumentCount"; (String.capitalize fnName)], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt)
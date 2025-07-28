﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open Scripting
module ScriptingUnary =

    type [<ReferenceEquality>] UnaryFns =
        { Bool : bool -> SymbolOrigin ValueOption -> Expr
          Int : int -> SymbolOrigin ValueOption -> Expr
          Int64 : int64 -> SymbolOrigin ValueOption -> Expr
          Single : single -> SymbolOrigin ValueOption -> Expr
          Double : double -> SymbolOrigin ValueOption -> Expr
          String : string -> SymbolOrigin ValueOption -> Expr
          Keyword : string -> SymbolOrigin ValueOption -> Expr
          Tuple : Expr array -> SymbolOrigin ValueOption -> Expr
          Union : string -> Expr array -> SymbolOrigin ValueOption -> Expr
          Option : Expr option -> SymbolOrigin ValueOption -> Expr
          Codata : Codata -> SymbolOrigin ValueOption -> Expr
          List : Expr list -> SymbolOrigin ValueOption -> Expr
          Ring : Expr Set -> SymbolOrigin ValueOption -> Expr
          Table : Map<Expr, Expr> -> SymbolOrigin ValueOption -> Expr
          Record : string -> Map<string, int> -> Expr array -> SymbolOrigin ValueOption -> Expr }

    let HashFns =
        { Bool = fun value _ -> Int (hash value)
          Int = fun value _ -> Int (hash value)
          Int64 = fun value _ -> Int (hash value)
          Single = fun value _ -> Int (hash value)
          Double = fun value _ -> Int (hash value)
          String = fun value _ -> Int (hash value)
          Keyword = fun value _ -> Int (hash value)
          Tuple = fun value _ -> Int (hash value)
          Union = fun name fields _ -> Int (hash (name, fields))
          Option = fun value _ -> Int (hash value)
          Codata = fun value _ -> Int (hash value)
          List = fun value _ -> Int (hash value)
          Ring = fun value _ -> Int (hash value)
          Table = fun value _ -> Int (hash value)
          Record = fun name map fields _ -> Int (hash (name, map, fields)) }

    let EmptyFns =
        { Bool = fun _ _ -> Bool false
          Int = fun _ _ -> Int 0
          Int64 = fun _ _ -> Int64 0L
          Single = fun _ _ -> Single 0.0f
          Double = fun _ _ -> Double 0.0
          String = fun _ _ -> String String.Empty
          Keyword = fun _ _ -> Keyword String.Empty
          Tuple = fun _ _ -> Tuple Array.empty
          Union = fun _ _ _ -> Union (String.Empty, Array.empty)
          Option = fun _ _ -> NoneValue
          Codata = fun _ _ -> Codata Empty
          List = fun _ _ -> List []
          Ring = fun _ _ -> Ring Set.empty
          Table = fun _ _ -> Table Map.empty
          Record = fun _ _ _ _ -> Record (String.Empty, Map.empty, Array.empty) }

    let IdentityFns =
        { Bool = fun _ _ -> Bool true
          Int = fun _ _ -> Int 1
          Int64 = fun _ _ -> Int64 1L
          Single = fun _ _ -> Single 1.0f
          Double = fun _ _ -> Double 1.0
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert a String to an identity representation.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert a Keyword to an identity representation.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert a Tuple to an identity representation.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert a Union to an identity representation.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert Option to an identity representation.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert Codata to an identity representation.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert a List to an identity representation.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert a Ring to an identity representation.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert a Table to an identity representation.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Identity"], "Cannot convert a Record to an identity representation.", originOpt) }

    let MinimumFns =
        { Bool = fun _ _ -> Bool false
          Int = fun _ _ -> Int Int32.MinValue
          Int64 = fun _ _ -> Int64 Int64.MinValue
          Single = fun _ _ -> Single Single.MinValue
          Double = fun _ _ -> Double Double.MinValue
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert a String to a minimum representation.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert a Keyword to a minimum representation.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert a Tuple to a minimum representation.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert a Union to a minimum representation.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert Option to a minimum representation.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert Codata to a minimum representation.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert a List to a minimum representation.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert a Ring to a minimum representation.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert a Table to a minimum representation.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Minimum"], "Cannot convert a Record to an minimum representation.", originOpt) }

    let MaximumFns =
        { Bool = fun _ _ -> Bool true
          Int = fun _ _ -> Int Int32.MaxValue
          Int64 = fun _ _ -> Int64 Int64.MaxValue
          Single = fun _ _ -> Single Single.MaxValue
          Double = fun _ _ -> Double Double.MaxValue
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert a String to a maximum representation.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert a Keyword to a maximum representation.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert a Tuple to a maximum representation.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert a Union to a maximum representation.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert Option to a maximum representation.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert Codata to a maximum representation.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert a List to a maximum representation.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert a Ring to a maximum representation.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert a Table to a maximum representation.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Maximum"], "Cannot convert a Record to an maximum representation.", originOpt) }

    let IncFns =
        { Bool = fun value _ -> Bool (if value then false else true)
          Int = fun value _ -> Int (inc value)
          Int64 = fun value _ -> Int64 (inc value)
          Single = fun value _ -> Single (inc value)
          Double = fun value _ -> Double (inc value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Inc"], "Cannot increment a Record.", originOpt) }

    let DecFns =
        { Bool = fun value _ -> Bool (if value then false else true)
          Int = fun value _ -> Int (dec value)
          Int64 = fun value _ -> Int64 (dec value)
          Single = fun value _ -> Single (dec value)
          Double = fun value _ -> Double (dec value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Dec"], "Cannot decrement a Record.", originOpt) }

    let NegateFns =
        { Bool = fun value _ -> Bool (if value then false else true)
          Int = fun value _ -> Int (0 - value)
          Int64 = fun value _ -> Int64 (0L - value)
          Single = fun value _ -> Single (0.0f - value)
          Double = fun value _ -> Double (0.0 - value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Negate"], "Cannot negate a Record.", originOpt) }

    let SqrFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a Bool.", originOpt)
          Int = fun value _ -> Int (value * value)
          Int64 = fun value _ -> Int64 (value * value)
          Single = fun value _ -> Single (value * value)
          Double = fun value _ -> Double (value * value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sqr"], "Cannot square a Record.", originOpt) }

    let SqrtFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Sqrt (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Sqrt (double value))
          Single = fun value _ -> Single (single $ Math.Sqrt (double value))
          Double = fun value _ -> Double (Math.Sqrt value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sqtr"], "Cannot square root a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sqrt"], "Cannot square root a Record.", originOpt) }

    let FloorFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Floor (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Floor (double value))
          Single = fun value _ -> Single (single $ Math.Floor (double value))
          Double = fun value _ -> Double (Math.Floor value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Floor"], "Cannot floor a Record.", originOpt) }

    let CeilingFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Ceiling (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Ceiling (double value))
          Single = fun value _ -> Single (single $ Math.Ceiling (double value))
          Double = fun value _ -> Double (Math.Ceiling value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of an Otion.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Ceiling"], "Cannot get ceiling of a Record.", originOpt) }

    let TruncateFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Truncate (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Truncate (double value))
          Single = fun value _ -> Single (single $ Math.Truncate (double value))
          Double = fun value _ -> Double (Math.Truncate value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Truncate"], "Cannot truncate a Record.", originOpt) }

    let ExpFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Exp (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Exp (double value))
          Single = fun value _ -> Single (single $ Math.Exp (double value))
          Double = fun value _ -> Double (Math.Exp value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Exp"], "Cannot exponentiate a Record.", originOpt) }

    let RoundFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Round (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Round (double value))
          Single = fun value _ -> Single (single $ Math.Round (double value))
          Double = fun value _ -> Double (Math.Round value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Round"], "Cannot round a Record.", originOpt) }

    let LogFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a Bool.", originOpt)
          Int = fun value originOpt -> if value = 0 then Violation (["ArgumentOutOfRange"; "Log"], "Cannot log a zero Int.", originOpt) else Int (int $ Math.Log (double value))
          Int64 = fun value originOpt -> if value = 0L then Violation (["ArgumentOutOfRange"; "Log"], "Cannot log a zero Int64.", originOpt) else Int64 (int64 $ Math.Log (double value))
          Single = fun value originOpt -> if value = 0.0f then Violation (["ArgumentOutOfRange"; "Log"], "Cannot log a zero Single.", originOpt) else Single (single $ Math.Log (double value))
          Double = fun value originOpt -> if value = 0.0 then Violation (["ArgumentOutOfRange"; "Log"], "Cannot log a zero Double.", originOpt) else Double (Math.Log value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Log"], "Cannot log a Record.", originOpt) }

    let SinFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Sin (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Sin (double value))
          Single = fun value _ -> Single (single $ Math.Sin (double value))
          Double = fun value _ -> Double (Math.Sin value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sin"], "Cannot sin a Record.", originOpt) }

    let CosFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Cos (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Cos (double value))
          Single = fun value _ -> Single (single $ Math.Cos (double value))
          Double = fun value _ -> Double (Math.Cos value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Cos"], "Cannot cos a Record.", originOpt) }

    let TanFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Tan (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Tan (double value))
          Single = fun value _ -> Single (single $ Math.Tan (double value))
          Double = fun value _ -> Double (Math.Tan value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Tan"], "Cannot tan a Record.", originOpt) }

    let AsinFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Asin (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Asin (double value))
          Single = fun value _ -> Single (single $ Math.Asin (double value))
          Double = fun value _ -> Double (Math.Asin value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Asin"], "Cannot asin a Record.", originOpt) }

    let AcosFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Acos (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Acos (double value))
          Single = fun value _ -> Single (single $ Math.Acos (double value))
          Double = fun value _ -> Double (Math.Acos value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Acos"], "Cannot acos a Record.", originOpt) }

    let AtanFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a Bool.", originOpt)
          Int = fun value _ -> Int (int $ Math.Atan (double value))
          Int64 = fun value _ -> Int64 (int64 $ Math.Atan (double value))
          Single = fun value _ -> Single (single $ Math.Atan (double value))
          Double = fun value _ -> Double (Math.Atan value)
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Atan"], "Cannot atan a Record.", originOpt) }

    let LengthFns =
        { Bool = fun value _ -> Int (if value then 1 else 0)
          Int = fun value _ -> Int (Math.Abs value)
          Int64 = fun value _ -> Int64 (Math.Abs value)
          Single = fun value _ -> Single (Math.Abs value)
          Double = fun value _ -> Double (Math.Abs value)
          String = fun value _ -> Int (value.Length)
          Keyword = fun value _ -> Int (value.Length)
          Tuple = fun value _ -> Int (Array.length value)
          Union = fun _ fields _ -> Int (Array.length fields)
          Option = fun value _ -> Int (Option.length value)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Length"], "Cannot get length of Codata.", originOpt)
          List = fun value _ -> Int (List.length value)
          Ring = fun value _ -> Int (value.Count)
          Table = fun value _ -> Int (value.Count)
          Record = fun _ _ fields _ -> Int (Array.length fields) }

    let NormalFns =
        { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a Bool.", originOpt)
          Int = fun value originOpt -> if value = 0 then Violation (["ArgumentOutOfRange"; "Normal"], "Cannot get the normal of a zero Int.", originOpt) elif value < 0 then Int -1 else Int 1
          Int64 = fun value originOpt -> if value = 0L then Violation (["ArgumentOutOfRange"; "Normal"], "Cannot get the normal of a zero Int64.", originOpt) elif value < 0L then Int64 -1L else Int64 1L
          Single = fun value originOpt -> if value = 0.0f then Violation (["ArgumentOutOfRange"; "Normal"], "Cannot get the normal of a zero Single.", originOpt) elif value < 0.0f then Single -1.0f else Single 1.0f
          Double = fun value originOpt -> if value = 0.0 then Violation (["ArgumentOutOfRange"; "Normal"], "Cannot get the normal of a zero Double.", originOpt) elif value < 0.0 then Double -1.0 else Double 1.0
          String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a String.", originOpt)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a Keyword.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a Tuple.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a Union.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize an Option.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize Codata.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a List.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a Ring.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a Table.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Normal"], "Cannot normalize a Record.", originOpt) }

    let BoolFns =
        { Bool = fun value _ -> Bool (value)
          Int = fun value _ -> Bool (value = 0)
          Int64 = fun value _ -> Bool (value = 0L)
          Single = fun value _ -> Bool (value = 0.0f)
          Double = fun value _ -> Bool (value = 0.0)
          String = fun value _ -> Bool (scvalue value)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert a Keyword to a Bool.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert a Tuple to a Bool.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert a Union to a Bool.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert an Option to a Bool.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert Codata to a Bool.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert a List to a Bool.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert a Ring to a Bool.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert a Table to a Bool.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Bool"], "Cannot convert a Record to a Bool.", originOpt) }

    let IntFns =
        { Bool = fun value _ -> Int (if value then 1 else 0)
          Int = fun value _ -> Int (value)
          Int64 = fun value _ -> Int (int value)
          Single = fun value _ -> Int (int value)
          Double = fun value _ -> Int (int value)
          String = fun value _ -> Int (scvalue value)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert a Keyword to an Int.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert a Tuple to an Int.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert a Union to an Int.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert an Option to a Int.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert Codata to an Int.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert a List to an Int.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert a Ring to an Int.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert a Table to an Int.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Int"], "Cannot convert a Record to an Int.", originOpt) }

    let Int64Fns =
        { Bool = fun value _ -> Int64 (if value then 1L else 0L)
          Int = fun value _ -> Int64 (int64 value)
          Int64 = fun value _ -> Int64 (value)
          Single = fun value _ -> Int64 (int64 value)
          Double = fun value _ -> Int64 (int64 value)
          String = fun value _ -> Int64 (scvalue value)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert a Keyword to an Int64.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert a Tuple to an Int64.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert a Union to an Int64.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert an Option to an Int64.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert Codata to an Int64.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert a List to an Int64.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert a Ring to an Int64.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert a Table to an Int64.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Int64"], "Cannot convert a Record to an Int64.", originOpt) }

    let SingleFns =
        { Bool = fun value _ -> Single (if value then 1.0f else 0.0f)
          Int = fun value _ -> Single (single value)
          Int64 = fun value _ -> Single (single value)
          Single = fun value _ -> Single (value)
          Double = fun value _ -> Single (single value)
          String = fun value _ -> Single (scvalue value)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert a Keyword to a Single.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert a Tuple to a Single.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert a Union to a Single.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert an Option to a Single.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert Codata to a Single.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert a List to a Single.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert a Ring to a Single.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert a Table to a Single.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Single"], "Cannot convert a Record to a Single.", originOpt) }

    let DoubleFns =
        { Bool = fun value _ -> Double (if value then 1.0 else 0.0)
          Int = fun value _ -> Double (double value)
          Int64 = fun value _ -> Double (double value)
          Single = fun value _ -> Double (double value)
          Double = fun value _ -> Double (value)
          String = fun value _ -> Double (scvalue value)
          Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert a Keyword to a Double.", originOpt)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert a Tuple to a Double.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert a Union to a Double.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert an Option to a Double.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert Codata to a Double.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert a List to a Double.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert a Ring to a Double.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert a Table to a Double.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Double"], "Cannot convert a Record to a Double.", originOpt) }

    let StringFns =
        { Bool = fun value _ -> String (scstring value)
          Int = fun value _ -> String (scstring value)
          Int64 = fun value _ -> String (scstring value)
          Single = fun value _ -> String (scstring value)
          Double = fun value _ -> String (scstring value)
          String = fun value _ -> String (value)
          Keyword = fun value _ -> String (value)
          Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "String"], "Cannot convert a Tuple to a String.", originOpt)
          Union = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "String"], "Cannot convert a Union to a String.", originOpt)
          Option = fun _ originOpt -> Violation (["InvalidArgumentType"; "String"], "Cannot convert an Option to a String.", originOpt)
          Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "String"], "Cannot convert Codata to a String.", originOpt)
          List = fun _ originOpt -> Violation (["InvalidArgumentType"; "String"], "Cannot convert a List to a String.", originOpt)
          Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "String"], "Cannot convert a Ring to a String.", originOpt)
          Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "String"], "Cannot convert a Table to a String.", originOpt)
          Record = fun _ _ _ originOpt -> Violation (["InvalidArgumentType"; "String"], "Cannot convert a Record to a String.", originOpt) }

    let evalBoolUnary fn fnName argsEvaled originOpt (_ : 'w) =
        match argsEvaled with
        | [|argEvaled|] ->
            match argEvaled with
            | Violation _ as violation -> violation
            | Bool bool -> Bool (fn bool)
            | _ -> Violation (["InvalidArgumentType"; (String.capitalize fnName)], "Cannot apply a Bool function to a non-Bool value.", originOpt)
        | _ -> Violation (["InvalidArgumentCount"; (String.capitalize fnName)], "Incorrect number of arguments for '" + fnName + "'; 1 argument required.", originOpt)

    let evalUnaryInner (fns : UnaryFns) fnName argEvaled originOpt (_ : 'w) : Expr =
        match argEvaled with
        | Violation _ as violation -> violation
        | Bool bool -> fns.Bool bool originOpt
        | Int int -> fns.Int int originOpt
        | Int64 int64 -> fns.Int64 int64 originOpt
        | Single single -> fns.Single single originOpt
        | Double double -> fns.Double double originOpt
        | String string -> fns.String string originOpt
        | Keyword keyword -> fns.Keyword keyword originOpt
        | Tuple tuple -> fns.Tuple tuple originOpt
        | Union (name, union) -> fns.Union name union originOpt
        | Option opt -> fns.Option opt originOpt
        | Codata codata -> fns.Codata codata originOpt
        | List list -> fns.List list originOpt
        | Ring ring -> fns.Ring ring originOpt
        | Table table -> fns.Table table originOpt
        | Record (name, map, fields) -> fns.Record name map fields originOpt
        | _ -> Violation (["InvalidArgumentType"; (String.capitalize fnName)], "Cannot apply an unary function on an incompatible value.", originOpt)

    let evalUnary fns fnName argsEvaled originOpt world : Expr =
        match argsEvaled with
        | [|argEvaled|] -> evalUnaryInner fns fnName argEvaled originOpt world
        | _ -> Violation (["InvalidArgumentCount"; (String.capitalize fnName)], "Incorrect number of arguments for '" + fnName + "'; 1 argument required.", originOpt)
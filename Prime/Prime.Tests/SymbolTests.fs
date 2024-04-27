// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open System.Collections.Generic
open NUnit.Framework
open Prime

type IntIntRecord =
    { Int : int; Int2 : int }

type IntIntRecordAbstract =
    private { IntX_ : int; IntX2_ : int }
    member this.IntX = this.IntX_
    member this.IntX2 = this.IntX2_
    static member make x x2 = { IntX_ = x; IntX2_ = x2 }

type [<SymbolicExpansion>] IntIntRecordExpanded =
    { IntX : int; IntX2 : int }

type SimpleUnion =
    | SimpleUnion
    | SimpleUnion2

type ComplexUnion =
    | ComplexUnion of int
    | ComplexUnion2 of int * int

type ComplexUnionAbstract =
    private
        | ComplexUnionAbstract of int
        static member make x = ComplexUnionAbstract x

module SymbolTests =
    
    let [<Test>] canConvertStringToAtom () =
        let converter = SymbolicConverter (true, None, typeof<Symbol>)
        match converter.ConvertFromString "atom" :?> Symbol with
        | Atom (str, _) -> Assert.Equal ("atom", str)
        | _ -> Assert.True false
    
    let [<Test>] canConvertStringToNumber () =
        let converter = SymbolicConverter (true, None, typeof<Symbol>)
        match converter.ConvertFromString "0" :?> Symbol with
        | Number (str, _) -> Assert.Equal ("0", str)
        | _ -> Assert.True false
    
    let [<Test>] canConvertStringToNegativeNumber () =
        let converter = SymbolicConverter (true, None, typeof<Symbol>)
        match converter.ConvertFromString "-1" :?> Symbol with
        | Number (str, _) -> Assert.Equal ("-1", str)
        | _ -> Assert.True false
    
    let [<Test>] canConvertStringToString () =
        let converter = SymbolicConverter (true, None, typeof<Symbol>)
        match converter.ConvertFromString "\"str\"" :?> Symbol with
        | Text (str, _) -> Assert.Equal ("str", str)
        | _ -> Assert.True false

    let [<Test>] canConvertStringToInt () =
        let value = scvalue<int> "0"
        Assert.Equal (0, value)

    let [<Test>] canConvertStringToNone () =
        let value = scvalue<string option> "None"
        Assert.Equal (None, value)

    let [<Test>] canConvertStringToSomeString () =
        let value = scvalue<string option> "[Some string]"
        Assert.Equal (Some "string", value)

    let [<Test>] canConvertStringToRightString () =
        let value = scvalue<Either<unit, string>> "[Right string]"
        Assert.Equal (Right "string", value)

    let [<Test>] canConvertStringToIntList () =
        let value = scvalue<int list> "[0 1]"
        Assert.Equal ([0; 1], value)

    let [<Test>] canConvertStringToIntListList () =
        let value = scvalue<int list list> "[[]]"
        Assert.Equal ([[]], value)

    let [<Test>] canConvertStringToIntListListEmpty () =
        let value = scvalue<int list list> "[]"
        Assert.Equal ([], value)

    let [<Test>] canConvertStringToTuple () =
        let value = scvalue<int * int> "[0 1]"
        Assert.Equal ((0, 1), value)

    let [<Test>] canConvertStringToTupleTuple () =
        let value = scvalue<(int * int) * (int * int)> "[[0 1] [2 3]]"
        Assert.Equal (((0, 1), (2, 3)), value)

    let [<Test>] canConvertStringToRecord () =
        let value = scvalue<IntIntRecord> "[0 1]"
        Assert.Equal ({ Int = 0; Int2 = 1 }, value)

    let [<Test>] canConvertStringToRecordAbstract () =
        let value = scvalue<IntIntRecordAbstract> "[0 1]"
        Assert.Equal (IntIntRecordAbstract.make 0 1, value)

    let [<Test>] canConvertStringToExpandedRecord () =
        let value = scvalue<IntIntRecordExpanded> "[[IntX 0] [IntX2 1]]"
        Assert.Equal ({ IntX = 0; IntX2 = 1 }, value)

    let [<Test>] canConvertStringToSimpleUnion () =
        let value = scvalue<SimpleUnion> "SimpleUnion"
        Assert.Equal (SimpleUnion, value)

    let [<Test>] canConvertStringToComplexUnion () =
        let value = scvalue<ComplexUnion> "[ComplexUnion 0]"
        Assert.Equal (ComplexUnion 0, value)

    let [<Test>] canConvertStringToComplexUnionAbstract () =
        let value = scvalue<ComplexUnionAbstract> "[ComplexUnionAbstract 0]"
        Assert.Equal (ComplexUnionAbstract 0, value)

    let [<Test>] canConvertStringToComplexUnionTuple () =
        let value = scvalue<ComplexUnion * ComplexUnion> "[[ComplexUnion 0] [ComplexUnion2 1 2]]"
        // each tuple element must be tested individually as Assert.AreEqual doesn't seem to support tuple unions...
        Assert.Equal (ComplexUnion 0, fst value)
        Assert.Equal (ComplexUnion2 (1, 2), snd value)

    let [<Test>] canConvertStringToMapIntInt () =
        let value = scvalue<Map<int, int>> "[[0 1]]"
        Assert.Equal (1, Map.find 0 value)

    let [<Test>] canConvertStringToIntGenericList () =
        let value = scvalue<int List> "[0 1]"
        Assert.True (value.Contains 0)
        Assert.True (value.Contains 1)
        Assert.True (value.Count = 2)

    let [<Test>] canConvertStringToIntGenericStack () =
        let value = scvalue<int Stack> "[0 1]"
        Assert.True (value.Contains 0)
        Assert.True (value.Contains 1)
        Assert.True (value.Count = 2)

    let [<Test>] canConvertStringToIntGenericQueue () =
        let value = scvalue<int Queue> "[0 1]"
        Assert.True (value.Contains 0)
        Assert.True (value.Contains 1)
        Assert.True (value.Count = 2)

    let [<Test>] canConvertStringToIntGenericHashSet () =
        let value = scvalue<int HashSet> "[0 1]"
        Assert.True (value.Contains 0)
        Assert.True (value.Contains 1)
        Assert.True (value.Count = 2)

    let [<Test>] canConvertStringToGenericDictionaryIntInt () =
        let value = scvalue<Dictionary<int, int>> "[[0 1]]"
        Assert.Equal (1, value.[0])

    let [<Test>] canConvertFromRecordAbstractToString () =
        let value = scvalue<IntIntRecordAbstract> "[1 2]"
        Assert.Equal (IntIntRecordAbstract.make 1 2, value)

    let [<Test>] canConvertFromUnionAbstractToString () =
        let value = scvalue<ComplexUnionAbstract> "[ComplexUnionAbstract 1]"
        Assert.Equal (ComplexUnionAbstract.make 1, value)

    let [<Test>] canPrettyPrintGuid () =
        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Guid>).PrettyPrinter
        let symbolStr = "[5ec8734f-6a3d-4472-b86a-78125d238dc2]"
        let prettyStr = PrettyPrinter.prettyPrint symbolStr prettyPrinter
        Assert.Equal (symbolStr, prettyStr)
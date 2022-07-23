// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime.Tests
open System
open Xunit
open Prime

type [<StructuralEquality; StructuralComparison>] IntIntRecord =
    { Int : int; Int2 : int }

type [<StructuralEquality; StructuralComparison>] IntIntRecordAbstract =
    private { IntX_ : int; IntX2_ : int }
    member this.IntX = this.IntX_
    member this.IntX2 = this.IntX2_
    static member make x x2 = { IntX_ = x; IntX2_ = x2 }

type [<StructuralEquality; StructuralComparison; SymbolicExpansion>] IntIntRecordExpanded =
    { IntX : int; IntX2 : int }

type [<StructuralEquality; StructuralComparison>] SimpleUnion =
    | SimpleUnion
    | SimpleUnion2

type [<StructuralEquality; StructuralComparison>] ComplexUnion =
    | ComplexUnion of int
    | ComplexUnion2 of int * int

type [<StructuralEquality; StructuralComparison>] ComplexUnionAbstract =
    private
        | ComplexUnionAbstract of int
        | ComplexUnionAbstract2 of int * int
        static member make x = ComplexUnionAbstract x

module SymbolTests =
    
    let [<Fact>] canConvertStringToAtom () =
        let converter = SymbolicConverter (true, None, typeof<Symbol>)
        match converter.ConvertFromString "atom" :?> Symbol with
        | Atom (str, _) -> Assert.Equal<string> ("atom", str)
        | _ -> Assert.True false
    
    let [<Fact>] canConvertStringToNumber () =
        let converter = SymbolicConverter (true, None, typeof<Symbol>)
        match converter.ConvertFromString "0" :?> Symbol with
        | Number (str, _) -> Assert.Equal<string> ("0", str)
        | _ -> Assert.True false
    
    let [<Fact>] canConvertStringToNegativeNumber () =
        let converter = SymbolicConverter (true, None, typeof<Symbol>)
        match converter.ConvertFromString "-1" :?> Symbol with
        | Number (str, _) -> Assert.Equal<string> ("-1", str)
        | _ -> Assert.True false
    
    let [<Fact>] canConvertStringToString () =
        let converter = SymbolicConverter (true, None, typeof<Symbol>)
        match converter.ConvertFromString "\"str\"" :?> Symbol with
        | Text (str, _) -> Assert.Equal<string> ("str", str)
        | _ -> Assert.True false

    let [<Fact>] canConvertStringToInt () =
        let value = scvalue<int> "0"
        Assert.Equal (0, value)

    let [<Fact>] canConvertStringToNone () =
        let value = scvalue<string option> "None"
        Assert.Equal<string option> (None, value)

    let [<Fact>] canConvertStringToSomeString () =
        let value = scvalue<string option> "[Some string]"
        Assert.Equal<string option> (Some "string", value)

    let [<Fact>] canConvertStringToRightString () =
        let value = scvalue<Either<unit, string>> "[Right string]"
        Assert.Equal<Either<unit, string>> (Right "string", value)

    let [<Fact>] canConvertStringToIntList () =
        let value = scvalue<int list> "[0 1]"
        Assert.Equal<int list> ([0; 1], value)

    let [<Fact>] canConvertStringToIntListList () =
        let value = scvalue<int list list> "[[]]"
        Assert.Equal<int list list> ([[]], value)

    let [<Fact>] canConvertStringToIntListListEmpty () =
        let value = scvalue<int list list> "[]"
        Assert.Equal<int list list> ([], value)

    let [<Fact>] canConvertStringToTuple () =
        let value = scvalue<int * int> "[0 1]"
        Assert.Equal ((0, 1), value)

    let [<Fact>] canConvertStringToTupleTuple () =
        let value = scvalue<(int * int) * (int * int)> "[[0 1] [2 3]]"
        Assert.Equal (((0, 1), (2, 3)), value)

    let [<Fact>] canConvertStringToRecord () =
        let value = scvalue<IntIntRecord> "[0 1]"
        Assert.Equal ({ Int = 0; Int2 = 1 }, value)

    let [<Fact>] canConvertStringToRecordAbstract () =
        let value = scvalue<IntIntRecordAbstract> "[0 1]"
        Assert.Equal (IntIntRecordAbstract.make 0 1, value)

    let [<Fact>] canConvertStringToExpandedRecord () =
        let value = scvalue<IntIntRecordExpanded> "[[IntX 0] [IntX2 1]]"
        Assert.Equal ({ IntX = 0; IntX2 = 1 }, value)

    let [<Fact>] canConvertStringToSimpleUnion () =
        let value = scvalue<SimpleUnion> "SimpleUnion"
        Assert.Equal (SimpleUnion, value)

    let [<Fact>] canConvertStringToComplexUnion () =
        let value = scvalue<ComplexUnion> "[ComplexUnion 0]"
        Assert.Equal (ComplexUnion 0, value)

    let [<Fact>] canConvertStringToComplexUnionAbstract () =
        let value = scvalue<ComplexUnionAbstract> "[ComplexUnionAbstract 0]"
        Assert.Equal (ComplexUnionAbstract 0, value)

    let [<Fact>] canConvertStringToComplexUnionTuple () =
        let value = scvalue<ComplexUnion * ComplexUnion> "[[ComplexUnion 0] [ComplexUnion2 1 2]]"
        // each tuple element must be tested individually as Assert.Equal doesn't seem to support tuple unions...
        Assert.Equal (ComplexUnion 0, fst value)
        Assert.Equal (ComplexUnion2 (1, 2), snd value)

    let [<Fact>] canConvertStringToMapIntInt () =
        let value = scvalue<Map<int, int>> "[[0 1]]"
        Assert.Equal (1, Map.find 0 value)

    let [<Fact>] canConvertFromRecordAbstractToString () =
        let value = scvalue<IntIntRecordAbstract> "[1 2]"
        Assert.Equal (IntIntRecordAbstract.make 1 2, value)

    let [<Fact>] canConvertFromUnionAbstractToString () =
        let value = scvalue<ComplexUnionAbstract> "[ComplexUnionAbstract 1]"
        Assert.Equal (ComplexUnionAbstract.make 1, value)

    let [<Fact>] canPrettyPrintGuid () =
        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Guid>).PrettyPrinter
        let symbolStr = "[5ec8734f-6a3d-4472-b86a-78125d238dc2]"
        let prettyStr = PrettyPrinter.prettyPrint symbolStr prettyPrinter
        Assert.Equal (symbolStr, prettyStr)
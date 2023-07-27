// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open NUnit.Framework
open Prime
module ScriptingTests =

    type [<ReferenceEquality>] TestWorld =
        { ScriptingEnv : Scripting.Env }
        interface TestWorld ScriptingSystem with
            member this.GetEnv () = this.ScriptingEnv
            member this.TryGetExtrinsic _ = None
            member this.TryImport _ _ = failwithnie ()
            member this.TryExport _ _ = failwithnie ()
        static member make () = { ScriptingEnv = Scripting.Env.make () }

    let evalPartial exprStr =
        let world = TestWorld.make ()
        match ScriptingSystem.tryEvalScript id Constants.Scripting.PreludeFilePath world with
        | Right (_, _, world) ->
            let expr = scvalue<Scripting.Expr> exprStr
            ScriptingSystem.eval expr world |> fst'
        | Left _ ->
            Assert.True false
            Scripting.Unit

    let eval exprStr =
        let converter = SymbolicConverter (true, None, typeof<Scripting.Expr>)
        let evaled = evalPartial exprStr
        let evaledSymbol = converter.ConvertTo (evaled, typeof<Symbol>) :?> Symbol
        Symbol.toString evaledSymbol

    let [<Test>] keywordsWork () = Assert.Equal ("Keyword", eval "Keyword")
    let [<Test>] plusWorks () = Assert.Equal ("2", eval "[+ 1 1]")
    let [<Test>] equalityWorks () = Assert.Equal ("true", eval "[= 1 1]")
    let [<Test>] nestedApplicationWorks () = Assert.Equal ("4", eval "[+ [+ 1 1] [+ 1 1]]")
    let [<Test>] optionsWork () = Assert.Equal ("true", eval "[isSome [some 1]]")
    let [<Test>] tuplesWork () = Assert.Equal ("1", eval "[fst [tuple 1]]")
    let [<Test>] listsWork () = Assert.Equal ("1", eval "[head [list 1]]")
    let [<Test>] unionsWork () = Assert.Equal ("1", eval "[fst [U 1]]")
    let [<Test>] recordsWork () = Assert.Equal ("1", eval "[fst [record R [F 1]]]")
    let [<Test>] conditionalWorks () = Assert.Equal ("1", eval "[if [= 1 1] 1 0]")
    let [<Test>] matchWorks () = Assert.Equal ("2", eval "[match 1 [0 0] [1 2]]")
    let [<Test>] selectWorks () = Assert.Equal ("1", eval "[select [false 0] [true 1]]")
    let [<Test>] letWorks () = Assert.Equal ("2", eval "[let [x 1] [+ x x]]")
    let [<Test>] letManyWorks () = Assert.Equal ("3", eval "[let [x 1] [y 2] [+ x y]]")
    let [<Test>] letFxWorks () = Assert.Equal ("1", eval "[let [f [x] x] [f 1]]")
    let [<Test>] letFunWorks () = Assert.Equal ("1", eval "[let [f [fun [x] x]] [f 1]]")
    let [<Test>] doWorks () = Assert.Equal ("4", eval "[do [+ 1 1] [+ 2 2]]")
    let [<Test>] infoWorks () = Assert.Equal ("\"[fun [a b c] ...]\"", eval "[info [fun [a b c] a]]")
    let [<Test>] infoTrinsicWorks () = Assert.Equal ("\"[fun [a b] 'Determine equality.']\"", eval "[info =]")

    let [<Test>] matchFailureWorks () =
        match evalPartial "[match 2 [0 0] [1 2]]" with
        | Scripting.Violation (_, _, _) -> Assert.True true
        | _ -> Assert.True false

    let [<Test>] selectFailureWorks () =
        match evalPartial "[select [false 0] [false 1]]" with
        | Scripting.Violation (_, _, _) -> Assert.True true
        | _ -> Assert.True false

    let [<Test>] outOfRangeWorks () =
        match evalPartial "[fst coempty]" with
        | Scripting.Violation _ -> Assert.True true
        | _ -> Assert.True false

    let [<Test>] unboundBindingWorks () =
        match evalPartial "unbound" with
        | Scripting.Violation _ -> Assert.True true
        | _ -> Assert.True false

    let [<Test>] unboundFunctionCallWorks () =
        match evalPartial "[unbound 0]" with
        | Scripting.Violation _ -> Assert.True true
        | _ -> Assert.True false
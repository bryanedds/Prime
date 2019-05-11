// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open System.Collections.Generic
open System.Diagnostics
open Prime
module Program =

    type [<ReferenceEquality>] private ScriptingWorld =
        { ScriptingEnv : Scripting.Env }
        interface ScriptingWorld ScriptingSystem with
            member this.GetEnv () = this.ScriptingEnv
            member this.TryGetExtrinsic _ = FOption.none ()
            member this.TryImport _ _ = failwithnie ()
            member this.TryExport _ _ = failwithnie ()
        static member make () = { ScriptingEnv = Scripting.Env.Env.make () }
        
    let rec private runRepl world =
        while Console.KeyAvailable do ignore (Console.ReadKey true)
        Console.Write "> "
        match Console.ReadLine () with
        | input when String.IsNullOrWhiteSpace input ->
            runRepl world
        | input ->
            let expr = scvalue<Scripting.Expr> input
            try let struct (result, world) = ScriptingSystem.eval expr world
                Console.Write "< "
                Console.WriteLine (scstring result)
                runRepl world
            with exn ->
                Console.WriteLine ("Unexpected exception:\n" + scstring exn)
                runRepl world

    let [<EntryPoint; STAThread>] main _ =
        let world = ScriptingWorld.make ()
        match ScriptingSystem.tryEvalScript id ("./Prelude.amsl") world with
        | Left (err, _) -> Console.WriteLine ("Failed to evaluate prelude due to: " + err)
        | Right (_, _, world) ->
            Console.WriteLine "Welcome to the Amsl Repl!"
            Console.WriteLine "Try out a symbolic expression like [+ 2 2]"
            runRepl world
        0

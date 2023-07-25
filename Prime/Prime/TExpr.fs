// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System

/// Determines whether a data structure has functional (immutable) or imperative (mutable) semantics.
type [<StructuralEquality; StructuralComparison>] TConfig =
    | Functional
    | Imperative

    /// Check that functional (immutable) semantics are used.
    static member inline isFunctional config =
        match config with
        | Functional -> true
        | Imperative -> false

    /// Check that imperative (mutable) semantics are used.
    static member inline isImperative config =
        not (TConfig.isFunctional config)

/// TExpr as a reader monad.
type TExpr<'a, 'env> =
    'env -> struct ('a * 'env)

/// Builds TExprs.
type TExprBuilder<'env> () =

    /// Monadic bind.
    member this.Bind (expr : TExpr<'a, 'env>, lift : 'a -> TExpr<'b, 'env>) : TExpr<'b, 'env> =
        fun env ->
            let struct (result, env') = expr env
            let expr' = lift result
            expr' env'

    /// Monadic return.
    member this.Return (value : 'a) : TExpr<'a, 'env> =
        fun expr ->
            struct (value, expr)

    /// Monadic return from.
    member this.ReturnFrom (value : 'a) =
        value

    /// Monoidal zero.
    member this.Zero () =
        this.Return ()
        
    /// Sequence two TExprs.
    member this.Combine (l, r) =
        this.Bind (l, fun () -> r)
        
    /// TryWith for TExpr.
    member this.TryWith (body : TExpr<'a, 'expr>, handler : exn -> TExpr<'a, 'expr>) : TExpr<'a, 'expr> =
        fun env ->
            try body env
            with exn -> handler exn env

    /// TryFinally for TExpr.
    member this.TryFinally (body : TExpr<'a, 'expr>, compensation) : TExpr<'a,'expr> =
        fun env ->
            try body env
            finally compensation ()

    /// Using for TExpr.
    member this.Using (res : #IDisposable, body) =
        this.TryFinally (body res, fun () ->
            match res with null -> () | disp -> disp.Dispose ())

    /// Delay for TExpr.
    member this.Delay f =
        this.Bind (this.Return (), f)

    /// While for TExpr.
    member this.While (guard, body) =
        if not (guard ())
        then this.Zero ()
        else this.Bind (body, fun () -> this.While (guard, body))

    /// For for TExpr.
    member this.For (seq : _ seq, body) =
        this.Using (seq.GetEnumerator (), fun enr ->
            this.While (enr.MoveNext, this.Delay (fun () ->
                body enr.Current)))
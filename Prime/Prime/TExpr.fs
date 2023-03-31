﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System

type [<StructuralEquality; StructuralComparison>] TConfig =
    | Functional
    | Imperative

[<RequireQualifiedAccess>]
module TConfig =

    let inline isFunctional config =
        match config with
        | Functional -> true
        | Imperative -> false

type TExpr<'a, 'env> =
    'env -> struct ('a * 'env)

type TExprBuilder<'env> () =

    member this.Bind (expr : TExpr<'a, 'env>, lift : 'a -> TExpr<'b, 'env>) : TExpr<'b, 'env> =
        fun env ->
            let struct (result, env') = expr env
            let expr' = lift result
            expr' env'

    member this.Return (value : 'a) : TExpr<'a, 'env> =
        fun expr ->
            struct (value, expr)

    member this.ReturnFrom (value : 'a) =
        value

    member this.Zero () =
        this.Return ()
        
    member this.Combine (l, r) =
        this.Bind (l, fun () -> r)
        
    member this.TryWith (body : TExpr<'a, 'expr>, handler : exn -> TExpr<'a, 'expr>) : TExpr<'a, 'expr> =
        fun env ->
            try body env
            with exn -> handler exn env

    member this.TryFinally (body : TExpr<'a, 'expr>, compensation) : TExpr<'a,'expr> =
        fun env ->
            try body env
            finally compensation ()

    member this.Using (res : #IDisposable, body) =
        this.TryFinally (body res, fun () ->
            match res with null -> () | disp -> disp.Dispose ())

    member this.Delay f =
        this.Bind (this.Return (), f)

    member this.While (guard, body) =
        if not (guard ())
        then this.Zero ()
        else this.Bind (body, fun () -> this.While (guard, body))

    member this.For (seq : _ seq, body) =
        this.Using (seq.GetEnumerator (), fun enr ->
            this.While (enr.MoveNext, this.Delay (fun () ->
                body enr.Current)))
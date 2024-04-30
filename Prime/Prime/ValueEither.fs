// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System

/// Haskell-style Either type with value semantics.
type [<Struct>] ValueEither<'l, 'r> =
    | ValueRight of Right : 'r
    | ValueLeft of Left : 'l

/// Builds an either monad.
type ValueEitherBuilder () =
    member this.Return a = ValueRight a
    member this.ReturnFrom a = a
    member this.Bind (a, f) = match a with ValueRight r -> f r | ValueLeft l -> ValueLeft l
    member this.Using (d, b) = use u = d in b u
    member this.TryWith (b, h) = try b () with exn -> h exn
    member this.TryFinally (b, h) = try b () finally h ()
    member this.Delay f = f ()
    member this.Run f = f ()
    member this.Zero () = ValueRight ()
    member this.Yield a = ValueRight a
    member this.YieldFrom e = e
    member this.Combine (a, b) = this.Bind (a, b)

    member this.While (g, b) =
        if g ()
        then match b () with ValueRight () -> this.While (g, b) | error -> error
        else this.Zero ()

    member this.For (sequence : _ seq, body) =
        use enr = sequence.GetEnumerator ()
        let mutable errorOpt = ValueNone
        while enr.MoveNext () && ValueOption.isNone errorOpt do
            match body enr.Current with
            | ValueRight () -> ()
            | left -> errorOpt <- ValueSome left
        match errorOpt with
        | ValueSome error -> error
        | ValueNone -> this.Zero ()

[<AutoOpen>]
module ValueEitherBuilder =

    /// Builds value eithers.
    let valueEither = ValueEitherBuilder ()

[<RequireQualifiedAccess>]
module ValueEither =

    /// Monadic return for ValueEither.
    let inline returnM a = valueEither.Return a

    /// Monadic 'return from' for ValueEither.
    let inline returnFrom a = valueEither.ReturnFrom a

    /// Monadic bind for ValueEither.
    let inline bind a f = valueEither.Bind (a, f)

    /// Check whether a ValueEither is ValueLeft.
    let isLeft eir =
        match eir with
        | ValueRight _ -> false
        | ValueLeft _ -> true
    
    /// Check whether a ValueEither is ValueRight.
    let isRight eir =
        match eir with
        | ValueRight _ -> true
        | ValueLeft _ -> false

    /// Get the ValueLeft of a ValueEither, failing if not available.
    let getLeft eir =
        match eir with
        | ValueRight _ -> failwith "Could not get ValueLeft value from a ValueRight value."
        | ValueLeft l -> l

    /// Get the ValueRight of a ValueEither, failing if not available.
    let getRight eir =
        match eir with
        | ValueRight r -> r
        | ValueLeft _ -> failwith "Could not get ValueRight value from a ValueLeft value."

    /// Get only the ValueLefts of a sequence of a ValueEithers.
    let getLefts eirs =
        List.foldBack
            (fun eir lefts -> match eir with ValueRight _ -> lefts | ValueLeft left -> left :: lefts)
            (List.ofSeq eirs)
            []

    /// Get only the ValueRights of a sequence of ValueEithers.
    let getRights eirs =
        List.foldBack
            (fun eir rights -> match eir with ValueRight right -> right :: rights | ValueLeft _ -> rights)
            (List.ofSeq eirs)
            []

    /// Map over the left side of a ValueEither.
    let mapLeft mapper eir =
        match eir with
        | ValueRight r -> ValueRight r
        | ValueLeft l -> ValueLeft (mapper l)

    /// Map over the right side of a ValueEither.
    let mapRight mapper eir =
        match eir with
        | ValueRight r -> ValueRight (mapper r)
        | ValueLeft l -> ValueLeft l

    /// Map both sides of a ValueEither.
    let map fnl fnr eir =
        eir |>
        mapLeft fnl |>
        mapRight fnr

    /// Split a sequence of ValueEithers into a pair of left and right lists.
    let split eirs =
        List.foldBack
            (fun eir (ls, rs) ->
                match eir with
                | ValueRight r -> (ls, r :: rs)
                | ValueLeft l -> (l :: ls, rs))
            (List.ofSeq eirs)
            ([], [])

    /// Pick whichever of the ValueEithers exists so long as they are the same type.
    let amb (eir : ValueEither<'a, 'a>) =
        match eir with
        | ValueRight value -> value
        | ValueLeft value -> value

    /// Pick whichever of the ValueEithers exists.
    let ambBy pickFst pickSnd (eir : ValueEither<'a, 'b>) =
        match eir with
        | ValueRight value -> pickFst value
        | ValueLeft value -> pickSnd value
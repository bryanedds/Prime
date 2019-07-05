// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open Prime

/// Arrows defined in terms of streams.
type [<NoEquality; NoComparison>]
    Arrow<'a, 'b, 'w when 'w :> EventSystem<'w>> =
    Arrow of (Stream<'a, 'w> -> Stream<'b, 'w>) with

    static member rrr () : Arrow<'a, 'b, 'w> -> Arrow<'b, 'c, 'w> -> Arrow<'a, 'c, 'w> =
        function Arrow arrow ->
                 function Arrow arrow2 ->
                          Arrow (fun stream -> arrow2 (arrow stream))


    static member sss () : Arrow<'a, 'b, 'w> -> Arrow<'a2, 'b2, 'w> -> Arrow<'a * 'a2, 'b * 'b2, 'w> =
        function Arrow arrow ->
                 function Arrow arrow2 ->
                          Arrow (fun stream ->
                            let streamB = arrow (Stream.first stream)
                            let streamB2 = arrow2 (Stream.second stream)
                            Stream.product streamB streamB2)

    static member aaa () : Arrow<'a, 'b, 'w> -> Arrow<'a, 'b2, 'w> -> Arrow<'a, 'b * 'b2, 'w> =
        function Arrow arrow ->
                 function Arrow arrow2 ->
                          Arrow (fun stream ->
                            let streamB = arrow stream
                            let streamB2 = arrow2 stream
                            Stream.product streamB streamB2)

    static member ppp () : Arrow<'a, 'b, 'w> -> Arrow<'a2, 'b2, 'w> -> Arrow<Either<'a, 'a2>, Either<'b, 'b2>, 'w> =
        function (Arrow arrow : Arrow<'a, 'b, 'w>) ->
                 function (Arrow arrow2 : Arrow<'a2, 'b2, 'w>) ->
                           Arrow (fun (stream : Stream<Either<'a, 'a2>, 'w>) ->
                            let streamB = arrow (Stream.filterLeft stream)
                            let streamB2 = arrow2 (Stream.filterRight stream)
                            Stream.sum streamB streamB2)

    static member ooo () : Arrow<'a, 'c, 'w> -> Arrow<'b, 'c, 'w> -> Arrow<Either<'a, 'b>, 'c, 'w> =
        function (Arrow arrow : Arrow<'a, 'c, 'w>) ->
                 function (Arrow arrow2 : Arrow<'b, 'c, 'w>) ->
                           Arrow (fun (stream : Stream<Either<'a, 'b>, 'w>) ->
                            let streamL = arrow (Stream.filterLeft stream)
                            let streamR = arrow2 (Stream.filterRight stream)
                            Stream.append streamL streamR)

    static member inline ( >>> ) (a, b) = Arrow.rrr () a b
    static member inline ( *** ) (a, b) = Arrow.sss () a b
    static member inline ( &&& ) (a, b) = Arrow.aaa () a b
    static member inline ( +++ ) (a, b) = Arrow.ppp () a b
    static member inline ( ||| ) (a, b) = Arrow.ooo () a b

[<RequireQualifiedAccess>]
module Arrow =

    let returnA : Arrow<'a, 'a, 'w> =
        Arrow (fun stream -> stream)

    let arr (f : 'a -> 'b) : Arrow<'a, 'b, 'w> =
        Arrow (function stream -> Stream.map f stream)


    let first : Arrow<'a, 'b, 'w> -> Arrow<'a * 'c, 'b * 'c, 'w> =
        function Arrow arrow ->
                 Arrow (fun (stream : Stream<'a * 'c, 'w>) ->
                    let (stream2 : Stream<'b, 'w>) = arrow (Stream.first stream)
                    Stream.product stream2 (Stream.second stream))

    let second : Arrow<'a, 'b, 'w> -> Arrow<'c * 'a, 'c * 'b, 'w> =
        function Arrow arrow ->
                 Arrow (fun (stream : Stream<'c * 'a, 'w>) ->
                    let (stream2 : Stream<'c, 'w>) = (Stream.first stream)
                    Stream.product stream2 (arrow (Stream.second stream)))
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open Prime

/// Arrows defined in terms of streams.
type [<NoEquality; NoComparison>]
    Arrow<'a, 'b, 'w when 'w :> EventSystem<'w>> =
    Arrow of (Stream<'a, 'w> -> Stream<'b, 'w>)

[<RequireQualifiedAccess>]
module Arrow =

    let returnA : Arrow<'a, 'a, 'w> =
        Arrow (fun stream -> stream)

    let arr (f : 'a -> 'b) : Arrow<'a, 'b, 'w> =
        Arrow (function stream -> Stream.map f stream)

    let (>>>>) : Arrow<'a, 'b, 'w> -> Arrow<'b, 'c, 'w> -> Arrow<'a, 'c, 'w> =
        function Arrow arrow ->
                 function Arrow arrow2 ->
                          Arrow (fun stream -> arrow2 (arrow stream))

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

    let ( **** ) : Arrow<'a, 'b, 'w> -> Arrow<'a2, 'b2, 'w> -> Arrow<'a * 'a2, 'b * 'b2, 'w> =
        function Arrow arrow ->
                 function Arrow arrow2 ->
                          Arrow (fun stream ->
                            let streamB = arrow (Stream.first stream)
                            let streamB2 = arrow2 (Stream.second stream)
                            Stream.product streamB streamB2)

    let (&&&&) : Arrow<'a, 'b, 'w> -> Arrow<'a, 'b2, 'w> -> Arrow<'a, 'b * 'b2, 'w> =
        function Arrow arrow ->
                 function Arrow arrow2 ->
                          Arrow (fun stream ->
                            let streamB = arrow stream
                            let streamB2 = arrow2 stream
                            Stream.product streamB streamB2)

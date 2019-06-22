// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open Prime

type [<NoEquality; NoComparison>] Binding<'m, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>> =
    { Stream : Stream<obj, 'w>
      MakeValueOpt : Event<obj, 's> -> 'm option }

type [<NoEquality; NoComparison>] Binding<'m, 'e, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>> =
    | Message of Binding<'m, 's, 'w>
    | Command of Binding<'e, 's, 'w>

[<RequireQualifiedAccess>]
module Binding =

    let make<'a, 'v, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>>
        (stream : Stream<'a, 'w>) (makeValueOpt : Event<'a, 's> -> 'v option) =
        { Stream = Stream.generalize stream
          MakeValueOpt = fun evt -> makeValueOpt (Event.specialize evt) }

    let makeSimple<'a, 'v, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>>
        (stream : Stream<'a, 'w>) (value : 'v) =
        { Stream = Stream.generalize stream
          MakeValueOpt = fun (_ : Event<obj, 's>) -> Some value }

type Binding<'m, 'e, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>> with

    static member (=>) (_ : Binding<'m, 'e, 's, 'w>, source : Address<'a>) =
        fun (message : 'm) ->
            Message (Binding.makeSimple (Stream.make source) message)

    static member (=>) (_ : Binding<'m, 'e, 's, 'w>, source : Stream<'a, 'w>) =
        fun (message : 'm) ->
            Message (Binding.makeSimple source message)

    static member (=|>) (_ : Binding<'m, 'e, 's, 'w>, source : Address<'a>) =
        fun (message : Event<'a, 's> -> 'm option) ->
            Message (Binding.make (Stream.make source) message)

    static member (=|>) (_ : Binding<'m, 'e, 's, 'w>, source : Stream<'a, 'w>) =
        fun (message : Event<'a, 's> -> 'm option) ->
            Message (Binding.make source message)

    static member (=>!) (_ : Binding<'m, 'e, 's, 'w>, source : Address<'a>) =
        fun (command : 'e) ->
            Command (Binding.makeSimple (Stream.make source) command)

    static member (=>!) (_ : Binding<'m, 'e, 's, 'w>, source : Stream<'a, 'w>) =
        fun (command : 'e) ->
            Command (Binding.makeSimple source command)

    static member (=|>!) (_ : Binding<'m, 'e, 's, 'w>, source : Address<'a>) =
        fun (command : Event<'a, 's> -> 'e option) ->
            Command (Binding.make (Stream.make source) command)

    static member (=|>!) (_ : Binding<'m, 'e, 's, 'w>, source : Stream<'a, 'w>) =
        fun (command : Event<'a, 's> -> 'e option) ->
            Command (Binding.make source command)

[<AutoOpen>]
module BindingOperators =

    let inline (=>) source message : Binding<'m, 'e, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'e, 's, 'w>> => source) message

    let inline (=|>) source message : Binding<'m, 'e, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'e, 's, 'w>> =|> source) message

    let inline (=>!) source message : Binding<'m, 'e, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'e, 's, 'w>> =>! source) message

    let inline (=|>!) source message : Binding<'m, 'e, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'e, 's, 'w>> =|>! source) message
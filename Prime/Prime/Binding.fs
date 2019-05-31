// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open Prime

type [<NoEquality; NoComparison>] Binding<'m, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>> =
    { Stream : Stream<obj, 'w>
      MakeMessage : Event<obj, 's> -> 'm option }

type [<NoEquality; NoComparison>] Binding<'m, 'e, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>> =
    | Message of Binding<'m, 's, 'w>
    | Effect of Binding<'e, 's, 'w>

[<RequireQualifiedAccess>]
module Binding =

    let make<'a, 'm, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>>
        (stream : Stream<obj, 'w>) (message : 'm) =
        { Stream = stream
          MakeMessage = fun (_ : Event<obj, 's>) -> Some message }

    let makeSimple<'a, 's, 'm, 'w when 's :> Participant and 'w :> EventSystem<'w>>
        (stream : Stream<obj, 'w>) (makeMessage : Event<obj, 's> -> 'm option) =
        { Stream = stream
          MakeMessage = makeMessage }

[<AutoOpen>]
module BindingOperators =

    let (==>) address makeMessageOpt =
        Message (Binding.makeSimple (address |> Stream.make |> Stream.generalize) makeMessageOpt)

    let (=|>) address message =
        Message (Binding.make (address |> Stream.make |> Stream.generalize) message)

    let (==>!) address makeMessageOpt =
        Effect (Binding.makeSimple (address |> Stream.make |> Stream.generalize) makeMessageOpt)

    let (=|>!) address message =
        Effect (Binding.make (address |> Stream.make |> Stream.generalize) message)

    let (===>) stream makeMessageOpt =
        Message (Binding.makeSimple stream makeMessageOpt)

    let (==|>) stream message =
        Message (Binding.make stream message)

    let (===>!) stream makeMessageOpt =
        Effect (Binding.makeSimple stream makeMessageOpt)

    let (==|>!) stream message =
        Effect (Binding.make stream message)
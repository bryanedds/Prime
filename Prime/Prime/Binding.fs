// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open Prime

type [<NoEquality; NoComparison>] BindingValue<'a, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>> =
    { Stream : Stream<obj, 'w>
      MakeValue : Event<obj, 's> -> 'a }

type [<NoEquality; NoComparison>] Binding<'m, 'c, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>> =
    | MessageBinding of BindingValue<'m, 's, 'w>
    | CommandBinding of BindingValue<'c, 's, 'w>

[<RequireQualifiedAccess>]
module Binding =

    let make<'a, 'v, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>>
        (stream : Stream<'a, 'w>) (makeValue : Event<'a, 's> -> 'v) =
        { Stream = Stream.generalize stream
          MakeValue = fun evt -> makeValue (Event.specialize evt) }

    let makeSimple<'a, 'v, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>>
        (stream : Stream<'a, 'w>) (value : 'v) =
        { Stream = Stream.generalize stream
          MakeValue = fun (_ : Event<obj, 's>) -> value }

type Binding<'m, 'c, 's, 'w when 's :> Participant and 'w :> EventSystem<'w>> with

    static member (=>) (_ : Binding<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (message : 'm) ->
            MessageBinding (Binding.makeSimple (Stream.make source) message)

    static member (=>) (_ : Binding<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (message : 'm) ->
            MessageBinding (Binding.makeSimple source message)

    static member (=|>) (_ : Binding<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (message : Event<'a, 's> -> 'm) ->
            MessageBinding (Binding.make (Stream.make source) message)

    static member (=|>) (_ : Binding<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (message : Event<'a, 's> -> 'm) ->
            MessageBinding (Binding.make source message)

    static member (=>!) (_ : Binding<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (command : 'c) ->
            CommandBinding (Binding.makeSimple (Stream.make source) command)

    static member (=>!) (_ : Binding<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (command : 'c) ->
            CommandBinding (Binding.makeSimple source command)

    static member (=|>!) (_ : Binding<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (command : Event<'a, 's> -> 'c) ->
            CommandBinding (Binding.make (Stream.make source) command)

    static member (=|>!) (_ : Binding<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (command : Event<'a, 's> -> 'c) ->
            CommandBinding (Binding.make source command)

[<AutoOpen>]
module BindingOperators =

    let inline (=>) source message : Binding<'m, 'c, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'c, 's, 'w>> => source) message

    let inline (=|>) source message : Binding<'m, 'c, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'c, 's, 'w>> =|> source) message

    let inline (=>!) source message : Binding<'m, 'c, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'c, 's, 'w>> =>! source) message

    let inline (=|>!) source message : Binding<'m, 'c, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'c, 's, 'w>> =|>! source) message

/// A model-message-command signal.
type Signal<'message, 'command> =
    | Message of message : 'message
    | Command of command : 'command
    | Signals of signals : Signal<'message, 'command> list
    
    static member add (left : Signal<'message, 'command>) (right : Signal<'message, 'command>) = Signals [left; right]
    static member (+) (left, right) = Signal.add left right

[<RequireQualifiedAccess>]
module Signal =

    let add left right = Signal<_, _>.add left right
    let msg message = Message message
    let msgs messages = Signals (List.map Message messages)
    let cmd command = Command command
    let cmds commands = Signals (List.map Command commands)
    let many signals = Signals signals
    let none = Signals []

    let rec private processModelInternal<'model, 'message, 'command, 'p, 'w when 'p :> Participant>
        processMessage
        processCommand
        (model : Lens<'model, 'w>)
        (participant : 'p)
        (next : Signal<'message, 'command>)
        (world : 'w) =
        match next with
        | Message message ->
            let (modelValue, next) = processMessage (message, model.Get world, participant, world)
            let world = model.Set modelValue world
            processModelInternal processMessage processCommand model participant next world
        | Command command ->
            let (world, next) = processCommand (command, model.Get world, participant, world)
            processModelInternal processMessage processCommand model participant next world
        | Signals nexts ->
            List.fold (flip (processModelInternal processMessage processCommand model participant)) world nexts

    let processModel processMessage processCommand model participant bindings world =
        List.fold (fun world binding ->
            match binding with
            | MessageBinding binding ->
                Stream.monitor (fun evt world ->
                    let next = msg (binding.MakeValue evt)
                    processModelInternal processMessage processCommand model participant next world)
                    participant binding.Stream world
            | CommandBinding binding ->
                Stream.monitor (fun evt world ->
                    let next = cmd (binding.MakeValue evt)
                    processModelInternal processMessage processCommand model participant next world)
                    participant binding.Stream world)
            world bindings

[<AutoOpen>]
module SignalOperators =

    let withMsg value message = (value, Message message)
    let withMsgs value messages = (value, Signal.msgs messages)
    let withCmd value command = (value, Command command)
    let withCmds value commands = (value, Signal.cmds commands)
    let withSig value (signal : Signal<_, _>) = (value, signal)
    let withSigs value signals = (value, Signals signals)
    let just value = (value, Signal.none)
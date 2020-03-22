// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open Prime

/// A model-message-command signal.
type Signal<'message, 'command> =
    | Message of message : 'message
    | Command of command : 'command
    | Signals of signals : Signal<'message, 'command> list
    
    static member add (left : Signal<'message, 'command>) (right : Signal<'message, 'command>) = Signals [left; right]
    static member (+) (left, right) = Signal.add left right

type [<NoEquality; NoComparison>] Binding<'m, 'c, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>> =
    { Stream : Stream<obj, 'w>
      MakeValue : Event<obj, 's> -> Signal<'m, 'c> }

type Binding<'m, 'c, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>> with

    static member (=>) (_ : Binding<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (signal : Signal<'m, 'c>) ->
            { Stream = source |> Stream.make |> Stream.generalize
              MakeValue = fun (_ : Event<obj, 's>) -> signal }

    static member (=>) (_ : Binding<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (signal : Signal<'m, 'c>) ->
            { Stream = source |> Stream.generalize
              MakeValue = fun (_ : Event<obj, 's>) -> signal }

    static member (=|>) (_ : Binding<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (handler : Event<'a, 's> -> Signal<'m, 'c>) ->
            { Stream = source |> Stream.make |> Stream.generalize
              MakeValue = fun evt -> Event.generalize evt |> Event.specialize |> handler }

    static member (=|>) (_ : Binding<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (handler : Event<'a, 's> -> Signal<'m, 'c>) ->
            { Stream = source |> Stream.generalize
              MakeValue = fun evt -> Event.generalize evt |> Event.specialize |> handler }

[<AutoOpen>]
module BindingOperators =

    let inline (=>) source signal : Binding<'m, 'c, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'c, 's, 'w>> => source) signal

    let inline (=|>) source signal : Binding<'m, 'c, 's, 'w> =
        (Unchecked.defaultof<Binding<'m, 'c, 's, 'w>> =|> source) signal

    let inline bind source signal : Binding<'m, 'c, 's, 'w> =
        source => signal

    let inline react source signal : Binding<'m, 'c, 's, 'w> =
        source =|> signal

[<AutoOpen>]
module SignalOperators =

    let msg message = Message message
    let msgs messages = Signals (List.map Message messages)
    let cmd command = Command command
    let cmds commands = Signals (List.map Command commands)
    let withMsg value message = (value, Message message)
    let withMsgs value messages = (value, msgs messages)
    let withCmd value command = (value, Command command)
    let withCmds value commands = (value, cmds commands)
    let withSig value (signal : Signal<_, _>) = (value, signal)
    let withSigs value signals = (value, Signals signals)
    let just value = (value, Signals [])

[<RequireQualifiedAccess>]
module Signal =

    let add left right = Signal<_, _>.add left right
    let many signals = Signals signals
    let none = Signals []

    let rec processSignal<'model, 'message, 'command, 's, 'w when 's :> Simulant>
        processMessage
        processCommand
        (model : Lens<'model, 'w>)
        (signal : Signal<'message, 'command>)
        (simulant : 's)
        (world : 'w) =
        match signal with
        | Message message ->
            let (modelValue, signal) = processMessage (model.Get world, message, simulant, world)
            let world = model.Set modelValue world
            processSignal processMessage processCommand model signal simulant world
        | Command command ->
            let (world, signal) = processCommand (model.Get world, command, simulant, world)
            processSignal processMessage processCommand model signal simulant world
        | Signals signals ->
            List.fold
                (fun world signal -> processSignal processMessage processCommand model signal simulant world)
                world signals

    let processBindings processMessage processCommand model bindings simulant world =
        List.fold (fun world binding ->
            Stream.monitor (fun evt world ->
                let signal = binding.MakeValue evt
                processSignal processMessage processCommand model signal simulant world)
                simulant binding.Stream world)
            world bindings
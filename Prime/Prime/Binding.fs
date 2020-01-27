// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

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
        (signal : Signal<'message, 'command>)
        processMessage
        processCommand
        (model : Lens<'model, 'w>)
        (simulant : 's)
        (world : 'w) =
        match signal with
        | Message message ->
            let (modelValue, signal) = processMessage (message, model.Get world, simulant, world)
            let world = model.Set modelValue world
            processSignal signal processMessage processCommand model simulant world
        | Command command ->
            let (world, signal) = processCommand (command, model.Get world, simulant, world)
            processSignal signal processMessage processCommand model simulant world
        | Signals signals ->
            List.fold
                (fun world signal -> processSignal signal processMessage processCommand model simulant world)
                world signals

    let processBindings bindings processMessage processCommand model simulant world =
        List.fold (fun world binding ->
            Stream.monitor (fun evt world ->
                let signal = binding.MakeValue evt
                processSignal signal processMessage processCommand model simulant world)
                simulant binding.Stream world)
            world bindings
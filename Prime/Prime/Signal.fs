// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open Prime

/// A model-message-command signal.
type [<StructuralEquality; StructuralComparison>] Signal<'message, 'command> =
    | Message of message : 'message
    | Command of command : 'command

type [<NoEquality; NoComparison>] Channel<'m, 'c, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>> =
    { Source : Either<obj Address, Stream<obj, 'w>>
      Handler : Event<obj, 's> -> Signal<'m, 'c> list }

type Channel<'m, 'c, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>> with

    static member (=>) (_ : Channel<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (signals : Signal<'m, 'c> list) ->
            { Source = source |> atooa |> Left
              Handler = fun (_ : Event<obj, 's>) -> signals }

    static member (=>) (_ : Channel<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (signals : Signal<'m, 'c> list) ->
            { Source = source |> Stream.generalize |> Right
              Handler = fun (_ : Event<obj, 's>) -> signals }

    static member (=|>) (_ : Channel<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (handler : Event<'a, 's> -> Signal<'m, 'c> list) ->
            { Source = source |> atooa |> Left
              Handler = fun evt -> Event.generalize evt |> Event.specialize |> handler }
  
    static member (=|>) (_ : Channel<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (handler : Event<'a, 's> -> Signal<'m, 'c> list) ->
            { Source = source |> Stream.generalize |> Right
              Handler = fun evt -> Event.generalize evt |> Event.specialize |> handler }

[<AutoOpen>]
module SignalOperators =

    let inline (=>) source signals : Channel<'m, 'c, 's, 'w> =
        (Unchecked.defaultof<Channel<'m, 'c, 's, 'w>> => source) signals

    let inline (=|>) source signals : Channel<'m, 'c, 's, 'w> =
        (Unchecked.defaultof<Channel<'m, 'c, 's, 'w>> =|> source) signals

    let msg message = Message message
    let msgs messages = List.map Message messages
    let cmd command = Command command
    let cmds commands = List.map Command commands
    let withMsg value message = (value, [Message message])
    let withMsgs value messages = (value, msgs messages)
    let withCmd value command = (value, [Command command])
    let withCmds value commands = (value, cmds commands)
    let withSig value (signal : Signal<_, _>) = (value, [signal])
    let withSigs value signals = (value, signals)
    let just value = (value, [])

[<RequireQualifiedAccess>]
module Signal =

    let rec
        processSignal
        (processMessage : 'model * 'message * 's * 'w -> 'model * Signal<'message, 'command> list)
        (processCommand : 'model * 'command * 's * 'w -> 'w * Signal<'message, 'command> list)
        (model : Lens<'model, 'w>)
        (signal : Signal<'message, 'command>)
        (simulant : 's)
        (world : 'w) =
        match signal with
        | Message message ->
            let (modelValue, signals) = processMessage (model.Get world, message, simulant, world)
            let world = model.Set modelValue world
            processSignals processMessage processCommand model signals simulant world
        | Command command ->
            let (world, signals) = processCommand (model.Get world, command, simulant, world)
            processSignals processMessage processCommand model signals simulant world

    and processSignals processMessage processCommand model signals simulant world =
        List.fold
            (fun world signal -> processSignal processMessage processCommand model signal simulant world)
            world signals

    let processChannels processMessage processCommand model channels simulant world =
        List.fold (fun world channel ->
            match channel.Source with
            | Left address ->
                EventSystem.monitor (fun evt world ->
                    let signals = channel.Handler evt
                    List.fold (fun world signal -> processSignal processMessage processCommand model signal simulant world) world signals)
                    address simulant world
            | Right stream ->
                Stream.monitor (fun evt world ->
                    let signals = channel.Handler evt
                    List.fold (fun world signal -> processSignal processMessage processCommand model signal simulant world) world signals)
                    simulant stream world)
            world
            channels
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
      Handler : Event<obj, 's> -> Signal<'m, 'c> }

type Channel<'m, 'c, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>> with

    static member (=>) (_ : Channel<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (signal : Signal<'m, 'c>) ->
            { Source = source |> atooa |> Left
              Handler = fun (_ : Event<obj, 's>) -> signal }

    static member (=>) (_ : Channel<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (signal : Signal<'m, 'c>) ->
            { Source = source |> Stream.generalize |> Right
              Handler = fun (_ : Event<obj, 's>) -> signal }

    static member (=|>) (_ : Channel<'m, 'c, 's, 'w>, source : Address<'a>) =
        fun (handler : Event<'a, 's> -> Signal<'m, 'c>) ->
            { Source = source |> atooa |> Left
              Handler = fun evt -> Event.generalize evt |> Event.specialize |> handler }
  
    static member (=|>) (_ : Channel<'m, 'c, 's, 'w>, source : Stream<'a, 'w>) =
        fun (handler : Event<'a, 's> -> Signal<'m, 'c>) ->
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
    let withMsg message value = ([Message message], value)
    let withMsgs messages value = (msgs messages, value)
    let withCmd command value = ([Command command], value)
    let withCmds commands value = (cmds commands, value)
    let withSig (signal : Signal<_, _>) value = ([signal], value)
    let withSigs signals value = (signals, value)
    let just value = ([], value)

[<RequireQualifiedAccess>]
module Signal =

    let rec internal
        processSignalInternal
        (processMessage : 'model * 'message * 's * 'w -> Signal<'message, 'command> list * 'model)
        (processCommand : 'model * 'command * 's * 'w -> Signal<'message, 'command> list * 'w)
        (model : 'model)
        (signal : Signal<'message, 'command>)
        (simulant : 's)
        (world : 'w) =
        match signal with
        | Message message ->
            let (signals, model) = processMessage (model, message, simulant, world)
            match signals with
            | _ :: _ -> processSignalsInternal processMessage processCommand model signals simulant world
            | [] -> (model, world)
        | Command command ->
            let (signals, world) = processCommand (model, command, simulant, world)
            match signals with
            | _ :: _ -> processSignalsInternal processMessage processCommand model signals simulant world
            | [] -> (model, world)

    and internal processSignalsInternal processMessage processCommand model signals simulant world =
        List.fold
            (fun (model, world) signal -> processSignalInternal processMessage processCommand model signal simulant world)
            (model, world)
            signals

    let processSignal processMessage processCommand modelLens signal simulant world =
        let model = Lens.get modelLens world
        let (model, world) = processSignalInternal processMessage processCommand model signal simulant world
        let world = Lens.set model modelLens world
        world

    let processSignals processMessage processCommand modelLens signals simulant world =
        let model = Lens.get modelLens world
        let (model, world) = processSignalsInternal processMessage processCommand model signals simulant world
        let world = Lens.set model modelLens world
        world

    let processChannels processMessage processCommand modelLens channels simulant world =
        List.fold (fun world channel ->
            match channel.Source with
            | Left address ->
                EventSystem.monitor (fun evt world ->
                    let signal = channel.Handler evt
                    let model = Lens.get modelLens world
                    let (model, world) = processSignalInternal processMessage processCommand model signal simulant world
                    let world = Lens.set model modelLens world
                    (Cascade, world))
                    address simulant world
            | Right stream ->
                Stream.monitor (fun evt world ->
                    let signal = channel.Handler evt
                    let model = Lens.get modelLens world
                    let (model, world) = processSignalInternal processMessage processCommand model signal simulant world
                    let world = Lens.set model modelLens world
                    world)
                    simulant stream world)
            world
            channels
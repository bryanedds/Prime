// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open Prime

/// A model-message-command signal.
/// TODO: consider if there is any wisdom in making this a struct.
type [<StructuralEquality; StructuralComparison>] Signal<'message, 'command> =
    | Message of message : 'message
    | Command of command : 'command
    //| Update of transform : 'model -> 's -> 'w -> 'model
    //| Effect of effect : 'model -> 's -> 'w -> 'w

[<AutoOpen>]
module SignalOperators =

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

    let rec
        processSignal
        (processMessage : 'model * 'message * 's * 'w -> Signal<'message, 'command> list * 'model)
        (processCommand : 'model * 'command * 's * 'w -> Signal<'message, 'command> list * 'w)
        (modelLens : Lens<'model, 's, 'w>)
        (signal : Signal<'message, 'command>)
        (simulant : 's)
        (world : 'w) :
        'w =
        match signal with
        | Message message ->
            let model = Lens.get modelLens world
            let (signals, model) = processMessage (model, message, simulant, world)
            let world = Lens.set model modelLens world
            match signals with
            | _ :: _ -> processSignals processMessage processCommand modelLens signals simulant world
            | [] -> world
        | Command command ->
            let model = Lens.get modelLens world
            let (signals, world) = processCommand (model, command, simulant, world)
            match signals with
            | _ :: _ -> processSignals processMessage processCommand modelLens signals simulant world
            | [] -> world

    and processSignals processMessage processCommand modelLens signals simulant world =
        List.fold
            (fun world signal -> processSignal processMessage processCommand modelLens signal simulant world)
            world signals
﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System.Diagnostics
open Prime

/// The Chain monad. Allows the user to define a chain of operations over the world that
/// optionally spans across a bounded number of events.
///
/// The following is a potentially tail-recursible representation as speculated by @tpetricek -
/// 'w -> ('w * Either<'e -> Chain<'e, 'a, 'w>, 'a> -> 'a) -> 'a
type [<NoEquality; NoComparison>] Chain<'e, 'a, 'w when 'w :> EventSystem<'w>> =
    Chain of ('w -> 'w * Either<'e -> Chain<'e, 'a, 'w>, 'a>)

/// Implements the chain monad.
type ChainBuilder () =

    /// Functor map for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Map (f : 'a -> 'b) (a : Chain<'e, 'a, 'w>) : Chain<'e, 'b, 'w> =
        Chain (fun w ->
            let chainMapper eir =
                match eir with
                | Left c -> Left (fun w -> this.Map f (c w))
                | Right a -> Right (f a)
            let (w, eir) = match a with Chain b -> b w
            (w, chainMapper eir))

    /// Applicative apply for the chain monad.
    /// TODO: Implement!
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Apply (m : Chain<'e, ('a -> 'b), 'w>) (_ : Chain<'e, 'a, 'w>) : Chain<'e, 'b, 'w> =
        Chain (fun world ->
            match (match m with Chain f -> f world) with
            //                             ^--- NOTE: unbounded recursion here
            | _ -> failwithnie ())

    /// Monadic return for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Return (a : 'a) : Chain<'e, 'a, 'w> =
        Chain (fun world -> (world, Right a))

    /// Monadic bind for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Bind (m : Chain<'e, 'a, 'w>, cont : 'a -> Chain<'e, 'b, 'w>) : Chain<'e, 'b, 'w> =
        Chain (fun world ->
            match (match m with Chain f -> f world) with
            //                             ^--- NOTE: unbounded recursion here
            | (world, Left m) -> (world, Left (fun e -> this.Bind (m e, cont)))
            | (world, Right v) -> match cont v with Chain f -> f world)

[<AutoOpen>]
module ChainBuilder =

    /// Builds the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] chain = ChainBuilder ()

[<RequireQualifiedAccess>]
module Chain =

    /// Functor map for the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline map f a = chain.Map f a

    /// Functor map for the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline apply m a = chain.Apply m a

    /// Monadic return for the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline returnM a = chain.Return a

    /// Monadic bind for the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline bind m a = chain.Bind (m, a)

    /// Get the world.
    let [<DebuggerHidden; DebuggerStepThrough>] get : Chain<'e, 'w, 'w> =
        Chain (fun world -> (world, Right world))

    /// Get the world as transformed via 'by'.
    let [<DebuggerHidden; DebuggerStepThrough>] getBy by : Chain<'e, 'a, 'w> =
        Chain (fun world -> (world, Right (by world)))

    /// Set the world.
    let [<DebuggerHidden; DebuggerStepThrough>] set world : Chain<'e, unit, 'w> =
        Chain (fun _ -> (world, Right ()))

    /// Update the world with an additional transformed world parameter.
    let [<DebuggerHidden; DebuggerStepThrough>] updateBy by expr : Chain<'e, unit, 'w> =
        Chain (fun world -> (expr (by world) world, Right ()))

    /// Update the world.
    let [<DebuggerHidden; DebuggerStepThrough>] update expr : Chain<'e, unit, 'w> =
        Chain (fun world -> (expr world, Right ()))

    /// Get the next event.
    let [<DebuggerHidden; DebuggerStepThrough>] next : Chain<'e, 'e, 'w> =
        Chain (fun world -> (world, Left returnM))

    /// Pass over the next event.
    let [<DebuggerHidden; DebuggerStepThrough>] pass : Chain<'e, unit, 'w> =
        Chain (fun world -> (world, Left (fun _ -> returnM ())))

    /// React to the next event, using the event's data in the reaction.
    // TODO: See if we can make this acceptable to F#'s type system -
    //let [<DebuggerHidden; DebuggerStepThrough>] reactData<'a, 's, 'e, 'w when 's :> Simulant and 'e :> Event<'a, 's> and 'w :> EventSystem<'w>> expr : Chain<'e, unit, 'w> =
    //    chain {
    //        let! e = next
    //        let! world = get
    //        let world = expr (e.Data) world
    //        do! set world }

    /// React to the next event, using the event's value in the reaction.
    let [<DebuggerHidden; DebuggerStepThrough>] reactEvent expr : Chain<'e, unit, 'w> =
        chain {
            let! e = next
            let! world = get
            let world = expr e world
            do! set world }

    /// React to the next event, discarding the event's value.
    let [<DebuggerHidden; DebuggerStepThrough>] react expr : Chain<'e, unit, 'w> =
        chain {
            do! pass
            let! world = get
            let world = expr world
            do! set world }

    /// Loop in a chain context while 'pred' evaluate to true considering only the loop data.
    let rec [<DebuggerHidden; DebuggerStepThrough>] loop (i : 'i) (step : 'i -> 'i) (pred : 'i -> 'w -> bool) (m : 'i -> Chain<'e, unit, 'w>) =
        chain {
            let! world = get
            do! if pred i world then
                    chain {
                        do! m i
                        let i = step i
                        do! loop i step pred m }
                else returnM () }

    /// Loop in a chain context while 'pred' evaluates to true considering only the world state.
    let [<DebuggerHidden; DebuggerStepThrough>] during (pred : 'w -> bool) (m : Chain<'e, unit, 'w>) =
        loop () id (fun _ -> pred) (fun _ -> m)

    /// Step once into a chain.
    let [<DebuggerHidden; DebuggerStepThrough>] step (m : Chain<'e, 'a, 'w>) (world : 'w) : 'w * Either<'e -> Chain<'e, 'a, 'w>, 'a> =
        match m with Chain f -> f world

    /// Advance a chain value by one step, providing 'e'.
    let [<DebuggerHidden; DebuggerStepThrough>] advance (m : 'e -> Chain<'e, 'a, 'w>) (e : 'e) (world : 'w) : 'w * Either<'e -> Chain<'e, 'a, 'w>, 'a> =
        step (m e) world

    /// Run a chain to its end, providing 'e' for all its steps.
    let rec [<DebuggerHidden; DebuggerStepThrough>] run3 (m : Chain<'e, 'a, 'w>) (e : 'e) (world : 'w) : ('w * 'a) =
        match step m world with
        | (world', Left m') -> run3 (m' e) e world'
        | (world', Right v) -> (world', v)

    /// Run a chain to its end, providing unit for all its steps.
    let [<DebuggerHidden; DebuggerStepThrough>] run2 (m : Chain<unit, 'a, 'w>) (world : 'w) : ('w * 'a) =
        run3 m () world

    /// Run a chain to its end, providing unit for all its steps.
    let [<DebuggerHidden; DebuggerStepThrough>] run (m : Chain<unit, 'a, 'w>) (world : 'w) : 'w =
        run2 m world |> fst

    let [<DebuggerHidden; DebuggerStepThrough>] private run4 handling (chain : Chain<Event<'a, Simulant>, unit, 'w>) (stream : Stream<'a, 'w>) (world : 'w) =
        let globalSimulant = EventSystem.getGlobalSimulantGeneralized world
        let stateId = makeGuid ()
        let subscriptionId = makeGuid ()
        let world = EventSystem.addEventState stateId (fun (_ : Event<'a, Simulant>) -> chain) world
        let (eventAddress, unsubscribe, world) = stream.Subscribe world
        let unsubscribe = fun world ->
            let world = EventSystem.removeEventState stateId world
            let world = unsubscribe world
            EventSystem.unsubscribe subscriptionId world
        let advance = fun evt world ->
            let chain = EventSystem.getEventState stateId world : Event<'a, Simulant> -> Chain<Event<'a, Simulant>, unit, 'w>
            let (world, advanceResult) = advance chain evt world
            match advanceResult with
            | Right () -> unsubscribe world
            | Left chainNext -> EventSystem.addEventState stateId chainNext world
        let callback = fun evt world ->
            let world = advance evt world
            (handling, world)
        let world = advance Unchecked.defaultof<Event<'a, Simulant>> world
        let world = EventSystem.subscribePlus<'a, Simulant, 'w> subscriptionId callback eventAddress globalSimulant world |> snd
        (unsubscribe, world)

    /// Run a chain over Prime's event system.
    /// Allows each chainhronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runAssumingCascade chain (stream : Stream<'a, 'w>) world =
        run4 Cascade chain stream world

    /// Run a chain over Prime's event system.
    /// Allows each chainhronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runAssumingResolve chain (stream : Stream<'a, 'w>) world =
        run4 Resolve chain stream world
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime.Tests
open System
open Xunit
open Prime
module EventTests =

    type [<NoEquality; NoComparison>] TestSimulant =
        { TestAddress : TestSimulant Address }
        interface Simulant with
            member this.SimulantAddress = atoa<TestSimulant, Simulant> this.TestAddress
            end

    type [<ReferenceEquality; NoComparison>] TestWorld =
        { TestState : int
          TestEventSystemDelegate : TestWorld EventSystemDelegate }
        interface EventSystem<TestWorld> with
            member this.GetLiveness () = Live
            member this.GetGlobalSimulantSpecialized () = EventSystemDelegate.getGlobalSimulantSpecialized this.TestEventSystemDelegate
            member this.GetGlobalSimulantGeneralized () = EventSystemDelegate.getGlobalSimulantGeneralized this.TestEventSystemDelegate
            member this.SimulantExists simulant = simulant.GetType () = typeof<TestSimulant>
            member this.GetPropertyOpt _ _ = failwithnie ()
            member this.SetPropertyOpt _ _ _ = failwithnie ()
            member this.HandlePropertyChange _ _ _ = failwithnie ()
            member this.GetEventSystemDelegateHook () = this.TestEventSystemDelegate
            member this.UpdateEventSystemDelegateHook updater = { this with TestEventSystemDelegate = updater this.TestEventSystemDelegate }
            member this.HandleUserDefinedCallback _ _ _ = failwithnie ()
            member this.PublishEventHook (simulant : Simulant) publisher eventData eventAddress eventTrace callback world =
                match simulant with
                | :? GlobalSimulantGeneralized -> EventSystem.publishEvent<'a, 'p, Simulant, TestWorld> simulant publisher eventData eventAddress eventTrace callback world
                | :? TestSimulant -> EventSystem.publishEvent<'a, 'p, TestSimulant, TestWorld> simulant publisher eventData eventAddress eventTrace callback world
                | _ -> failwithumf ()
            member this.SubscribeEventHook _ _ world = world
            member this.UnsubscribeEventHook _ _ world = world
        static member incTestState this =
            { this with TestState = inc this.TestState }
        static member make eventTracerOpt eventFilter globalSimulant globalContext =
            { TestState = 0; TestEventSystemDelegate = EventSystemDelegate.make eventTracerOpt eventFilter globalSimulant globalContext }

    let TestEvent = ntoa<int> "Inc"
    let TestEvent2 = ntoa<bool> "Flag"
    let TestSimulantGeneralized = { GpgAddress = Address.empty<GlobalSimulantGeneralized> }
    let TestSimulantSpecialized = { TestAddress = Address.empty<TestSimulant> }
    let incTestState _ world = TestWorld.incTestState world
    let incTestStateNoEvent world = TestWorld.incTestState world
    let incTestStateTwice _ world = TestWorld.incTestState (TestWorld.incTestState world)
    let incTestStateTwiceNoEvent world = TestWorld.incTestState (TestWorld.incTestState world)
    let incTestStateAndCascade (_ : Event<int, TestSimulant>) world = (Cascade, TestWorld.incTestState world)
    let incTestStateAndResolve (_ : Event<int, TestSimulant>) world = (Resolve, TestWorld.incTestState world)

    let [<Fact>] subscribeWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world = EventSystem.subscribe incTestStateAndCascade TestEvent TestSimulantSpecialized world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world = EventSystem.subscribe incTestStateAndCascade TestEvent TestSimulantSpecialized world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world = EventSystem.subscribe incTestStateAndCascade TestEvent TestSimulantSpecialized world
        let world = EventSystem.subscribe incTestStateAndCascade TestEvent TestSimulantSpecialized world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeWithResolutionWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world = EventSystem.subscribePlus (makeGuid ()) incTestStateAndResolve TestEvent TestSimulantSpecialized world |> snd
        let world = EventSystem.subscribePlus (makeGuid ()) incTestStateAndCascade TestEvent TestSimulantSpecialized world |> snd
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] unsubscribeWorks () =
        let key = makeGuid ()
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world = EventSystem.subscribePlus key incTestStateAndResolve TestEvent TestSimulantSpecialized world |> snd
        let world = EventSystem.unsubscribe key world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (0, world.TestState)

    let [<Fact>] streamWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world = Stream.make TestEvent |> Stream.subscribe incTestState TestSimulantSpecialized $ world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] streamSubscribeTwiceUnsubscribeOnceWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let stream = Stream.make TestEvent
        let world = Stream.subscribe incTestState TestSimulantSpecialized stream world
        let (unsubscribe, world) = Stream.subscribeEffect incTestStateAndCascade TestSimulantSpecialized stream world
        let world = unsubscribe world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] streamUnsubscribeWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let (unsubscribe, world) = Stream.make TestEvent |> Stream.subscribeEffect incTestStateAndCascade TestSimulantSpecialized $ world
        let world = unsubscribe world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.True (UMap.isEmpty (EventSystem.getSubscriptions world))
        Assert.Equal (0, world.TestState)

    let [<Fact>] filterWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world =
            Stream.make TestEvent |>
            Stream.filterWorld (fun _ world -> world.TestState = 0) |>
            Stream.subscribe incTestState TestSimulantSpecialized $
            world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] mapWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world =
            Stream.make TestEvent |>
            Stream.map (fun a -> a * 2) |>
            Stream.subscribe (fun evt world -> { world with TestState = evt.Data }) TestSimulantSpecialized $
            world
        let world = EventSystem.publish 1 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (2, world.TestState)

    let [<Fact>] productWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world =
            Stream.product (Stream.make TestEvent) (Stream.make TestEvent) |>
            Stream.subscribe (fun evt world -> { world with TestState = fst evt.Data + snd evt.Data }) TestSimulantSpecialized $
            world
        let world = EventSystem.publish 1 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (2, world.TestState)

    let [<Fact>] sumWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world =
            Stream.sum (Stream.make TestEvent) (Stream.make TestEvent2) |>
            Stream.subscribe (fun evt world -> { world with TestState = match evt.Data with Left i -> i | Right _ -> 10 }) TestSimulantSpecialized $
            world
        let world = EventSystem.publish 1 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (1, world.TestState)
        let world = EventSystem.publish true TestEvent2 EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (10, world.TestState)

    let [<Fact>] scanWorks () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let world =
            Stream.make TestEvent |>
            Stream.fold (+) 0 |>
            Stream.subscribe (fun evt world -> { world with TestState = evt.Data }) TestSimulantSpecialized $
            world
        let world = EventSystem.publish 1 TestEvent EventTrace.empty TestSimulantSpecialized world
        let world = EventSystem.publish 2 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (3, world.TestState)

    let [<Fact>] scan2DoesntLeaveGarbage () =
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let (unsubscribe, world) =
            Stream.make TestEvent |>
            Stream.reduce (+) |>
            Stream.subscribeEffect incTestStateAndCascade TestSimulantSpecialized $
            world
        let world = EventSystem.publish 0 TestEvent EventTrace.empty TestSimulantSpecialized world
        let world = unsubscribe world
        Assert.True (UMap.isEmpty (EventSystem.getSubscriptions world))

    let [<Fact>] chainWorks () =

        // build everything
        let world = TestWorld.make None EventFilter.Empty TestSimulantSpecialized TestSimulantGeneralized
        let chain =
            chain {
                let! e = Chain.next
                do! Chain.update $ incTestState e
                do! Chain.react incTestStateNoEvent
                do! Chain.reactEvent incTestState
                do! Chain.pass
                do! Chain.loop 0 inc (fun i _ -> i < 1) (fun _ -> Chain.update incTestStateTwiceNoEvent) }
        let stream = Stream.make TestEvent
        let world = Chain.runAssumingCascade chain stream world |> snd
        Assert.Equal (0, world.TestState)

        // assert the first publish executes the first chained operation
        let world = EventSystem.publish 1 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (1, world.TestState)

        // assert the second publish executes the second chained operation
        let world = EventSystem.publish 2 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (2, world.TestState)

        // and so on...
        let world = EventSystem.publish 3 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (3, world.TestState)

        // and so on...
        let world = EventSystem.publish 4 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (5, world.TestState)
        
        // and so on...
        // TODO: P1: inspect this part of the test - the expected value seems wrong!
        let world = EventSystem.publish 5 TestEvent EventTrace.empty TestSimulantSpecialized world
        let world = EventSystem.publish 6 TestEvent EventTrace.empty TestSimulantSpecialized world
        Assert.Equal (5, world.TestState)
        
        // assert no garbage is left over after chained computation is concluded
        Assert.True (UMap.isEmpty (EventSystem.getSubscriptions world))
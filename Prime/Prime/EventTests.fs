// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime.Tests
open System
open Xunit
open Prime
module EventTests =

    type [<StructuralEquality; NoComparison>] TestParticipant =
        { TestAddress : TestParticipant Address }
        interface Participant with
            member this.ParticipantAddress = atoa<TestParticipant, Participant> this.TestAddress
            end

    type [<ReferenceEquality>] TestWorld =
        { TestState : int
          TestEventDelegate : TestWorld EventDelegate }
        interface EventWorld<TestParticipant, TestWorld> with
            member this.GetLiveness () = Running
            member this.GetGlobalParticipantSpecialized () = EventDelegate.getGlobalParticipantSpecialized this.TestEventDelegate
            member this.GetGlobalParticipantGeneralized () = EventDelegate.getGlobalParticipantGeneralized this.TestEventDelegate
            member this.ParticipantExists participant = participant.GetType () = typeof<TestParticipant>
            member this.GetPropertyOpt _ _ = failwithnie ()
            member this.SetPropertyOpt _ _ _ = failwithnie ()
            member this.HandlePropertyChange _ _ _ = failwithnie ()
            member this.GetEventDelegateHook () = this.TestEventDelegate
            member this.UpdateEventDelegateHook updater = { this with TestEventDelegate = updater this.TestEventDelegate }
            member this.PublishEventHook (participant : Participant) publisher eventData eventAddress eventTrace subscription world =
                match participant with
                | :? GlobalParticipantGeneralized -> EventWorld.publishEvent<'a, 'p, Participant, TestParticipant, TestWorld> participant publisher eventData eventAddress eventTrace subscription world
                | :? TestParticipant -> EventWorld.publishEvent<'a, 'p, TestParticipant, TestParticipant, TestWorld> participant publisher eventData eventAddress eventTrace subscription world
                | _ -> failwithumf ()
        static member incTestState this =
            { this with TestState = inc this.TestState }
        static member make eventTracer eventTracing eventFilter globalParticipant globalContext =
            { TestState = 0; TestEventDelegate = EventDelegate.make eventTracer eventTracing eventFilter globalParticipant globalContext }

    let TestEvent = ntoa<int> "Inc"
    let TestEvent2 = ntoa<bool> "Flag"
    let TestParticipantGeneralized = { GpgAddress = Address.empty<GlobalParticipantGeneralized> }
    let TestParticipantSpecialized = { TestAddress = Address.empty<TestParticipant> }
    let incTestState _ world = TestWorld.incTestState world
    let incTestStateNoEvent world = TestWorld.incTestState world
    let incTestStateTwice _ world = TestWorld.incTestState (TestWorld.incTestState world)
    let incTestStateTwiceNoEvent world = TestWorld.incTestState (TestWorld.incTestState world)
    let incTestStateAndCascade (_ : Event<int, TestParticipant>) world = (Cascade, TestWorld.incTestState world)
    let incTestStateAndResolve (_ : Event<int, TestParticipant>) world = (Resolve, TestWorld.incTestState world)

    let [<Fact>] subscribeWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world = EventWorld.subscribe incTestState TestEvent TestParticipantSpecialized world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world = EventWorld.subscribe incTestState TestEvent TestParticipantSpecialized world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world = EventWorld.subscribe incTestState TestEvent TestParticipantSpecialized world
        let world = EventWorld.subscribe incTestState TestEvent TestParticipantSpecialized world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeWithResolutionWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world = EventWorld.subscribePlus (makeGuid ()) incTestStateAndResolve TestEvent TestParticipantSpecialized world |> snd
        let world = EventWorld.subscribePlus (makeGuid ()) incTestStateAndCascade TestEvent TestParticipantSpecialized world |> snd
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] unsubscribeWorks () =
        let key = makeGuid ()
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world = EventWorld.subscribePlus key incTestStateAndResolve TestEvent TestParticipantSpecialized world |> snd
        let world = EventWorld.unsubscribe key world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (0, world.TestState)

    let [<Fact>] streamWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world = Stream.make TestEvent |> Stream.subscribe incTestState TestParticipantSpecialized <| world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] streamSubscribeTwiceUnsubscribeOnceWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let stream = Stream.make TestEvent
        let world = Stream.subscribe incTestState TestParticipantSpecialized stream world
        let (unsubscribe, world) = Stream.subscribePlus incTestStateAndCascade TestParticipantSpecialized stream world
        let world = unsubscribe world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] streamUnsubscribeWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let (unsubscribe, world) = Stream.make TestEvent |> Stream.subscribePlus incTestStateAndCascade TestParticipantSpecialized <| world
        let world = unsubscribe world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.True (UMap.isEmpty (EventWorld.getSubscriptions world))
        Assert.Equal (0, world.TestState)

    let [<Fact>] filterWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world =
            Stream.make TestEvent |>
            Stream.filterWorld (fun _ world -> world.TestState = 0) |>
            Stream.subscribe incTestState TestParticipantSpecialized <|
            world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (1, world.TestState)

    let [<Fact>] mapWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world =
            Stream.make TestEvent |>
            Stream.map (fun a -> a * 2) |>
            Stream.subscribe (fun evt world -> { world with TestState = evt.Data }) TestParticipantSpecialized <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (2, world.TestState)

    let [<Fact>] productWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world =
            Stream.product (Stream.make TestEvent) (Stream.make TestEvent) |>
            Stream.subscribe (fun evt world -> { world with TestState = fst evt.Data + snd evt.Data }) TestParticipantSpecialized <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (2, world.TestState)

    let [<Fact>] sumWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world =
            Stream.sum (Stream.make TestEvent) (Stream.make TestEvent2) |>
            Stream.subscribe (fun evt world -> { world with TestState = match evt.Data with Left i -> i | Right _ -> 10 }) TestParticipantSpecialized <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (1, world.TestState)
        let world = EventWorld.publish true TestEvent2 EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (10, world.TestState)

    let [<Fact>] scanWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let world =
            Stream.make TestEvent |>
            Stream.fold (+) 0 |>
            Stream.subscribe (fun evt world -> { world with TestState = evt.Data }) TestParticipantSpecialized <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipantSpecialized world
        let world = EventWorld.publish 2 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (3, world.TestState)

    let [<Fact>] scan2DoesntLeaveGarbage () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let (unsubscribe, world) =
            Stream.make TestEvent |>
            Stream.reduce (+) |>
            Stream.subscribePlus incTestStateAndCascade TestParticipantSpecialized <|
            world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipantSpecialized world
        let world = unsubscribe world
        Assert.True (UMap.isEmpty (EventWorld.getSubscriptions world))

    let [<Fact>] chainWorks () =
        
        // build everything
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipantSpecialized TestParticipantGeneralized
        let chain =
            chain {
                let! e = Chain.next
                do! Chain.update (incTestState e)
                do! Chain.react incTestStateNoEvent
                do! Chain.reactE incTestState
                do! Chain.pass
                do! Chain.loop 0 inc (fun i _ -> i < 2) (fun _ -> Chain.update incTestStateTwiceNoEvent) }
        let stream = Stream.make TestEvent
        let world = Chain.runAssumingCascade chain stream world |> snd
        Assert.Equal (0, world.TestState)

        // assert the first publish executes the first chained operation
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (1, world.TestState)

        // assert the second publish executes the second chained operation
        let world = EventWorld.publish 2 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (2, world.TestState)
        
        // and so on...
        let world = EventWorld.publish 3 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (3, world.TestState)
        
        // and so on...
        let world = EventWorld.publish 4 TestEvent EventTrace.empty TestParticipantSpecialized world
        Assert.Equal (7, world.TestState)
        
        // assert no garbage is left over after chained computation is concluded
        Assert.True (UMap.isEmpty (EventWorld.getSubscriptions world))
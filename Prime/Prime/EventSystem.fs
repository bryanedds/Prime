// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections.Generic
open System.Diagnostics
open Prime

/// A publisher-neutral event system.
/// Effectively a mix-in for the 'w type, where 'w is a type that represents the client program.
type EventSystem<'w when 'w :> 'w EventSystem> =
    interface
        inherit PropertySystem<'w>
        abstract member GetLiveness : unit -> Liveness
        abstract member GetGlobalSimulantSpecialized : unit -> Simulant
        abstract member GetGlobalSimulantGeneralized : unit -> GlobalSimulantGeneralized
        abstract member SimulantExists : Simulant -> bool
        abstract member GetEventSystemDelegateHook : unit -> 'w EventSystemDelegate
        abstract member UpdateEventSystemDelegateHook : ('w EventSystemDelegate -> 'w EventSystemDelegate) -> 'w
        abstract member HandleUserDefinedCallback : obj -> obj -> 'w -> Handling * 'w
        abstract member PublishEventHook<'a, 'p when 'p :> Simulant> : Simulant -> 'p -> obj -> 'a Address -> EventTrace -> obj -> 'w -> Handling * 'w
        end

/// Handles simulant property changes.
and SimulantPropertyChangeHandler<'w when 'w :> 'w EventSystem> =
    'w EventSystem -> 'w EventSystem -> 'w EventSystem

/// Detaches a simulant property change handler.
and SimulantPropertyChangeUnhandler<'w when 'w :> 'w EventSystem> =
    'w EventSystem -> 'w EventSystem

[<RequireQualifiedAccess>]
module EventSystem =

    let private getEventSystemDelegate<'w when 'w :> 'w EventSystem> (world : 'w) =
        world.GetEventSystemDelegateHook ()
        
    let private getEventSystemDelegateBy<'a, 'w when 'w :> 'w EventSystem> (by : 'w EventSystemDelegate -> 'a) (world : 'w) : 'a =
        let propertySystem = world.GetEventSystemDelegateHook ()
        by propertySystem
        
    let private updateEventSystemDelegate<'w when 'w :> 'w EventSystem> updater (world : 'w) =
        world.UpdateEventSystemDelegateHook updater

    /// Get event subscriptions.
    let getSubscriptions<'w when 'w :> 'w EventSystem> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getSubscriptions world

    /// Get event unsubscriptions.
    let getUnsubscriptions<'w when 'w :> 'w EventSystem> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getUnsubscriptions world

    /// Set event subscriptions.
    let setSubscriptions<'w when 'w :> 'w EventSystem> subscriptions (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setSubscriptions subscriptions) world

    /// Set event unsubscriptions.
    let setUnsubscriptions<'w when 'w :> 'w EventSystem> unsubscriptions (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setUnsubscriptions unsubscriptions) world

    /// Add event state to the world.
    let addEventState<'a, 'w when 'w :> 'w EventSystem> key (state : 'a) (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.addEventState key state) world

    /// Remove event state from the world.
    let removeEventState<'w when 'w :> 'w EventSystem> key (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.removeEventState key) world

    /// Get event state from the world.
    let getEventState<'a, 'g ,'w when 'w :> 'w EventSystem> key (world : 'w) : 'a =
        getEventSystemDelegateBy (EventSystemDelegate.getEventState<'a, 'w> key) world

    /// Get whether events are being traced.
    let getEventTracing<'w when 'w :> 'w EventSystem> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventTracing<'w> world

    /// Set whether events are being traced.
    let setEventTracing<'w when 'w :> 'w EventSystem> tracing (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setEventTracing tracing) world

    /// Get the state of the event filter.
    let getEventFilter<'w when 'w :> 'w EventSystem> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventFilter world

    /// Set the state of the event filter.
    let setEventFilter<'w when 'w :> 'w EventSystem> filter (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setEventFilter filter) world

    /// Get the event context of the world.
    let getEventContext<'w when 'w :> 'w EventSystem> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventContext world

    /// Set the event context of the world.
    let setEventContext<'w when 'w :> 'w EventSystem> context (world : 'w) =
        EventSystemDelegate.setEventContext context (getEventSystemDelegate world)

    /// Qualify the event context of the world.
    let qualifyEventContext<'w when 'w :> 'w EventSystem> (address : obj Address) (world : 'w) =
        getEventSystemDelegateBy (EventSystemDelegate.qualifyEventContext address) world

    let boxCallback<'a, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (callback : Callback<'a, 's, 'w>) =
        let boxableCallback = fun (evt : Event<obj, Simulant>) world ->
            let evt =
                { Data = evt.Data :?> 'a
                  Subscriber = evt.Subscriber :?> 's
                  Publisher = evt.Publisher
                  Address = Address.specialize<'a> evt.Address
                  Trace = evt.Trace }
            callback evt world
        boxableCallback :> obj

    let getSortableSubscriptions
        (getSortPriority : Simulant -> 'w -> IComparable)
        (subscriptions : SubscriptionEntry array)
        (world : 'w) :
        (IComparable * SubscriptionEntry) array =
        Array.map
            (fun (subscription : SubscriptionEntry) ->
                let priority = getSortPriority subscription.SubscriberEntry world
                (priority, subscription))
            subscriptions

    let getLiveness<'w when 'w :> 'w EventSystem> (world : 'w) =
        world.GetLiveness ()

    let getGlobalSimulantSpecialized<'w when 'w :> 'w EventSystem> (world : 'w) =
        world.GetGlobalSimulantSpecialized ()

    let getGlobalSimulantGeneralized<'w when 'w :> 'w EventSystem> (world : 'w) =
        world.GetGlobalSimulantGeneralized ()

    let simulantExists<'w when 'w :> 'w EventSystem> (simulant : Simulant) (world : 'w) =
        world.SimulantExists simulant

    /// Publish an event directly.
    let publishEvent<'a, 'p, 's, 'w when 'p :> Simulant and 's :> Simulant and 'w :> 'w EventSystem>
        (subscriber : Simulant) (publisher : 'p) (eventData : obj) (eventAddress : 'a Address) eventTrace (subscription : obj) (world : 'w) =
        let evt =
            { Data = eventData
              Subscriber = subscriber
              Publisher = publisher :> Simulant
              Address = atooa eventAddress
              Trace = eventTrace }
        let callableSubscription = subscription :?> 'w BoxableSubscription
        let oldEventContext = getEventContext world
        setEventContext subscriber world
        let (handling, world) = callableSubscription evt world
        setEventContext oldEventContext world
        (handling, world)

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry array) (world : 'w) =
        let subscriptions = getSortableSubscriptions by subscriptions world
        let subscriptions = Array.sortWith (fun ((p : IComparable), _) ((p2 : IComparable), _) -> p.CompareTo p2) subscriptions
        Array.map snd subscriptions

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry array) (_ : 'w) =
        subscriptions

    /// Publish an event, using the given publishSorter procedures to arrange the order to which subscriptions are published.
    let publishPlus<'a, 'p, 'w when 'p :> Simulant and 'w :> 'w EventSystem>
        (eventData : 'a)
        (eventAddress : 'a Address)
        eventTrace
        (publisher : 'p)
        (sorterOpt : SubscriptionSorter option)
        (world : 'w) =
        let eventAddressObj = atooa eventAddress
#if DEBUG
        logEvent<'w> eventAddressObj eventTrace world
#endif
        let subscriptions =
            match sorterOpt with
            | Some sorter ->
                EventSystemDelegate.getSubscriptionsSorted sorter eventAddressObj (getEventSystemDelegate world) world
            | None ->
                let subscriptions = EventSystemDelegate.getSubscriptions (getEventSystemDelegate world)
                match UMap.tryFind eventAddressObj subscriptions with Some subs -> subs | None -> [||]
        let (_, world) =
            Array.foldWhile
                (fun (handling, world : 'w) (subscription : SubscriptionEntry) ->
#if DEBUG
                    let eventAddresses = getEventAddresses world
                    let cycleDetected = List.containsTriplicates eventAddresses
                    if cycleDetected then Trace.WriteLine ("Event cycle detected in '" + scstring eventAddresses + "'.")
#else
                    let cycleDetected = false
#endif
                    if not cycleDetected &&
                        (match handling with Cascade -> true | Resolve -> false) &&
                        (match world.GetLiveness () with Running -> true | Exiting -> false) then
#if DEBUG
                        let world = pushEventAddress objEventAddress world
#endif
                        let mapped =
                            match subscription.MapperOpt with
                            | Some mapper -> mapper eventData subscription.PreviousDataOpt world
                            | None -> eventData :> obj
                        let filtered =
                            match subscription.FilterOpt with
                            | Some filter -> filter mapped subscription.PreviousDataOpt world
                            | None -> true
                        subscription.PreviousDataOpt <- Some mapped
                        let (handling, world) =
                            if filtered then
                                match subscription.Callback with
                                | UserDefinedCallback callback -> world.HandleUserDefinedCallback callback mapped world
                                | FunctionCallback callback -> world.PublishEventHook subscription.SubscriberEntry publisher mapped eventAddress eventTrace callback world
                            else (Cascade, world)
#if DEBUG
                        let world = popEventAddress world
#endif
                        Some (handling, world)
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event with no subscription sorting.
    let publish<'a, 'p, 'w when 'p :> Simulant and 'w :> 'w EventSystem>
        (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publishPlus<'a, 'p, 'w> eventData eventAddress eventTrace publisher None world

    /// Unsubscribe from an event.
    let unsubscribe<'w when 'w :> 'w EventSystem> subscriptionKey (world : 'w) =
        let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
        match UMap.tryFind subscriptionKey unsubscriptions with
        | Some (eventAddress, subscriber) ->
            match UMap.tryFind eventAddress subscriptions with
            | Some subscriptionsArray ->
                let subscriptionsArray =
                    Array.remove (fun subscription ->
                        Console.Write ""
                        subscription.SubscriptionKey = subscriptionKey && subscription.SubscriberEntry = subscriber)
                        subscriptionsArray
                let subscriptions = 
                    match subscriptionsArray with
                    | [||] -> UMap.remove eventAddress subscriptions
                    | _ -> UMap.add eventAddress subscriptionsArray subscriptions
                let unsubscriptions = UMap.remove subscriptionKey unsubscriptions
                let world = setSubscriptions subscriptions world
                let world = setUnsubscriptions unsubscriptions world
                publish<_, _, 'w>
                    eventAddress
                    (rtoa<obj Address> [|"Unsubscribe"; "Event"|])
                    (EventTrace.record "EventSystem" "unsubscribe" EventTrace.empty)
                    (getGlobalSimulantSpecialized world)
                    world
            | None -> world
        | None -> world

    /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
    let subscribeSpecial<'a, 'b, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (subscriptionKey : Guid)
        (mapperOpt : ('a -> 'b option -> 'w -> 'b) option)
        (filterOpt : ('b -> 'b option -> 'w -> bool) option)
        (stateOpt : 'b option)
        (callback : Either<Callback<'b, 's, 'w>, obj>)
        (eventAddress : 'a Address)
        (subscriber : 's)
        (world : 'w) =
        if not (Address.isEmpty eventAddress) then
            let objEventAddress = atooa eventAddress
            let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
            let callback = match callback with Left s ->  s |> boxCallback |> FunctionCallback | Right o -> UserDefinedCallback o
            let subscriptions =
                let subscriptionEntry =
                    { SubscriptionKey = subscriptionKey
                      SubscriberEntry = subscriber :> Simulant
                      MapperOpt = Option.map (fun mapper -> fun a p w -> mapper (a :?> 'a) (Option.map cast<'b> p) (w :?> 'w) :> obj) mapperOpt
                      FilterOpt = Option.map (fun filter -> fun b p w -> filter (b :?> 'b) (Option.map cast<'b> p) (w :?> 'w)) filterOpt
                      PreviousDataOpt = Option.map box stateOpt
                      Callback = callback }
                match UMap.tryFind objEventAddress subscriptions with
                | Some subscriptionEntries ->
                    UMap.add objEventAddress (Array.add subscriptionEntry subscriptionEntries) subscriptions
                | None ->
                    UMap.add objEventAddress [|subscriptionEntry|] subscriptions
            let unsubscriptions = UMap.add subscriptionKey (objEventAddress, subscriber :> Simulant) unsubscriptions
            let world = setSubscriptions subscriptions world
            let world = setUnsubscriptions unsubscriptions world
            let world =
                publish
                    objEventAddress
                    (rtoa<obj Address> [|"Subscribe"; "Event"|])
                    (EventTrace.record "EventSystem" "subscribePlus5" EventTrace.empty)
                    (getGlobalSimulantSpecialized world)
                    world
            (unsubscribe<'w> subscriptionKey, world)
        else failwith "Event name cannot be empty."

    /// Subscribe to an event.
    let subscribePlus<'a, 'b, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (subscriptionKey : Guid)
        (mapperOpt : ('a -> 'b option -> 'w -> 'b) option)
        (filterOpt : ('b -> 'b option -> 'w -> bool) option)
        (stateOpt : 'b option)
        (callback : Callback<'b, 's, 'w>)
        (eventAddress : 'a Address)
        (subscriber : 's)
        (world : 'w) =
        subscribeSpecial subscriptionKey mapperOpt filterOpt stateOpt (Left callback) eventAddress subscriber world

    /// Subscribe to an event.
    let subscribe<'a, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (callback : Callback<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribePlus (makeGuid ()) None None None callback eventAddress subscriber world |> snd

    /// Keep active a subscription for the life span of a simulant, and be provided with an unsubscription callback.
    let monitorSpecial<'a, 'b, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (mapperOpt : ('a -> 'b option -> 'w -> 'b) option)
        (filterOpt : ('b -> 'b option -> 'w -> bool) option)
        (stateOpt : 'b option)
        (callback : Either<Callback<'b, 's, 'w>, obj>)
        (eventAddress : 'a Address)
        (subscriber : 's)
        (world : 'w) =
        let monitorKey = makeGuid ()
        let removalKey = makeGuid ()
        let world = subscribeSpecial<'a, 'b, 's, 'w> monitorKey mapperOpt filterOpt stateOpt callback eventAddress subscriber world |> snd
        let unsubscribe = fun (world : 'w) ->
            let world = unsubscribe removalKey world
            let world = unsubscribe monitorKey world
            world
        let callback' = Left (fun _ eventSystem -> (Cascade, unsubscribe eventSystem))
        let removingEventAddress = rtoa<obj> [|"Unregistering"; "Event"|] --> subscriber.SimulantAddress
        let world = subscribeSpecial<obj, obj, Simulant, 'w> removalKey None None None callback' removingEventAddress subscriber world |> snd
        (unsubscribe, world)

    /// Keep active a subscription for the life span of a simulant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 'b, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (mapperOpt : ('a -> 'b option -> 'w -> 'b) option)
        (filterOpt : ('b -> 'b option -> 'w -> bool) option)
        (stateOpt : 'b option)
        (callback : Callback<'b, 's, 'w>)
        (eventAddress : 'a Address)
        (subscriber : 's)
        (world : 'w) =
        monitorSpecial<'a, 'b, 's, 'w> mapperOpt filterOpt stateOpt (Left callback) eventAddress subscriber world

    /// Keep active a subscription for the life span of a simulant.
    let monitor<'a, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (callback : Callback<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        monitorPlus<'a, 'a, 's, 'w> None None None callback eventAddress subscriber world |> snd
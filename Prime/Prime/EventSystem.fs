// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open Prime

/// Handles simulant property changes.
type SimulantPropertyChangeHandler<'w when 'w :> 'w EventSystem> =
    'w EventSystem -> 'w EventSystem -> 'w EventSystem

/// Detaches a simulant property change handler.
and SimulantPropertyChangeUnhandler<'w when 'w :> 'w EventSystem> =
    'w EventSystem -> 'w EventSystem

/// A publisher-neutral event system.
/// Effectively a mix-in for the 'w type, where 'w is a type that represents the client program.
and EventSystem<'w when 'w :> 'w EventSystem> =
    interface
        inherit PropertySystem<'w>
        abstract member GetLiveness : unit -> Liveness
        abstract member GetSimulantExists : Simulant -> bool
        abstract member GetGlobalSimulantSpecialized : unit -> Simulant
        abstract member GetGlobalSimulantGeneralized : unit -> GlobalSimulantGeneralized
        abstract member GetEventSystemDelegate : unit -> 'w EventSystemDelegate
        abstract member UpdateEventSystemDelegate : ('w EventSystemDelegate -> 'w EventSystemDelegate) -> 'w
        abstract member HandleUserDefinedCallback : obj -> obj -> 'w -> Handling * 'w
        abstract member PublishEventHook<'a, 'p when 'p :> Simulant> : Simulant -> 'p -> obj -> 'a Address -> EventTrace -> obj -> 'w -> Handling * 'w
        abstract member SubscribeEventHook : obj Address -> Simulant -> 'w -> 'w
        abstract member UnsubscribeEventHook : obj Address -> Simulant -> 'w -> 'w
        end

[<RequireQualifiedAccess>]
module EventSystem =

    let private getEventSystemDelegate<'w when 'w :> 'w EventSystem> (world : 'w) =
        world.GetEventSystemDelegate ()
        
    let private getEventSystemDelegateBy<'a, 'w when 'w :> 'w EventSystem> (by : 'w EventSystemDelegate -> 'a) (world : 'w) : 'a =
        let propertySystem = world.GetEventSystemDelegate ()
        by propertySystem
        
    let private updateEventSystemDelegate<'w when 'w :> 'w EventSystem> updater (world : 'w) =
        world.UpdateEventSystemDelegate updater

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

    /// Get how events are being traced.
    let getEventTracerOpt<'w when 'w :> 'w EventSystem> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventTracerOpt<'w> world

    /// Set how events are being traced.
    let setEventTracerOpt<'w when 'w :> 'w EventSystem> tracerOpt (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setEventTracerOpt tracerOpt) world

    /// Get the state of the event filter.
    let getEventFilter<'w when 'w :> 'w EventSystem> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventFilter world

    /// Set the state of the event filter.
    let setEventFilter<'w when 'w :> 'w EventSystem> filter (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setEventFilter filter) world

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
        (subscriptionEntries : (Guid * SubscriptionEntry) seq)
        (world : 'w) =
        Seq.map
            (fun (_, subscription : SubscriptionEntry) ->
                // NOTE: we just take the sort priority of the first callback found when callbacks are compressed. This
                // is semantically sub-optimal, but should be fine for all of our cases.
                let priority = getSortPriority subscription.Subscriber world
                struct (priority, subscription))
            subscriptionEntries

    let getLiveness<'w when 'w :> 'w EventSystem> (world : 'w) =
        world.GetLiveness ()

    let getSimulantExists<'w when 'w :> 'w EventSystem> (simulant : Simulant) (world : 'w) =
        world.GetSimulantExists simulant

    let getGlobalSimulantSpecialized<'w when 'w :> 'w EventSystem> (world : 'w) =
        world.GetGlobalSimulantSpecialized ()

    let getGlobalSimulantGeneralized<'w when 'w :> 'w EventSystem> (world : 'w) =
        world.GetGlobalSimulantGeneralized ()

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
        callableSubscription evt world

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : (Guid * SubscriptionEntry) seq) (world : 'w) : seq<Guid * SubscriptionEntry> =
        getSortableSubscriptions by subscriptions world |>
        Array.ofSeq |>
        Array.sortWith (fun (struct ((p : IComparable), _)) (struct ((p2 : IComparable), _)) -> p.CompareTo p2) |>
        Array.map (fun (struct (_, subscription)) -> (subscription.SubscriptionId, subscription)) |>
        Array.toSeq

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
        EventSystemDelegate.logEvent<'w> eventAddressObj eventTrace (getEventSystemDelegate world)
#endif
        let subscriptions =
            match sorterOpt with
            | Some sorter ->
                EventSystemDelegate.getSubscriptionsSorted sorter eventAddressObj (getEventSystemDelegate world) world
            | None ->
                let subscriptions = EventSystemDelegate.getSubscriptions (getEventSystemDelegate world)
                match UMap.tryFind eventAddressObj subscriptions with
                | Some subs -> OMap.toSeq subs | None -> Seq.empty
        let (_, world) =
            Seq.foldWhile
                (fun (handling, world : 'w) (_ : Guid, subscription : SubscriptionEntry) ->
                    match (handling, world.GetLiveness ()) with
                    | (Cascade, Live) ->
                        let (handling, world) = world.PublishEventHook subscription.Subscriber publisher eventData eventAddress eventTrace subscription.CallbackBoxed world
                        Some (handling, world)
                    | (_, _) -> None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event with no subscription sorting.
    let publish<'a, 'p, 'w when 'p :> Simulant and 'w :> 'w EventSystem>
        (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publishPlus<'a, 'p, 'w> eventData eventAddress eventTrace publisher None world

    /// Unsubscribe from an event.
    let unsubscribe<'w when 'w :> 'w EventSystem> subscriptionId (world : 'w) =
        let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
        match UMap.tryFind subscriptionId unsubscriptions with
        | Some (eventAddress, _) ->
            match UMap.tryFind eventAddress subscriptions with
            | Some subscriptionEntries ->
                let subscriptions =
                    let subscriptionEntries = OMap.remove subscriptionId subscriptionEntries
                    if OMap.isEmpty subscriptionEntries
                    then UMap.remove eventAddress subscriptions
                    else UMap.add eventAddress subscriptionEntries subscriptions
                let unsubscriptions = UMap.remove subscriptionId unsubscriptions
                let world = setSubscriptions subscriptions world
                let world = setUnsubscriptions unsubscriptions world
                let world = world.UnsubscribeEventHook eventAddress (getGlobalSimulantSpecialized world) world
                world
            | None -> world
        | None -> world

    /// Subscribe to an event with the given subscription id.
    let subscribePlus<'a, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (subscriptionId : Guid)
        (callback : Event<'a, 's> -> 'w -> Handling * 'w)
        (eventAddress : 'a Address)
        (subscriber : 's)
        (world : 'w) =
        if not (Address.isEmpty eventAddress) then
            let eventAddressObj = atooa eventAddress
            let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
            let subscriptions =
                match UMap.tryFind eventAddressObj subscriptions with
                | Some subscriptionEntries ->
                    match OMap.tryFind subscriptionId subscriptionEntries with
                    | Some subscriptionEntry ->
                        let subscriptionEntry = { subscriptionEntry with CallbackBoxed = boxCallback callback }
                        let subscriptionEntries = OMap.add subscriptionId subscriptionEntry subscriptionEntries
                        UMap.add eventAddressObj subscriptionEntries subscriptions
                    | None ->
                        let subscriptionEntry = { SubscriptionId = subscriptionId; Subscriber = subscriber; CallbackBoxed = boxCallback callback }
                        let subscriptionEntries = OMap.add subscriptionId subscriptionEntry subscriptionEntries
                        UMap.add eventAddressObj subscriptionEntries subscriptions
                | None ->
                    let subscriptionEntry = { SubscriptionId = subscriptionId; Subscriber = subscriber; CallbackBoxed = boxCallback callback }
                    UMap.add eventAddressObj (OMap.makeSingleton subscriptionId subscriptionEntry Functional) subscriptions
            let unsubscriptions = UMap.add subscriptionId (eventAddressObj, subscriber :> Simulant) unsubscriptions
            let world = setSubscriptions subscriptions world
            let world = setUnsubscriptions unsubscriptions world
            let world = world.SubscribeEventHook eventAddressObj (getGlobalSimulantSpecialized world) world
            (unsubscribe<'w> subscriptionId, world)
        else failwith "Event name cannot be empty."

    /// Subscribe to an event.
    let subscribe<'a, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (callback : Event<'a, 's> -> 'w -> Handling * 'w) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribePlus (makeGuid ()) callback eventAddress subscriber world |> snd

    /// Keep active a subscription for the life span of a simulant.
    let monitorPlus<'a, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (callback : Event<'a, 's> -> 'w -> Handling * 'w)
        (eventAddress : 'a Address)
        (subscriber : 's)
        (world : 'w) =
        let removalId = makeGuid ()
        let monitorId = makeGuid ()
        let world = subscribePlus<'a, 's, 'w> monitorId callback eventAddress subscriber world |> snd
        let unsubscribe = fun (world : 'w) ->
            let world = unsubscribe removalId world
            let world = unsubscribe monitorId world
            world
        let callback' = fun _ eventSystem -> (Cascade, unsubscribe eventSystem)
        let removingEventAddress = rtoa<obj> [|"Unregistering"; "Event"|] --> subscriber.SimulantAddress
        let world = subscribePlus<obj, Simulant, 'w> removalId callback' removingEventAddress subscriber world |> snd
        (unsubscribe, world)

    /// Keep active a subscription for the life span of a simulant.
    let monitor<'a, 's, 'w when 's :> Simulant and 'w :> 'w EventSystem>
        (callback : Event<'a, 's> -> 'w -> Handling * 'w) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        monitorPlus<'a, 's, 'w> callback eventAddress subscriber world |> snd
﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections.Generic
open System.Diagnostics
open Prime

/// A publisher-neutral event system.
/// Effectively a mix-in for the 'w type, where 'w is a type that represents the client program.
type EventSystem<'w when 'w :> EventSystem<'w>> =
    interface
        inherit PropertySystem<'w>
        abstract member GetLiveness : unit -> Liveness
        abstract member GetGlobalSimulantSpecialized : unit -> Simulant
        abstract member GetGlobalSimulantGeneralized : unit -> GlobalSimulantGeneralized
        abstract member SimulantExists : Simulant -> bool
        abstract member GetEventSystemDelegateHook : unit -> 'w EventSystemDelegate
        abstract member UpdateEventSystemDelegateHook : ('w EventSystemDelegate -> 'w EventSystemDelegate) -> 'w
        abstract member PublishEventHook<'a, 'p when 'p :> Simulant> : Simulant -> 'p -> obj -> 'a Address -> EventTrace -> obj -> 'w -> Handling * 'w
        end

/// Handles simulant property changes.
and SimulantPropertyChangeHandler<'w when 'w :> EventSystem<'w>> =
    EventSystem<'w> -> EventSystem<'w> -> EventSystem<'w>

/// Detaches a simulant property change handler.
and SimulantPropertyChangeUnhandler<'w when 'w :> EventSystem<'w>> =
    EventSystem<'w> -> EventSystem<'w>

[<RequireQualifiedAccess>]
module EventSystem =

    let mutable EventAddressCaching = false
    let EventAddressCache = Dictionary<obj, obj> HashIdentity.Structural
    let EventAddressListCache = Dictionary<obj Address, obj List> HashIdentity.Structural
    
    let private getEventSystemDelegate<'w when 'w :> EventSystem<'w>> (world : 'w) =
        world.GetEventSystemDelegateHook ()
        
    let private getEventSystemDelegateBy<'a, 'w when 'w :> EventSystem<'w>> (by : 'w EventSystemDelegate -> 'a) (world : 'w) : 'a =
        let propertySystem = world.GetEventSystemDelegateHook ()
        by propertySystem
        
    let private updateEventSystemDelegate<'w when 'w :> EventSystem<'w>> updater (world : 'w) =
        world.UpdateEventSystemDelegateHook updater

    /// Get event subscriptions.
    let getSubscriptions<'w when 'w :> EventSystem<'w>> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getSubscriptions world

    /// Get event unsubscriptions.
    let getUnsubscriptions<'w when 'w :> EventSystem<'w>> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getUnsubscriptions world

    /// Set event subscriptions.
    let private setSubscriptions<'w when 'w :> EventSystem<'w>> subscriptions (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setSubscriptions subscriptions) world

    /// Set event unsubscriptions.
    let private setUnsubscriptions<'w when 'w :> EventSystem<'w>> unsubscriptions (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setUnsubscriptions unsubscriptions) world

    /// Add event state to the world.
    let addEventState<'a, 'w when 'w :> EventSystem<'w>> key (state : 'a) (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.addEventState key state) world

    /// Remove event state from the world.
    let removeEventState<'w when 'w :> EventSystem<'w>> key (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.removeEventState key) world

    /// Get event state from the world.
    let getEventState<'a, 'g ,'w when 'w :> EventSystem<'w>> key (world : 'w) : 'a =
        getEventSystemDelegateBy (EventSystemDelegate.getEventState<'a, 'w> key) world

    /// Get whether events are being traced.
    let getEventTracing<'w when 'w :> EventSystem<'w>> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventTracing<'w> world

    /// Set whether events are being traced.
    let setEventTracing<'w when 'w :> EventSystem<'w>> tracing (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setEventTracing tracing) world

    /// Get the state of the event filter.
    let getEventFilter<'w when 'w :> EventSystem<'w>> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventFilter world

    /// Set the state of the event filter.
    let setEventFilter<'w when 'w :> EventSystem<'w>> filter (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.setEventFilter filter) world

    /// Get the event context of the world.
    let getEventContext<'w when 'w :> EventSystem<'w>> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventContext world

    /// Set the event context of the world.
    let setEventContext<'w when 'w :> EventSystem<'w>> context (world : 'w) =
        EventSystemDelegate.setEventContext context (getEventSystemDelegate world)

    /// Qualify the event context of the world.
    let qualifyEventContext<'w when 'w :> EventSystem<'w>> (address : obj Address) (world : 'w) =
        getEventSystemDelegateBy (EventSystemDelegate.qualifyEventContext address) world

    /// Set whether event addresses are cached internally.
    /// If you enable caching, be sure to use EventSystem.cleanEventAddressCache to keep the cache from expanding
    /// indefinitely.
    let setEventAddressCaching caching =
        if not caching then
            EventAddressCache.Clear ()
            EventAddressListCache.Clear ()
        EventAddressCaching <- caching

    /// Remove from the event address cache all addresses belonging to the given target.
    let cleanEventAddressCache (eventTarget : 'a Address) =
        if EventAddressCaching then
            let eventTargetOa = atooa eventTarget
            match EventAddressListCache.TryGetValue eventTargetOa with
            | (true, entries) ->
                for entry in entries do EventAddressCache.Remove entry |> ignore
                EventAddressListCache.Remove eventTargetOa |> ignore
            | (false, _) -> ()
        else ()

    // NOTE: event addresses are ordered from general to specific. This is so a generalized subscriber can preempt
    // any specific subscribers. Whether this is the best order is open for discussion.
    // OPTIMIZATION: imperative for speed
    let private getEventAddresses1 (eventAddress : 'a Address) =
        
        // create target event address array
        let eventAddressNames = Address.getNames eventAddress
        let eventAddressNamesLength = eventAddressNames.Length
        let eventAddresses = Array.zeroCreate (inc eventAddressNamesLength)

        // make non-wildcard address the last element
        eventAddresses.[eventAddressNamesLength] <- eventAddress

        // populate wildcard addresses from specific to general
        Array.iteri (fun i _ ->
            let eventAddressNamesAny = Array.zeroCreate eventAddressNamesLength
            Array.Copy (eventAddressNames, 0, eventAddressNamesAny, 0, eventAddressNamesLength)
            eventAddressNamesAny.[i] <- Address.head Events.Wildcard
            let eventAddressAny = Address.rtoa eventAddressNamesAny
            eventAddresses.[i] <- eventAddressAny)
            eventAddressNames

        // fin
        eventAddresses

    let private getEventAddresses2 (eventAddress : 'a Address) allowWildcard =
        if allowWildcard then
            if EventAddressCaching then
                match EventAddressCache.TryGetValue eventAddress with
                | (false, _) ->
                    let eventAddressNames = Address.getNames eventAddress
                    let eventAddresses = getEventAddresses1 eventAddress
                    let eventTargetIndex = Array.findIndex (fun name -> name = "Event") eventAddressNames + 1
                    if eventTargetIndex < Array.length eventAddressNames then
                        let eventTarget = eventAddressNames |> Array.skip eventTargetIndex |> Address.makeFromArray
                        match EventAddressListCache.TryGetValue eventTarget with
                        | (false, _) -> EventAddressListCache.Add (eventTarget, List [eventAddress :> obj]) |> ignore
                        | (true, list) -> list.Add eventAddress
                        EventAddressCache.Add (eventAddress, eventAddresses)
                    eventAddresses
                | (true, eventAddressesObj) -> eventAddressesObj :?> 'a Address array
            else getEventAddresses1 eventAddress
        else [|eventAddress|]

    let private boxSubscription<'a, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>>
        (subscription : Event<'a, 's> -> 'w -> Handling * 'w) =
        let boxableSubscription = fun (evt : Event<obj, Simulant>) world ->
            let evt =
                { Data = evt.Data :?> 'a
                  Subscriber = evt.Subscriber :?> 's
                  Publisher = evt.Publisher
                  Address = Address.specialize<'a> evt.Address
                  Trace = evt.Trace }
            subscription evt world
        boxableSubscription :> obj

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

    let private getSubscriptionsSorted (publishSorter : SubscriptionSorter<'w>) eventAddress allowWildcard (world : 'w) =
        let eventDeleage = getEventSystemDelegate world
        let eventSubscriptions = EventSystemDelegate.getSubscriptions eventDeleage
        let eventAddresses = getEventAddresses2 eventAddress allowWildcard
        let subscriptionOpts = Array.map (fun eventAddress -> UMap.tryFind eventAddress eventSubscriptions) eventAddresses
        let subscriptionOpts = Array.filter Option.isSome subscriptionOpts
        let subscriptions = Array.map Option.get subscriptionOpts // TODO: consider fusing with filter by using filterBy
        let subscriptions = Array.concat subscriptions
        publishSorter subscriptions world

    let private logEvent<'w when 'w :> EventSystem<'w>> eventAddress eventTrace (world : 'w) =
        EventSystemDelegate.logEvent<'w> eventAddress eventTrace (getEventSystemDelegate world)

    let private pushEventAddress<'w when 'w :> EventSystem<'w>> eventAddress (world : 'w) =
        updateEventSystemDelegate (EventSystemDelegate.pushEventAddress eventAddress) world

    let private popEventAddress<'w when 'w :> EventSystem<'w>> (world : 'w) =
        updateEventSystemDelegate EventSystemDelegate.popEventAddress world

    let private getEventAddresses<'w when 'w :> EventSystem<'w>> (world : 'w) =
        getEventSystemDelegateBy EventSystemDelegate.getEventAddresses world

    let getLiveness<'w when 'w :> EventSystem<'w>> (world : 'w) =
        world.GetLiveness ()

    let getGlobalSimulantSpecialized<'w when 'w :> EventSystem<'w>> (world : 'w) =
        world.GetGlobalSimulantSpecialized ()

    let getGlobalSimulantGeneralized<'w when 'w :> EventSystem<'w>> (world : 'w) =
        world.GetGlobalSimulantGeneralized ()

    let simulantExists<'w when 'w :> EventSystem<'w>> (simulant : Simulant) (world : 'w) =
        world.SimulantExists simulant

    /// Publish an event directly.
    let publishEvent<'a, 'p, 's, 'w when 'p :> Simulant and 's :> Simulant and 'w :> EventSystem<'w>>
        (subscriber : Simulant) (publisher : 'p) (eventData : obj) (eventAddress : 'a Address) eventTrace subscription (world : 'w) =
        let evt =
            { Data = eventData
              Subscriber = subscriber
              Publisher = publisher :> Simulant
              Address = atooa eventAddress
              Trace = eventTrace }
        let callableSubscription = unbox<'w BoxableSubscription> subscription
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
    let publishPlus<'a, 'p, 'w when 'p :> Simulant and 'w :> EventSystem<'w>>
        (publishSorter : SubscriptionSorter<'w>)
        (eventData : 'a)
        (eventAddress : 'a Address)
        eventTrace
        (publisher : 'p)
        allowWildcard
        (world : 'w) =
        let objEventAddress = atooa eventAddress in logEvent<'w> objEventAddress eventTrace world
        let subscriptions = getSubscriptionsSorted publishSorter objEventAddress allowWildcard world
        let (_, world) =
            // NOTE: inlined foldWhite here in order to compact the call stack
            let mutable lastState = (Cascade, world)
            let mutable stateOpt = Some lastState
            use mutable enr = (subscriptions :> _ seq).GetEnumerator ()
            while stateOpt.IsSome && enr.MoveNext () do
                lastState <- stateOpt.Value
                stateOpt <-
                    let (handling, world) = lastState
                    let subscription = enr.Current
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
                            if filtered
                            then world.PublishEventHook subscription.SubscriberEntry publisher mapped eventAddress eventTrace subscription.Callback world
                            else (Cascade, world)
#if DEBUG
                        let world = popEventAddress world
#endif
                        Some (handling, world)
                    else None
            match stateOpt with
            | Some state -> state
            | None -> lastState
        world

    /// Publish an event with no subscription sorting.
    let publish<'a, 'p, 'w when 'p :> Simulant and 'w :> EventSystem<'w>>
        (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publishPlus<'a, 'p, 'w> sortSubscriptionsNone eventData eventAddress eventTrace publisher true world

    /// Unsubscribe from an event.
    let unsubscribe<'w when 'w :> EventSystem<'w>> subscriptionKey (world : 'w) =
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
    let subscribePlus<'a, 'b, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>>
        (subscriptionKey : Guid)
        (mapperOpt : ('a -> 'b option -> 'w -> 'b) option)
        (filterOpt : ('b -> 'b option -> 'w -> bool) option)
        (stateOpt : 'b option)
        (subscription : Event<'b, 's> -> 'w -> Handling * 'w)
        (eventAddress : 'a Address) (subscriber : 's)
        (world : 'w) =
        if not (Address.isEmpty eventAddress) then
            let objEventAddress = atooa eventAddress
            let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
            let subscriptions =
                let subscriptionEntry =
                    { SubscriptionKey = subscriptionKey
                      SubscriberEntry = subscriber :> Simulant
                      MapperOpt = Option.map (fun mapper -> fun a p w -> mapper (a :?> 'a) (Option.map cast<'b> p) (w :?> 'w) :> obj) mapperOpt
                      FilterOpt = Option.map (fun filter -> fun b p w -> filter (b :?> 'b) (Option.map cast<'b> p) (w :?> 'w)) filterOpt
                      PreviousDataOpt = Option.map box stateOpt
                      Callback = boxSubscription subscription }
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
    let subscribe<'a, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>>
        (subscription : Event<'a, 's> -> 'w -> 'w) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribePlus (makeGuid ()) None None None (fun evt world -> (Cascade, subscription evt world)) eventAddress subscriber world |> snd

    /// Keep active a subscription for the life span of a simulant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 'b, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>>
        (mapperOpt : ('a -> 'b option -> 'w -> 'b) option)
        (filterOpt : ('b -> 'b option -> 'w -> bool) option)
        (stateOpt : 'b option)
        (subscription : Event<'b, 's> -> 'w -> Handling * 'w)
        (eventAddress : 'a Address)
        (subscriber : 's)
        (world : 'w) =
        let monitorKey = makeGuid ()
        let removalKey = makeGuid ()
        let world = subscribePlus<'a, 'b, 's, 'w> monitorKey mapperOpt filterOpt stateOpt subscription eventAddress subscriber world |> snd
        let unsubscribe = fun (world : 'w) ->
            let world = unsubscribe removalKey world
            let world = unsubscribe monitorKey world
            world
        let subscription' = fun _ eventSystem -> (Cascade, unsubscribe eventSystem)
        let removingEventAddress = rtoa<obj> [|"Unregistering"; "Event"|] --> subscriber.SimulantAddress
        let world = subscribePlus<obj, obj, Simulant, 'w> removalKey None None None subscription' removingEventAddress subscriber world |> snd
        (unsubscribe, world)

    /// Keep active a subscription for the life span of a simulant.
    let monitor<'a, 's, 'w when 's :> Simulant and 'w :> EventSystem<'w>>
        (subscription : Event<'a, 's> -> 'w -> 'w) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        monitorPlus<'a, 'a, 's, 'w> None None None (fun evt world -> (Cascade, subscription evt world)) eventAddress subscriber world |> snd
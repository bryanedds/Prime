// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections.Generic
open Prime

/// Specifies whether a process is live or dead.
type [<NoEquality; NoComparison; Struct>] Liveness =
   | Live
   | Dead

/// Describes whether an in-flight event has been resolved or should cascade to down-stream handlers.
type [<NoEquality; NoComparison; Struct>] Handling =
    | Resolve
    | Cascade

/// The generalized event type (can be used to handle any event).
type Event = Event<obj, Simulant>

/// An event used by the event system.
and [<NoEquality; NoComparison>] Event<'a, 's when 's :> Simulant> =
    { Data : 'a
      Subscriber : 's
      Publisher : Simulant
      Address : 'a Address
      Trace : EventTrace }

[<RequireQualifiedAccess>]
module Event =

    /// Specialize an event.
    let specialize<'a, 's when 's :> Simulant> (evt : Event) : Event<'a, 's> =
        { Data = evt.Data :?> 'a
          Subscriber = evt.Subscriber :?> 's
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }

    /// Generalize an event.
    let generalize (evt : Event<'a, 's>) : Event =
        { Data = evt.Data :> obj
          Subscriber = evt.Subscriber
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }

    /// Trace event information.
    let trace moduleName functionName eventTrace =
        EventTrace.trace moduleName functionName eventTrace

    /// Trace event information with greater detail.
    let trace4 moduleName functionName moreInfo eventTrace =
        EventTrace.trace4 moduleName functionName moreInfo eventTrace

type [<NoEquality; NoComparison>] Callback =
    | FunctionCallback of obj
    | UserDefinedCallback of obj

/// An entry in the subscription map.
type [<NoEquality; NoComparison>] SubscriptionEntry =
    { CompressionId : Guid
      SubscriptionId : Guid
      MapperOpt : (obj -> obj option -> obj -> obj) option // ('a -> 'b option -> 'w -> 'b) option
      FilterOpt : (obj -> obj option -> obj -> bool) option // ('b -> 'b option -> 'w -> bool) option
      mutable PreviousDataOpt : obj option // 'b option
      Callbacks : (Guid * Simulant * Callback) array } // TODO: consider using an FStack instead of an array here.

/// Abstracts over a subscription sorting procedure.
type SubscriptionSorter =
    (Guid * SubscriptionEntry) seq -> obj -> (Guid * SubscriptionEntry) seq

/// The type of a subscription.
type Callback<'a, 's, 'w when 's :> Simulant> =
    Event<'a, 's> -> 'w -> Handling * 'w

/// Describes an event subscription that can be boxed / unboxed.
type 'w BoxableSubscription =
    Event<obj, Simulant> -> 'w -> Handling * 'w

/// A map of event subscriptions.
type SubscriptionEntries =
    UMap<obj Address, OMap<Guid, SubscriptionEntry>>

/// A map of subscription keys to unsubscription data.
type UnsubscriptionEntries =
    UMap<Guid, obj Address * Simulant>

[<RequireQualifiedAccess>]
module Events =

    /// Represents a wildcard in an event.
    let Wildcard = ntoa<obj> "@"

[<RequireQualifiedAccess>]
module EventSystemDelegate =

    /// OPTIMIZATION: caches event address for fast wildcard address generation.
    let mutable private EventAddressCaching = false
    let private EventAddressCache = Dictionary<obj, obj> HashIdentity.Structural
    let private EventAddressListCache = Dictionary<obj Address, obj List> HashIdentity.Structural

    /// OPTIMIZATION: these were pulled out from EventSystemDelegate in order to fit its structure
    /// inside a cache line.
    let mutable private GlobalSimulantSpecialized : Simulant = Unchecked.defaultof<_>
    let mutable private GlobalSimulantGeneralized : GlobalSimulantGeneralized = Unchecked.defaultof<_>

    /// The implementation portion of EventSystem.
    type [<ReferenceEquality; NoComparison>] 'w EventSystemDelegate =
        private
            { // cache line begin
              Subscriptions : SubscriptionEntries
              Unsubscriptions : UnsubscriptionEntries
              EventStates : UMap<Guid, obj>
              EventTracerOpt : (string -> unit) option
              EventFilter : EventFilter.Filter }
              // 12 free cache line bytes here

    /// Add event state.
    let addEventState<'a, 'w> key (state : 'a) (esd : 'w EventSystemDelegate) =
        { esd with EventStates = UMap.add key (state :> obj) esd.EventStates }

    /// Remove event state.
    let removeEventState<'w> key (esd : 'w EventSystemDelegate) =
        { esd with EventStates = UMap.remove key esd.EventStates }

    /// Get subscriptions.
    let getSubscriptions<'w> (esd : 'w EventSystemDelegate) =
        esd.Subscriptions

    /// Get unsubscriptions.
    let getUnsubscriptions<'w> (esd : 'w EventSystemDelegate) =
        esd.Unsubscriptions

    /// Set subscriptions.
    let internal setSubscriptions<'w> subscriptions (esd : 'w EventSystemDelegate) =
        { esd with Subscriptions = subscriptions }

    /// Set unsubscriptions.
    let internal setUnsubscriptions<'w> unsubscriptions (esd : 'w EventSystemDelegate) =
        { esd with Unsubscriptions = unsubscriptions }

    /// Get event state.
    let getEventState<'a, 'w> key (esd : 'w EventSystemDelegate) =
        let state = UMap.find key esd.EventStates
        state :?> 'a

    /// Get how events are being traced.
    let getEventTracerOpt<'w> (esd : 'w EventSystemDelegate) =
        esd.EventTracerOpt

    /// Set how events are being traced.
    let setEventTracerOpt<'w> tracing (esd : 'w EventSystemDelegate) =
        { esd with EventTracerOpt = tracing }

    /// Get the state of the event filter.
    let getEventFilter<'w> (esd : 'w EventSystemDelegate) =
        esd.EventFilter

    /// Set the state of the event filter.
    let setEventFilter<'w> filter (esd : 'w EventSystemDelegate) =
        { esd with EventFilter = filter }

    /// Get the specialized global simulant of the event system.
    let getGlobalSimulantSpecialized (_ : 'w EventSystemDelegate) =
        GlobalSimulantSpecialized

    /// Get the generalized global simulant of the event system.
    let getGlobalSimulantGeneralized (_ : 'w EventSystemDelegate) =
        GlobalSimulantGeneralized

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
    let getEventAddresses1 (eventAddress : 'a Address) =

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

    /// Get the wild-carded addresses of an event address.
    let getEventAddresses2 (eventAddress : 'a Address) (_ : 'w EventSystemDelegate) =
        if EventAddressCaching then
            match EventAddressCache.TryGetValue eventAddress with
            | (false, _) ->
                let eventAddressNames = Address.getNames eventAddress
                let eventAddresses = getEventAddresses1 eventAddress
                match Array.tryFindIndex (fun name -> name = "Event") eventAddressNames with
                | Some eventIndex ->
                    let eventTargetIndex = inc eventIndex
                    if eventTargetIndex < Array.length eventAddressNames then
                        let eventTarget = eventAddressNames |> Array.skip eventTargetIndex |> Address.makeFromArray
                        match EventAddressListCache.TryGetValue eventTarget with
                        | (false, _) -> EventAddressListCache.Add (eventTarget, List [eventAddress :> obj]) |> ignore
                        | (true, list) -> list.Add eventAddress
                        EventAddressCache.Add (eventAddress, eventAddresses)
                    eventAddresses
                | None ->
                    failwith
                        ("The event address '" + scstring eventAddress +
                         "' is missing the 'Event' name. All event addresses must separate the event names from the publisher names with 'Event', " +
                         "like 'Click/Event/Button', or 'Mouse/Left/Down/Event' if there is no publisher.")
            | (true, eventAddressesObj) -> eventAddressesObj :?> 'a Address array
        else getEventAddresses1 eventAddress

    /// Get subscriptions for eventAddress sorted by publishSorter.
    let getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress (esd : 'w EventSystemDelegate) (world : 'w) =
        let eventSubscriptions = getSubscriptions esd
        let eventAddresses = getEventAddresses2 eventAddress esd
        let subscriptionOpts = Array.map (fun eventAddress -> UMap.tryFind eventAddress eventSubscriptions) eventAddresses
        let subscriptions = subscriptionOpts |> Array.definitize |> Array.map OMap.toSeq |> Seq.concat
        publishSorter subscriptions world

    /// Log an event.
    let logEvent<'w> (address : obj Address) (trace : EventTrace) (esd : 'w EventSystemDelegate) =
        match esd.EventTracerOpt with
        | Some tracer ->
            let addressStr = scstring address
            let traceRev = List.rev trace // for efficiency during normal execution, trace is cons'd up into a reversed list
            if EventFilter.filter addressStr traceRev esd.EventFilter then tracer (addressStr + "|" + scstring traceRev)
        | None -> ()

    /// Make an event delegate.
    let make eventTracerOpt eventFilter globalSimulantSpecialized globalSimulantGeneralized =
        let esd =
            { Subscriptions = UMap.makeEmpty Functional
              Unsubscriptions = UMap.makeEmpty Functional
              EventStates = UMap.makeEmpty Functional
              EventTracerOpt = eventTracerOpt
              EventFilter = eventFilter }
        GlobalSimulantSpecialized <- globalSimulantSpecialized
        GlobalSimulantGeneralized <- globalSimulantGeneralized
        esd

/// The implementation portion of EventSystem.
type 'w EventSystemDelegate = 'w EventSystemDelegate.EventSystemDelegate
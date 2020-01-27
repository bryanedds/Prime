// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open Prime

/// Describes whether an in-flight event has been resolved or should cascade to down-stream handlers.
type Handling =
    | Resolve
    | Cascade

/// Specifies whether an event-based application is running or exiting.
type Liveness =
    | Running
    | Exiting

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

    // Generalize an event.
    let generalize (evt : Event<'a, 's>) : Event =
        { Data = evt.Data :> obj
          Subscriber = evt.Subscriber
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }

/// An entry in the subscription map.
type [<NoEquality; NoComparison>] SubscriptionEntry =
    { SubscriptionKey : Guid
      SubscriberEntry : Simulant
      Callback : obj }

/// Abstracts over a subscription sorting procedure.
type 'w SubscriptionSorter =
    SubscriptionEntry array -> 'w -> SubscriptionEntry array

/// Describes an event subscription that can be boxed / unboxed.
type 'w BoxableSubscription =
    obj -> obj -> 'w -> Handling * 'w

/// A map of event subscriptions.
type SubscriptionEntries =
    UMap<obj Address, SubscriptionEntry array>

/// A map of subscription keys to unsubscription data.
type UnsubscriptionEntries =
    UMap<Guid, obj Address * Simulant>

module Events =

    /// Represents a wildcard in an event.
    let Wildcard = ntoa<obj> "@"

[<AutoOpen>]
module EventSystemDelegate =

    /// OPTIMIZATION: these were pulled out from EventSystemDelegate in order to fit its structure
    /// inside a cache line.
    let mutable GlobalSimulantSpecialized : Simulant = Unchecked.defaultof<_>
    let mutable GlobalSimulantGeneralized : GlobalSimulantGeneralized = Unchecked.defaultof<_>

    /// The implementation portion of EventSystem.
    /// OPTIMIZATION: EventContext is mutable for speed.
    type [<ReferenceEquality>] 'w EventSystemDelegate =
        private
            { // cache line begin
              Subscriptions : SubscriptionEntries
              Unsubscriptions : UnsubscriptionEntries
              mutable EventContext : Simulant
              EventStates : UMap<Guid, obj>
              EventTracer : string -> unit
              EventTracing : bool
              // 2 free cache line bytes here
              EventFilter : EventFilter.Filter
              EventAddresses : obj Address list }
              // cache line end

    [<RequireQualifiedAccess>]
    module EventSystemDelegate =

        /// The TConfig of Xtension's T/U structures.
        let Config = Functional

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

        /// Get whether events are being traced.
        let getEventTracing<'w> (esd : 'w EventSystemDelegate) =
            esd.EventTracing

        /// Set whether events are being traced.
        let setEventTracing<'w> tracing (esd : 'w EventSystemDelegate) =
            { esd with EventTracing = tracing }

        /// Get the state of the event filter.
        let getEventFilter<'w> (esd : 'w EventSystemDelegate) =
            esd.EventFilter

        /// Set the state of the event filter.
        let setEventFilter<'w> filter (esd : 'w EventSystemDelegate) =
            { esd with EventFilter = filter }

        /// Get the context of the event system.
        let getEventContext (esd : 'w EventSystemDelegate) =
            esd.EventContext

        /// Get the specialized global simulant of the event system.
        let getGlobalSimulantSpecialized (_ : 'w EventSystemDelegate) =
            GlobalSimulantSpecialized

        /// Get the generalized global simulant of the event system.
        let getGlobalSimulantGeneralized (_ : 'w EventSystemDelegate) =
            GlobalSimulantGeneralized

        /// Qualify the event context of the world.
        let qualifyEventContext (address : obj Address) (esd : 'w EventSystemDelegate) =
            let context = getEventContext esd
            let contextAddress = context.SimulantAddress
            let contextAddressLength = Address.length contextAddress
            let addressLength = Address.length address
            if contextAddressLength = addressLength then
                Address.tryTake (contextAddressLength - 1) contextAddress =
                    Address.tryTake (addressLength - 1) address
            elif contextAddressLength < addressLength then
                contextAddress = Address.take contextAddressLength address
            elif contextAddressLength > addressLength then
                address = Address.take addressLength contextAddress
            else false

        /// Set the context of the event context.
        let setEventContext context (esd : 'w EventSystemDelegate) =
            esd.EventContext <- context

        /// Log an event.
        let logEvent<'w> (address : obj Address) (trace : EventTrace) (esd : 'w EventSystemDelegate) =
            if esd.EventTracing then
                let addressStr = scstring address
                let traceRev = List.rev trace // for efficiency during normal execution, trace is cons'd up into a reversed list
                if EventFilter.filter addressStr traceRev esd.EventFilter then
                    esd.EventTracer (addressStr + "|" + scstring traceRev)

        /// Push an event address to the list for cycle-detection.
        let pushEventAddress<'w> eventAddress (esd : 'w EventSystemDelegate) =
            { esd with EventAddresses = eventAddress :: esd.EventAddresses }
            
        /// Pop an event address to the list for cycle-detection.
        let popEventAddress<'w> (esd : 'w EventSystemDelegate) =
            { esd with EventAddresses = List.tail esd.EventAddresses }
            
        /// Get the current event address list for cycle-detection.
        let getEventAddresses<'w> (esd : 'w EventSystemDelegate) =
            esd.EventAddresses

        /// Make an event delegate.
        let make eventTracer eventTracing eventFilter globalSimulantSpecialized globalSimulantGeneralized =
            let esd =
                { Subscriptions = UMap.makeEmpty Config
                  Unsubscriptions = UMap.makeEmpty Config
                  EventContext = globalSimulantSpecialized
                  EventStates = UMap.makeEmpty Config
                  EventTracer = eventTracer
                  EventTracing = eventTracing
                  EventFilter = eventFilter
                  EventAddresses = [] }
            GlobalSimulantSpecialized <- globalSimulantSpecialized
            GlobalSimulantGeneralized <- globalSimulantGeneralized
            esd

/// The implementation portion of EventSystem.
type 'w EventSystemDelegate = 'w EventSystemDelegate.EventSystemDelegate
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open Prime

/// Handles participant property changes.
type PropertyChangeHandler<'w when 'w :> PropertySystem<'w>> = 'w -> 'w -> 'w

/// Detaches a participant property change handler.
and PropertyChangeUnhandler<'w when 'w :> PropertySystem<'w>> = 'w -> 'w

/// A participant in the observable-property system.
and Participant =
    interface
        abstract member ParticipantAddress : Participant Address
        end

/// Operators for the Participant type.
and ParticipantOperators =
    private
        | ParticipantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (participant : Participant) = acatf address (atooa participant.ParticipantAddress)

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, participant : Participant) = ParticipantOperators.acatf address participant

/// An observable-property system.
and PropertySystem<'w when 'w :> PropertySystem<'w>> =
    abstract member GetPropertyOpt<'a> : string -> Participant -> 'a option
    abstract member SetPropertyOpt<'a> : string -> Participant -> 'a option -> 'w
    abstract member HandlePropertyChange : string -> Participant -> PropertyChangeHandler<'w> -> PropertyChangeUnhandler<'w> * 'w

[<RequireQualifiedAccess>]
module PropertySystem =

    let getPropertyOpt<'w when 'w :> PropertySystem<'w>> (propertyName : string) (participant : Participant) (propertySystem : 'w) =
        propertySystem.GetPropertyOpt propertyName participant

    let setPropertyOpt<'a, 'w when 'w :> PropertySystem<'w>> (propertyName : string) (participant : Participant) (valueOpt : 'a option) (propertySystem : 'w) =
        propertySystem.SetPropertyOpt propertyName participant valueOpt

    let handlePropertyChange<'w when 'w :> PropertySystem<'w>> (propertyName : string) (participant : Participant) handler (propertySystem : 'w) =
        propertySystem.HandlePropertyChange propertyName participant handler
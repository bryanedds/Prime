// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open Prime

/// Handles participant property changes.
type PropertyChangeHandler<'w when 'w :> EventSystem<'w>> = 'w -> 'w -> 'w

/// Detaches a participant property change handler.
and PropertyChangeUnhandler<'w when 'w :> EventSystem<'w>> = 'w -> 'w

/// An observable-property system.
and EventSystem<'w when 'w :> EventSystem<'w>> =
    abstract member ParticipantExists : Participant -> bool
    abstract member GetPropertyOpt<'a> : string -> Participant -> 'a option
    abstract member SetPropertyOpt<'a> : string -> Participant -> 'a option -> 'w
    abstract member HandlePropertyChange : string -> Participant -> PropertyChangeHandler<'w> -> PropertyChangeUnhandler<'w> * 'w

[<RequireQualifiedAccess>]
module EventSystem =

    let participantExists<'w when 'w :> EventSystem<'w>> (participant : Participant) (eventSystem : 'w) =
        eventSystem.ParticipantExists participant

    let getPropertyOpt<'w when 'w :> EventSystem<'w>> (propertyName : string) (participant : Participant) (eventSystem : 'w) =
        eventSystem.GetPropertyOpt propertyName participant

    let setPropertyOpt<'a, 'w when 'w :> EventSystem<'w>> (propertyName : string) (participant : Participant) (valueOpt : 'a option) (eventSystem : 'w) =
        eventSystem.SetPropertyOpt propertyName participant valueOpt

    let handlePropertyChange<'w when 'w :> EventSystem<'w>> (propertyName : string) (participant : Participant) handler (eventSystem : 'w) =
        eventSystem.HandlePropertyChange propertyName participant handler
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open Prime

/// A participant in the observable-property system.
type Participant =
    interface
        inherit Propertied
        abstract member ParticipantAddress : Participant Address
        end

/// Operators for the Participant type.
type ParticipantOperators =
    private
        | ParticipantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (participant : Participant) = acatf address (atooa participant.ParticipantAddress)

    /// Concatenate two addresses, forcing the type of first address.
    static member acatff<'a> (address : 'a Address) (participant : Participant) = acatff address participant.ParticipantAddress

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, participant : Participant) = ParticipantOperators.acatf address participant

    /// Concatenate two addresses, forcing the type of first address.
    static member (-->) (address, participant : Participant) = ParticipantOperators.acatff address participant

/// The data for a change in a participant.
type [<StructuralEquality; NoComparison>] ChangeData =
    { Name : string
      Value : obj }

/// A participant in the event system that is globalized and compatible with generalized events.
type [<StructuralEquality; NoComparison>] GlobalParticipantGeneralized =
    { GpgAddress : GlobalParticipantGeneralized Address }
    interface Participant with
        member this.ParticipantAddress = atoa<GlobalParticipantGeneralized, Participant> this.GpgAddress
        end
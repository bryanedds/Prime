// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open Prime

/// A participant in the observable-property system.
type Simulant =
    interface
        inherit Propertied
        abstract member SimulantAddress : Simulant Address
        end

/// Operators for the Simulant type.
type SimulantOperators =
    private
        | SimulantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (simulant : Simulant) = acatf address (atooa simulant.SimulantAddress)

    /// Concatenate two addresses, forcing the type of first address.
    static member acatff<'a> (address : 'a Address) (simulant : Simulant) = acatff address simulant.SimulantAddress

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, simulant : Simulant) = SimulantOperators.acatf address simulant

    /// Concatenate two addresses, forcing the type of first address.
    static member (-->) (address, simulant : Simulant) = SimulantOperators.acatff address simulant

/// The data for a change in a simulant.
type [<StructuralEquality; NoComparison>] ChangeData =
    { Name : string
      Value : obj }

/// A simulant in the event system that is globalized and compatible with generalized events.
type [<StructuralEquality; NoComparison>] GlobalSimulantGeneralized =
    { GpgAddress : GlobalSimulantGeneralized Address }
    interface Simulant with
        member this.SimulantAddress = atoa<GlobalSimulantGeneralized, Simulant> this.GpgAddress
        end
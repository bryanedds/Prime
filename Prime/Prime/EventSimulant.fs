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

[<AutoOpen>]
module SimulantOperators =

    /// Operators for the Simulant type.
    type Simulant with
        
        /// Concatenate an address with a simulant's address, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (simulant : Simulant) =
            match box simulant with
            | null -> address // HACK: this case is a hack to be able to insert events into an elmish event handler
            | _ -> acatff address simulant.SimulantAddress

        /// Concatenate an address with a simulant's address, forcing the type of first address.
        // Disabled due to extension types not supporting operators: static member (-->) (address, simulant : Simulant) = Simulant.acatff address simulant

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
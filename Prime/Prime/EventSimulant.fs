﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open Prime

/// A participant in the observable-property system.
type Simulant =
    interface
        abstract member SimulantAddress : Simulant Address
        end

[<AutoOpen>]
module SimulantOperators =

    /// Operators for the Simulant type.
    type Simulant with

        /// The names of the simulant.
        member this.Names = this.SimulantAddress.Names
        
        /// Concatenate an address with a simulant's address, forcing the type of first address.
        static member acatff<'a> (address : 'a Address) (simulant : Simulant) =
            match box simulant with
            | null -> address // HACK: this case is a hack to be able to insert events into an elmish event handler
            | _ -> acatff address simulant.SimulantAddress

        /// Concatenate an address with a simulant's address, forcing the type of first address.
        // Disabled due to extension types not supporting operators: static member (-->) (address, simulant : Simulant) = Simulant.acatff address simulant

/// The data for a change in a simulant.
type ChangeData =
    { Name : string
      Previous : obj
      Value : obj }

/// A simulant in the event system that is globalized and compatible with generalized events.
type GlobalSimulantGeneralized =
    { GsgAddress : GlobalSimulantGeneralized Address }
    interface Simulant with
        member this.SimulantAddress = atoa<GlobalSimulantGeneralized, Simulant> this.GsgAddress
        end
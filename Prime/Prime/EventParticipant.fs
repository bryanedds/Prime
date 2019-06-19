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
type [<Struct; StructuralEquality; NoComparison>] 'w ParticipantChangeData =
    { PropertyName : string
      OldWorld : 'w }

/// A participant in the event system that is globalized and compatible with generalized events.
type [<StructuralEquality; NoComparison>] GlobalParticipantGeneralized =
    { GpgAddress : GlobalParticipantGeneralized Address }
    interface Participant with
        member this.ParticipantAddress = atoa<GlobalParticipantGeneralized, Participant> this.GpgAddress
        end

/// A generalized property tag.
type 'w PropertyTag =
    interface
        abstract This : Participant
        abstract Name : string
        abstract Get : 'w -> obj
        abstract SetOpt : (obj -> 'w -> 'w) option
        abstract Type : Type
        end

/// Describes a property of a participant.
/// Similar to a Haskell lens, but specialized to properties.
type [<NoEquality; NoComparison>] PropertyTag<'a, 'w> =
    { This : Participant
      Name : string
      Get : 'w -> 'a
      SetOpt : ('a -> 'w -> 'w) option }

    interface 'w PropertyTag with
        member this.This = this.This
        member this.Name = this.Name
        member this.Get world = this.Get world :> obj
        member this.SetOpt = Option.map (fun set -> fun (value : obj) world -> set (value :?> 'a) world) this.SetOpt
        member this.Type = typeof<'a>

    member this.Generalize () =
        let this = this :> 'w PropertyTag
        { This = this.This
          Name = this.Name
          Get = this.Get
          SetOpt = this.SetOpt}

    member this.GetBy by world =
        by (this.Get world)

    member this.TrySet value world =
        match this.SetOpt with
        | Some setter -> (true, setter value world)
        | None -> (false, world)
      
    member this.Set value world =
        match this.TrySet value world with
        | (true, world) -> world
        | (false, _) -> failwith ("PropertyTag for '" + this.Name + "' is readonly.")

    member this.TryUpdateEffect updater world =
        let value = this.Get world
        let (value', world) = updater value world
        this.TrySet value' world

    member this.TryUpdateWorld updater world =
        let value = this.Get world
        let value' = updater value world
        this.TrySet value' world

    member this.TryUpdate updater world =
        this.TryUpdateWorld (fun value _ -> updater value) world

    member this.UpdateEffect updater world =
        match this.TryUpdateEffect updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.UpdateWorld updater world =
        match this.TryUpdateWorld updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.Update updater world =
        match this.TryUpdate updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.Map mapper : PropertyTag<'a, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (mapper value)) | None -> None }

    member this.Map2 mapper unmapper : PropertyTag<'b, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (unmapper value)) | None -> None }

    member this.MapOut mapper : PropertyTag<'b, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = None }

    member this.ChangeEvent =
        let changeEventAddress = Address<'w ParticipantChangeData>.ltoa ["Change"; this.Name; "Event"]
        let changeEvent = changeEventAddress --> this.This.ParticipantAddress
        changeEvent

    member this.Type =
        typeof<'a>

[<RequireQualifiedAccess>]
module PropertyTag =

    let map mapper (property : PropertyTag<_, _>) =
        property.Map mapper

    let map2 mapper unmapper (property : PropertyTag<_, _>) =
        property.Map2 mapper unmapper

    let mapOut mapper (property : PropertyTag<_, _>) =
        property.MapOut mapper

    let makeReadOnly this name get =
        { This = this; Name = name; Get = get; SetOpt = None }

    let make this name get set =
        { This = this; Name = name; Get = get; SetOpt = Some set }

[<AutoOpen>]
module PropertyTagOperators =

    let define (tag : PropertyTag<'a, 'w>) (value : 'a) =
        PropertyDefinition.makeValidated tag.Name typeof<'a> (DefineExpr value)

    let variable (tag : PropertyTag<'a, 'w>) (variable : 'w -> 'a) =
        PropertyDefinition.makeValidated tag.Name typeof<'a> (VariableExpr (fun world -> variable (world :?> 'w) :> obj))

    // NOTE: failed lensing experiment - can't get the damn types to work out...
    //
    //let inline addEqual<'a, 'w when 'a : (static member (+) : 'a * 'a -> 'a)> (property : PropertyTag<'a, 'w>) (value : 'a) (world : 'w) : 'w =
    //    match property.SetOpt with
    //    | Some set -> set (property.Get world + value) world // NOTE: for some reason, F# is not forwarding the 'a constraint to the Get member...
    //    | None -> world
    //
    //let inline (+=) (left, right) = addEqual left right
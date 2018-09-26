// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open Prime

/// A participant in the event system.
type Participant =
    interface
        abstract member ParticipantAddress : Participant Address
        end

/// Operators for the Participant type.
type ParticipantOperators =
    private
        | ParticipantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (participant : Participant) = acatf address (atooa participant.ParticipantAddress)

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, participant : Participant) = ParticipantOperators.acatf address participant

/// The data for a change in a participant.
type [<Struct; StructuralEquality; NoComparison>] 'w ParticipantChangeData =
    { PropertyName : string
      OldWorld : 'w }

/// Describes a property of a participant.
/// Similar to a Haskell lens, but specialized to properties.
type [<NoEquality; NoComparison>] PropertyTag<'a, 'w> =
    { This : Participant
      Name : string
      Get : 'w -> 'a
      SetOpt : ('a -> 'w -> 'w) option }
      
    member this.TrySet value world =
        match this.SetOpt with
        | Some setter -> (true, setter value world)
        | None -> (false, world)
      
    member this.Set value world =
        match this.TrySet value world with
        | (true, world) -> world
        | (false, _) -> failwith ("PropertyTag for '" + this.Name + "' is readonly.")

    member this.TryUpdateWorld updater world =
        let value = this.Get world
        let value' = updater value world
        this.TrySet value' world

    member this.TryUpdate updater world =
        this.TryUpdateWorld (fun value _ -> updater value) world

    member this.UpdateWorld updater world =
        match this.TryUpdateWorld updater world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.Update updater world =
        this.UpdateWorld (fun value _ -> updater value) world

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
        let changeEvent = changeEventAddress ->>- this.This.ParticipantAddress
        changeEvent

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
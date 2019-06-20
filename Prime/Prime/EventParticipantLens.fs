// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open Prime

/// A generalized participant lens.
type 'w Lens =
    interface
        abstract This : Participant
        abstract Name : string
        abstract Get : 'w -> obj
        abstract SetOpt : (obj -> 'w -> 'w) option
        abstract Type : Type
        end

/// Describes a property of a participant.
/// Similar to a Haskell lens, but specialized to participant properties.
type [<NoEquality; NoComparison>] Lens<'a, 'w> =
    { This : Participant
      Name : string
      Get : 'w -> 'a
      SetOpt : ('a -> 'w -> 'w) option }

    interface 'w Lens with
        member this.This = this.This
        member this.Name = this.Name
        member this.Get world = this.Get world :> obj
        member this.SetOpt = Option.map (fun set -> fun (value : obj) world -> set (value :?> 'a) world) this.SetOpt
        member this.Type = typeof<'a>

    member this.Generalize () =
        let this = this :> 'w Lens
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

    member this.Map mapper : Lens<'a, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (mapper value)) | None -> None }

    member this.Map2 mapper unmapper : Lens<'b, 'w> =
        { This = this.This
          Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (unmapper value)) | None -> None }

    member this.MapOut mapper : Lens<'b, 'w> =
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

    (* Lensing Operators *)
    static member inline ( += ) (lens : Lens<_, _>, value) =  lens.Update (flip (+) value)
    static member inline ( -= ) (lens : Lens<_, _>, value) =  lens.Update (flip (-) value)
    static member inline ( *= ) (lens : Lens<_, _>, value) =  lens.Update (flip (*) value)
    static member inline ( /= ) (lens : Lens<_, _>, value) =  lens.Update (flip (/) value)
    static member inline ( %= ) (lens : Lens<_, _>, value) =  lens.Update (flip (%) value)
    static member inline (+)    (lens : Lens<_, _>, value) =  lens.GetBy  (flip (+) value)
    static member inline (-)    (lens : Lens<_, _>, value) =  lens.GetBy  (flip (-) value)
    static member inline (*)    (lens : Lens<_, _>, value) =  lens.GetBy  (flip (*) value)
    static member inline (/)    (lens : Lens<_, _>, value) =  lens.GetBy  (flip (/) value)
    static member inline (%)    (lens : Lens<_, _>, value) =  lens.GetBy  (flip (%) value)
    static member inline ( ** ) (lens : Lens<_, _>, value) =  lens.GetBy  (flip ( ** ) value)
    static member inline (<<<)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (<<<) value)
    static member inline (>>>)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (>>>) value)
    static member inline (&&&)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (&&&) value)
    static member inline (|||)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (|||) value)
    static member inline (^^^)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (^^^) value)
    static member inline (=.)   (lens : Lens<_, _>, value) =  lens.GetBy  (flip (=) value)
    static member inline (<>.)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (<>) value)
    static member inline (<.)   (lens : Lens<_, _>, value) =  lens.GetBy  (flip (<) value)
    static member inline (<=.)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (<=) value)
    static member inline (>.)   (lens : Lens<_, _>, value) =  lens.GetBy  (flip (>) value)
    static member inline (>=.)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (>=) value)
    static member inline (&&.)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (&&) value)
    static member inline (||.)  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (||) value)
    static member inline (.[])  (lens : Lens<_, _>, value) =  lens.GetBy  (flip (.[]) value)
    static member inline (~+)   (lens : Lens<_, _>) =         lens.Update (~+)
    static member inline (~-)   (lens : Lens<_, _>) =         lens.Update (~-)
    static member inline (!+)   (lens : Lens<_, _>) =         lens.Update inc
    static member inline (!-)   (lens : Lens<_, _>) =         lens.Update dec
    static member inline (~~~)  (lens : Lens<_, _>) =         lens.GetBy  (~~~)
    static member inline (-->)  (lens : Lens<_, _>, mapper) = lens.MapOut mapper
    static member inline (<--)  (lens : Lens<_, _>, value) =  lens.Set value
    static member inline (!.)   (lens : Lens<_, _>) =         lens.Get

[<RequireQualifiedAccess>]
module Lens =

    let map mapper (lens : Lens<_, _>) =
        lens.Map mapper

    let map2 mapper unmapper (lens : Lens<_, _>) =
        lens.Map2 mapper unmapper

    let mapOut mapper (lens : Lens<_, _>) =
        lens.MapOut mapper

    let makeReadOnly this name get =
        { This = this; Name = name; Get = get; SetOpt = None }

    let make this name get set =
        { This = this; Name = name; Get = get; SetOpt = Some set }

[<AutoOpen>]
module LensOperators =

    let define (lens : Lens<'a, 'w>) (value : 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (DefineExpr value)

    let variable (lens : Lens<'a, 'w>) (variable : 'w -> 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (VariableExpr (fun world -> variable (world :?> 'w) :> obj))
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System
open Prime

/// A generalized participant lens.
type 'w Lens =
    interface
        abstract Name : string
        abstract This : Participant
        abstract Get : 'w -> obj
        abstract SetOpt : (obj -> 'w -> 'w) option
        abstract Type : Type
        abstract ChangeEvent : ChangeData Address
        end

/// Describes a property of a participant.
/// Similar to a Haskell lens, but specialized to participant properties.
type [<NoEquality; NoComparison>] Lens<'a, 'w> =
    { Name : string
      Get : 'w -> 'a
      SetOpt : ('a -> 'w -> 'w) option
      This : Participant }

    interface 'w Lens with
        member this.Name = this.Name
        member this.Get world = this.Get world :> obj
        member this.SetOpt = Option.map (fun set -> fun (value : obj) world -> set (value :?> 'a) world) this.SetOpt
        member this.This = this.This
        member this.Type = typeof<'a>
        member this.ChangeEvent = this.ChangeEvent

    member this.Generalize () =
        let this = this :> 'w Lens
        { Name = this.Name
          Get = this.Get
          SetOpt = this.SetOpt
          This = this.This }

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
        { Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (mapper value)) | None -> None
          This = this.This }

    member this.Map2 mapper unmapper : Lens<'b, 'w> =
        { Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (unmapper value)) | None -> None
          This = this.This }

    member this.MapOut mapper : Lens<'b, 'w> =
        { Name = this.Name
          Get = fun world -> mapper (this.Get world)
          SetOpt = None
          This = this.This }

    member this.MapWorld mapper : Lens<'b, 'w> =
        { Name = this.Name
          Get = fun world -> mapper (this.Get world) world
          SetOpt = None
          This = this.This }

    member this.ChangeEvent =
        let changeEventAddress = rtoa<ChangeData> [|"Change"; this.Name; "Event"|]
        let changeEvent = changeEventAddress --> this.This.ParticipantAddress
        changeEvent

    member this.Type =
        typeof<'a>

    (* Lensing Operators *)
    static member inline ( += ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (+) value)
    static member inline ( -= ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (-) value)
    static member inline ( *= ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (*) value)
    static member inline ( /= ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (/) value)
    static member inline ( %= ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (%) value)
    static member inline (+)    (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (+) value)
    static member inline (-)    (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (-) value)
    static member inline (*)    (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (*) value)
    static member inline (/)    (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (/) value)
    static member inline (%)    (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (%) value)
    static member inline ( ** ) (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip ( ** ) value)
    static member inline (<<<)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (<<<) value)
    static member inline (>>>)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (>>>) value)
    static member inline (&&&)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (&&&) value)
    static member inline (|||)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (|||) value)
    static member inline (^^^)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (^^^) value)
    static member inline (=.)   (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (=) value)
    static member inline (<>.)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (<>) value)
    static member inline (<.)   (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (<) value)
    static member inline (<=.)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (<=) value)
    static member inline (>.)   (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (>) value)
    static member inline (>=.)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (>=) value)
    static member inline (&&.)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (&&) value)
    static member inline (||.)  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (||) value)
    static member inline (.[])  (lens : Lens<_, 'w>, value) =  lens.GetBy  (flip (.[]) value)
    static member inline (~+)   (lens : Lens<_, 'w>) =         lens.Update (~+)
    static member inline (~-)   (lens : Lens<_, 'w>) =         lens.Update (~-)
    static member inline (!+)   (lens : Lens<_, 'w>) =         lens.Update inc
    static member inline (!-)   (lens : Lens<_, 'w>) =         lens.Update dec
    static member inline (~~~)  (lens : Lens<_, 'w>) =         lens.GetBy  (~~~)

    /// Map over a lens in the given world context (read-only).
    static member inline (->>)  (lens : Lens<_, 'w>, mapper) = lens.MapWorld mapper

    /// Map over a lens (read-only).
    static member inline (-->)  (lens : Lens<_, 'w>, mapper) = lens.MapOut mapper

    /// Map over a lens (both read and write).
    static member inline (<^>)  (lens : Lens<_, 'w>, mapper) = lens.Map mapper

    /// Set a lensed property.
    static member inline (<--)  (lens : Lens<_, 'w>, value) =  lens.Set value

    /// Get a lensed property.
    static member inline (!.)   (lens : Lens<_, 'w>) =         lens.Get

[<RequireQualifiedAccess>]
module Lens =

    let map<'a, 'w> mapper (lens : Lens<'a, 'w>) =
        lens.Map mapper

    let map2<'a, 'b, 'w> mapper unmapper (lens : Lens<'a, 'w>) : Lens<'b, 'w> =
        lens.Map2 mapper unmapper

    let mapOut<'a, 'w> mapper (lens : Lens<'a, 'w>) =
        lens.MapOut mapper

    let makeReadOnly<'a, 'w> name get this : Lens<'a, 'w> =
        { Name = name; Get = get; SetOpt = None; This = this }

    let make<'a, 'w> name get set this : Lens<'a, 'w> =
        { Name = name; Get = get; SetOpt = Some set; This = this }

[<AutoOpen>]
module LensOperators =

    let define (lens : Lens<'a, 'w>) (value : 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (DefineExpr value)

    let variable (lens : Lens<'a, 'w>) (variable : 'w -> 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (VariableExpr (fun world -> variable (world :?> 'w) :> obj))
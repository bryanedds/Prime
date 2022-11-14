// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open Prime

/// A generalized simulant lens.
type 'w Lens =
    interface
        abstract Name : string
        abstract Get : Simulant -> 'w -> obj
        abstract SetOpt : (obj -> Simulant -> 'w -> 'w) voption
        abstract TrySet : obj -> Simulant -> 'w -> 'w
        abstract ChangeEvent : ChangeData Address
        abstract Type : Type
        end

/// Describes a property of a simulant.
/// Similar to a Haskell lens, but specialized to simulant properties.
type [<NoEquality; NoComparison>] Lens<'a, 's, 'w when 's :> Simulant> =
    { Name : string
      Get : 's -> 'w -> 'a
      SetOpt : ('a -> 's -> 'w -> 'w) voption }

    interface 'w Lens with
        member this.Name = this.Name
        member this.Get simulant world = this.Get (simulant :?> 's) world :> obj
        member this.SetOpt = ValueOption.map (fun set -> fun (value : obj) simulant world -> set (value :?> 'a) (simulant :?> 's) world) this.SetOpt
        member this.TrySet value simulant world = match this.SetOpt with ValueSome set -> set (value :?> 'a) (simulant :?> 's) world | ValueNone -> world
        member this.ChangeEvent = this.ChangeEvent
        member this.Type = typeof<'a>

    member this.GetBy by simulant world =
        by (this.Get simulant world)

    member this.GetByWorld by simulant world =
        by (this.Get simulant world) world

    member this.TrySet value simulant world =
        match this.SetOpt with
        | ValueSome setter -> (true, setter value simulant world)
        | ValueNone -> (false, world)

    member this.Set value simulant world =
        match this.TrySet value simulant world with
        | (true, world) -> world
        | (false, _) -> failwith ("Lens for '" + this.Name + "' is readonly.")

    member this.TryUpdateWorld (updater : 'a -> 's -> 'w -> 'a) simulant world =
        let value = this.Get simulant world
        let value' = updater value simulant world
        this.TrySet value' simulant world

    member this.TryUpdateEffect (updater : 'a -> 's -> 'w -> ('a * 'w)) simulant (world : 'w) =
        let value = this.Get simulant world
        let (value', world) = updater value simulant world
        this.TrySet value' simulant world

    member this.TryUpdate (updater : 'a -> 's -> 'a) simulant world =
        this.TryUpdateWorld (fun value simulant _ -> updater value simulant) simulant world

    member this.UpdateEffect updater simulant world =
        match this.TryUpdateEffect updater simulant world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.UpdateWorld updater simulant world =
        match this.TryUpdateWorld updater simulant world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.Update updater simulant world =
        match this.TryUpdate updater simulant world with
        | (true, world) -> world
        | (false, _) -> failwithumf ()

    member this.ChangeEvent : ChangeData Address =
        let names = [|Constants.Address.ChangeName; this.Name; Constants.Address.EventName|]
        { Names = names; HashCode = Constants.Address.ChangeNameHash ^^^ hash this.Name ^^^ Constants.Address.EventNameHash; Anonymous = true }

    member inline this.Type =
        typeof<'a>

[<RequireQualifiedAccess>]
module Lens =

    let name<'a, 's, 'w when 's :> Simulant> (lens : Lens<'a, 's, 'w>) =
        lens.Name

    let get<'a, 's, 'w when 's :> Simulant> (lens : Lens<'a, 's, 'w>) simulant world =
        lens.Get simulant world

    let getBy<'a, 'b, 's, 'w when 's :> Simulant> by (lens : Lens<'a, 's, 'w>) simulant world : 'b =
        lens.GetBy by simulant world

    let getByWorld<'a, 'b, 's, 'w when 's :> Simulant> by (lens : Lens<'a, 's, 'w>) simulant world : 'b =
        lens.GetByWorld by simulant world

    let setOpt<'a, 's, 'w when 's :> Simulant> a (lens : Lens<'a, 's, 'w>) simulant world =
        match lens.SetOpt with
        | ValueSome set -> set a simulant world
        | ValueNone -> world

    let trySet<'a, 's, 'w when 's :> Simulant> a (lens : Lens<'a, 's, 'w>) simulant world =
        lens.TrySet a simulant world

    let set<'a, 's, 'w when 's :> Simulant> a (lens : Lens<'a, 's, 'w>) simulant world =
        lens.Set a simulant world

    let tryUpdateEffect<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) simulant world =
        lens.TryUpdateEffect updater simulant world

    let tryUpdateWorld<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) simulant world =
        lens.TryUpdateWorld updater simulant world

    let tryUpdate<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) simulant world =
        lens.TryUpdate updater simulant world

    let updateEffect<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) simulant world =
        lens.UpdateEffect updater simulant world

    let updateWorld<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) simulant world =
        lens.UpdateWorld updater simulant world

    let update<'a, 's, 'w when 's :> Simulant> updater (lens : Lens<'a, 's, 'w>) simulant world =
        lens.Update updater simulant world

    let changeEvent<'a, 's, 'w when 's :> Simulant> (lens : Lens<'a, 's, 'w>) =
        lens.ChangeEvent

    let ty<'a, 's, 'w when 's :> Simulant> (lens : Lens<'a, 's, 'w>) =
        lens.Type

    let make<'a, 's, 'w when 's :> Simulant> name (get : 's -> 'w -> 'a) set : Lens<'a, 's, 'w> =
        { Name = name; Get = get; SetOpt = ValueSome set }

    let makeReadOnly<'a, 's, 'w when 's :> Simulant> name (get : 's -> 'w -> 'a) : Lens<'a, 's, 'w> =
        { Name = name; Get = get; SetOpt = ValueNone }

[<AutoOpen>]
module LensOperators =

    /// Make a writable lens.
    let lens<'a, 's, 'w when 's :> Simulant> name (get : 's -> 'w -> 'a) set =
        Lens.make name get set

    /// Make a read-only lens.
    let lensReadOnly<'a, 's, 'w when 's :> Simulant> name (get : 's -> 'w -> 'a) =
        Lens.makeReadOnly name get

    /// Define a property along with its initial value.
    let define (lens : Lens<'a, 's, 'w>) (value : 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (DefineExpr value)

    /// Define a variable property.
    let variable (lens : Lens<'a, 's, 'w>) (variable : 'w -> 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (VariableExpr (fun world -> variable (world :?> 'w) :> obj))

    /// Define a computed property.
    let computed (lens : Lens<'a, 's, 'w>) (get : 't -> 'w -> 'a) (setOpt : ('a -> 't -> 'w -> 'w) option) =
        let computedProperty =
            ComputedProperty.make
                typeof<'a>
                (fun (target : obj) (world : obj) -> get (target :?> 't) (world :?> 'w) :> obj)
                (match setOpt with
                 | Some set -> Some (fun value (target : obj) (world : obj) -> set (value :?> 'a) (target :?> 't) (world :?> 'w) :> obj)
                 | None -> None)
        PropertyDefinition.makeValidated lens.Name typeof<ComputedProperty> (ComputedExpr computedProperty)
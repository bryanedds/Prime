// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open Prime

/// A generalized simulant lens.
type 'w Lens2 =
    interface
        abstract Name : string
        abstract Get : Simulant -> 'w -> obj
        abstract SetOpt : (obj -> Simulant -> 'w -> 'w) option
        abstract TrySet : obj -> Simulant -> 'w -> 'w
        abstract ChangeEvent : ChangeData Address
        abstract Type : Type
        end

/// Describes a property of a simulant.
/// Similar to a Haskell lens, but specialized to simulant properties.
type [<NoEquality; NoComparison>] Lens2<'a, 's, 'w when 's :> Simulant> =
    { Name : string
      Get : 's -> 'w -> 'a
      SetOpt : ('a -> 's -> 'w -> 'w) option }

    interface 'w Lens2 with
        member this.Name = this.Name
        member this.Get simulant world = this.Get (simulant :?> 's) world :> obj
        member this.SetOpt = Option.map (fun set -> fun (value : obj) simulant world -> set (value :?> 'a) (simulant :?> 's) world) this.SetOpt
        member this.TrySet value simulant world = match this.SetOpt with Some set -> set (value :?> 'a) (simulant :?> 's) world | None -> world
        member this.ChangeEvent = this.ChangeEvent
        member this.Type = typeof<'a>

    member this.GetBy by simulant world =
        by (this.Get simulant world)

    member this.GetByWorld by simulant world =
        by (this.Get simulant world) world

    member this.TrySet value simulant world =
        match this.SetOpt with
        | Some setter -> (true, setter value simulant world)
        | None -> (false, world)

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

    member this.Map mapper : Lens2<'b, 's, 'w> =
        { Name = this.Name
          Get = fun simulant world -> mapper (this.Get simulant world)
          SetOpt = None }

    member this.MapWorld mapper : Lens2<'b, 's, 'w> =
        { Name = this.Name
          Get = fun simulant world -> mapper (this.Get simulant world) world
          SetOpt = None }

    member this.Isomap mapper unmapper : Lens2<'b, 's, 'w> =
        { Name = this.Name
          Get = fun simulant world -> mapper (this.Get simulant world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (unmapper value)) | None -> None }

    member this.IsomapWorld mapper unmapper : Lens2<'b, 's, 'w> =
        { Name = this.Name
          Get = fun simulant world -> mapper (this.Get simulant world) world
          SetOpt = match this.SetOpt with Some set -> Some (fun value world -> set (unmapper value world) world) | None -> None }

    member this.ChangeEvent =
        rtoa<ChangeData> [|"Change"; this.Name; "Event"|]

    member inline this.Type =
        typeof<'a>

    /// Map over a lens in the given world context (read-only).
    static member inline (--|>) (lens : Lens2<_, 's, 'w>, mapper) = lens.MapWorld mapper

    /// Map over a lens (read-only).
    static member inline (-->) (lens : Lens2<_, 's, 'w>, mapper) = lens.Map mapper

[<RequireQualifiedAccess>]
module Lens2 =

    let name<'a, 's, 'w when 's :> Simulant> (lens : Lens2<'a, 's, 'w>) =
        lens.Name

    let get<'a, 's, 'w when 's :> Simulant> (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.Get simulant world

    let getBy<'a, 'b, 's, 'w when 's :> Simulant> mapper (lens : Lens2<'a, 's, 'w>) simulant world : 'b =
        lens.GetBy mapper simulant world

    let getByWorld<'a, 'b, 's, 'w when 's :> Simulant> mapper (lens : Lens2<'a, 's, 'w>) simulant world : 'b =
        lens.GetByWorld mapper simulant world

    let setOpt<'a, 's, 'w when 's :> Simulant> a (lens : Lens2<'a, 's, 'w>) simulant world =
        match lens.SetOpt with
        | Some set -> set a simulant world
        | None -> world

    let trySet<'a, 's, 'w when 's :> Simulant> a (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.TrySet a simulant world

    let set<'a, 's, 'w when 's :> Simulant> a (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.Set a simulant world

    let tryUpdateEffect<'a, 's, 'w when 's :> Simulant> updater (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.TryUpdateEffect updater simulant world

    let tryUpdateWorld<'a, 's, 'w when 's :> Simulant> updater (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.TryUpdateWorld updater simulant world

    let tryUpdate<'a, 's, 'w when 's :> Simulant> updater (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.TryUpdate updater simulant world

    let updateEffect<'a, 's, 'w when 's :> Simulant> updater (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.UpdateEffect updater simulant world

    let updateWorld<'a, 's, 'w when 's :> Simulant> updater (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.UpdateWorld updater simulant world

    let update<'a, 's, 'w when 's :> Simulant> updater (lens : Lens2<'a, 's, 'w>) simulant world =
        lens.Update updater simulant world

    let map<'a, 'b, 's, 'w when 's :> Simulant> mapper (lens : Lens2<'a, 's, 'w>) : Lens2<'b, 's, 'w> =
        lens.Map mapper

    let mapWorld<'a, 'b, 's, 'w when 's :> Simulant> mapper (lens : Lens2<'a, 's, 'w>) : Lens2<'b, 's, 'w> =
        lens.MapWorld mapper

    let isomap<'a, 'b, 's, 'w when 's :> Simulant> mapper unmapper (lens : Lens2<'a, 's, 'w>) : Lens2<'b, 's, 'w> =
        lens.Isomap mapper unmapper

    let isomapWorld<'a, 'b, 's, 'w when 's :> Simulant> mapper unmapper (lens : Lens2<'a, 's, 'w>) : Lens2<'b, 's, 'w> =
        lens.IsomapWorld mapper unmapper

    let changeEvent<'a, 's, 'w when 's :> Simulant> (lens : Lens2<'a, 's, 'w>) =
        lens.ChangeEvent

    let ty<'a, 's, 'w when 's :> Simulant> (lens : Lens2<'a, 's, 'w>) =
        lens.Type

    let makeReadOnly<'a, 's, 'w when 's :> Simulant> name get : Lens2<'a, 's, 'w> =
        { Name = name; Get = get; SetOpt = None }

    let make<'a, 's, 'w when 's :> Simulant> name get setOpt : Lens2<'a, 's, 'w> =
        { Name = name; Get = get; SetOpt = setOpt }

[<AutoOpen>]
module Lens2Operators =

    /// Make a writable lens.
    let lens2<'a, 's, 'w> this name get set =
        Lens.make this name get set

    /// Make a read-only lens.
    let lens2ReadOnly<'a, 's, 'w> this name get =
        Lens.makeReadOnly this name get

    /// Define a property along with its initial value.
    let define2 (lens : Lens2<'a, 's, 'w>) (value : 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (DefineExpr value)

    /// Define a variable property.
    let variable2 (lens : Lens2<'a, 's, 'w>) (variable : 'w -> 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (VariableExpr (fun world -> variable (world :?> 'w) :> obj))

    /// Define a computed property.
    let computed2 (lens : Lens2<'a, 's, 'w>) (get : 't -> 'w -> 'a) (setOpt : ('a -> 't -> 'w -> 'w) option) =
        let computedProperty =
            ComputedProperty.make
                typeof<'a>
                (fun (target : obj) (world : obj) -> get (target :?> 't) (world :?> 'w) :> obj)
                (match setOpt with
                 | Some set -> Some (fun value (target : obj) (world : obj) -> set (value :?> 'a) (target :?> 't) (world :?> 'w) :> obj)
                 | None -> None)
        PropertyDefinition.makeValidated lens.Name typeof<ComputedProperty> (ComputedExpr computedProperty)
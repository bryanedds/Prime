// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open Prime

/// A generalized simulant lens.
type 'w Lens =
    interface
        abstract Name : string
        abstract ValidateOpt : ('w -> bool) option
        abstract Validate : 'w -> bool
        abstract GetWithoutValidation : 'w -> obj
        abstract SetOpt : (obj -> 'w -> 'w) option
        abstract This : Simulant
        abstract ChangeEvent : ChangeData Address
        abstract Type : Type
        end

/// Describes a property of a simulant.
/// Similar to a Haskell lens, but specialized to simulant properties.
type [<NoEquality; NoComparison>] Lens<'a, 'w> =
    { Name : string
      ValidateOpt : ('w -> bool) option
      GetWithoutValidation : 'w -> 'a
      SetOpt : ('a -> 'w -> 'w) option
      This : Simulant }

    interface 'w Lens with
        member this.Name = this.Name
        member this.ValidateOpt = this.ValidateOpt
        member this.Validate world = match this.ValidateOpt with Some validate -> validate world | None -> true
        member this.GetWithoutValidation world = this.GetWithoutValidation world :> obj
        member this.SetOpt = Option.map (fun set -> fun (value : obj) world -> set (value :?> 'a) world) this.SetOpt
        member this.This = this.This
        member this.ChangeEvent = this.ChangeEvent
        member this.Type = typeof<'a>

    member this.Generalize () =
        let this = this :> 'w Lens
        { Name = this.Name
          ValidateOpt = this.ValidateOpt
          GetWithoutValidation = this.GetWithoutValidation
          SetOpt = this.SetOpt
          This = this.This }

    member this.Get world =
        match this.ValidateOpt with
        | Some validate when not (validate world) -> failwith "Invalid lens."
        | Some _ | None _ -> this.GetWithoutValidation world

    member this.GetBy by world =
        match this.ValidateOpt with
        | Some validate when not (validate world) -> failwith "Invalid lens."
        | Some _ | None _ -> by (this.GetWithoutValidation world)

    member this.TrySet value world =
        match this.SetOpt with
        | Some setter -> (true, setter value world)
        | None -> (false, world)
      
    member this.Set value world =
        match this.TrySet value world with
        | (true, world) -> world
        | (false, _) -> failwith ("PropertyTag for '" + this.Name + "' is readonly.")

    member this.TryUpdateEffect updater world =
        match this.ValidateOpt with
        | Some validate when not (validate world) -> failwith "Invalid lens."
        | Some _ | None _ -> 
            let value = this.GetWithoutValidation world
            let (value', world) = updater value world
            this.TrySet value' world

    member this.TryUpdateWorld updater world =
        match this.ValidateOpt with
        | Some validate when not (validate world) -> failwith "Invalid lens."
        | Some _ | None _ ->
            let value = this.GetWithoutValidation world
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

    member this.Map mapper : Lens<'b, 'w> =
        { Name = this.Name
          ValidateOpt = this.ValidateOpt
          GetWithoutValidation = fun world -> mapper (this.GetWithoutValidation world)
          SetOpt = None
          This = this.This }

    member this.MapWorld mapper : Lens<'b, 'w> =
        { Name = this.Name
          ValidateOpt = this.ValidateOpt
          GetWithoutValidation = fun world -> mapper (this.GetWithoutValidation world) world
          SetOpt = None
          This = this.This }

    member this.Bimap mapper unmapper : Lens<'b, 'w> =
        { Name = this.Name
          ValidateOpt = this.ValidateOpt
          GetWithoutValidation = fun world -> mapper (this.GetWithoutValidation world)
          SetOpt = match this.SetOpt with Some set -> Some (fun value -> set (unmapper value)) | None -> None
          This = this.This }

    member this.ChangeEvent =
        let changeEventAddress = rtoa<ChangeData> [|"Change"; this.Name; "Event"|]
        match box this.This with
        | null ->
            // HACK: this case is a hack to be able to insert events into the elmish event handler.
            changeEventAddress
        | _ ->
            let changeEvent = changeEventAddress --> this.This.SimulantAddress
            changeEvent

    member this.Type =
        typeof<'a>

    (* Lensing Operators *)
    static member inline ( += ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (+) value)
    static member inline ( -= ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (-) value)
    static member inline ( *= ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (*) value)
    static member inline ( /= ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (/) value)
    static member inline ( %= ) (lens : Lens<_, 'w>, value) =  lens.Update (flip (%) value)
    static member inline (~+)   (lens : Lens<_, 'w>) =         lens.Update (~+)
    static member inline (~-)   (lens : Lens<_, 'w>) =         lens.Update (~-)
    static member inline (!+)   (lens : Lens<_, 'w>) =         lens.Update inc
    static member inline (!-)   (lens : Lens<_, 'w>) =         lens.Update dec
    static member inline (+)    (lens : Lens<_, 'w>, value) =  lens.Map (flip (+) value)
    static member inline (-)    (lens : Lens<_, 'w>, value) =  lens.Map (flip (-) value)
    static member inline (*)    (lens : Lens<_, 'w>, value) =  lens.Map (flip (*) value)
    static member inline (/)    (lens : Lens<_, 'w>, value) =  lens.Map (flip (/) value)
    static member inline (%)    (lens : Lens<_, 'w>, value) =  lens.Map (flip (%) value)
    static member inline ( ** ) (lens : Lens<_, 'w>, value) =  lens.Map (flip ( ** ) value)
    static member inline (<<<)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (<<<) value)
    static member inline (>>>)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (>>>) value)
    static member inline (&&&)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (&&&) value)
    static member inline (|||)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (|||) value)
    static member inline (^^^)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (^^^) value)
    static member inline (=.)   (lens : Lens<_, 'w>, value) =  lens.Map (flip (=) value)
    static member inline (<>.)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (<>) value)
    static member inline (<.)   (lens : Lens<_, 'w>, value) =  lens.Map (flip (<) value)
    static member inline (<=.)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (<=) value)
    static member inline (>.)   (lens : Lens<_, 'w>, value) =  lens.Map (flip (>) value)
    static member inline (>=.)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (>=) value)
    static member inline (&&.)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (&&) value)
    static member inline (||.)  (lens : Lens<_, 'w>, value) =  lens.Map (flip (||) value)
    static member inline (.[])  (lens : Lens<_, 'w>, value) =  lens.Map (flip (.[]) value)
    static member inline (~~~)  (lens : Lens<_, 'w>) =         lens.Map (~~~)

    /// Map over a lens in the given world context (read-only).
    static member inline (--|>) (lens : Lens<_, 'w>, mapper) = lens.MapWorld mapper

    /// Map over a lens (read-only).
    static member inline (-->) (lens : Lens<_, 'w>, mapper) = lens.Map mapper

    /// Set a lensed property.
    static member inline (<--) (lens : Lens<_, 'w>, value) =  lens.Set value

    /// Get a lensed property.
    /// TODO: see if this operator is actually useful / understandable.
    static member inline (!.) (lens : Lens<_, 'w>) =
        fun world ->
            match lens.ValidateOpt with
            | Some validate when not (validate world) -> failwith "Invalid lens."
            | Some _ | None _ -> lens.GetWithoutValidation world

[<RequireQualifiedAccess>]
module Lens =

    let name<'a, 'w> (lens : Lens<'a, 'w>) =
        lens.Name

    let validate<'a, 'w> (lens : Lens<'a, 'w>) world =
        match lens.ValidateOpt with
        | Some validate -> validate world
        | None -> true

    let getWithoutValidation<'a, 'w> (lens : Lens<'a, 'w>) world =
        lens.GetWithoutValidation world

    let get<'a, 'w> (lens : Lens<'a, 'w>) world =
        match lens.ValidateOpt with
        | Some validate when not (validate world) -> failwith "Invalid lens."
        | Some _ | None _ -> lens.GetWithoutValidation world

    let setOpt<'a, 'w> a (lens : Lens<'a, 'w>) world =
        match lens.SetOpt with
        | Some set -> set a world
        | None -> world

    let this (lens : Lens<'a, 'w>) =
        lens.This

    let generalize (lens : Lens<'a, 'w>) =
        lens.Generalize ()

    let getBy<'a, 'b, 'w> mapper (lens : Lens<'a, 'w>) world : 'b =
        lens.GetBy mapper world

    let trySet<'a, 'w> a (lens : Lens<'a, 'w>) world =
        lens.TrySet a world

    let set<'a, 'w> a (lens : Lens<'a, 'w>) world =
        lens.Set a world

    let map<'a, 'b, 'w> mapper (lens : Lens<'a, 'w>) : Lens<'b, 'w> =
        lens.Map mapper

    let mapWorld<'a, 'b, 'w> mapper (lens : Lens<'a, 'w>) : Lens<'b, 'w> =
        lens.MapWorld mapper

    let bimap<'a, 'b, 'w> mapper unmapper (lens : Lens<'a, 'w>) : Lens<'b, 'w> =
        lens.Bimap mapper unmapper

    let changeEvent<'a, 'w> (lens : Lens<'a, 'w>) =
        lens.ChangeEvent

    let ty<'a, 'w> (lens : Lens<'a, 'w>) =
        lens.Type

    let tryIndex i (lens : Lens<'a seq, 'w>) : Lens<'a option, 'w> =
        lens.Map (Seq.tryItem i)

    let explode (lens : Lens<'a seq, 'w>) : Lens<'a option, 'w> seq =
        Seq.initInfinite id |>
        Seq.map (fun index ->
            map (fun items ->
                match Seq.tryItem index items with
                | Some item -> Some item
                | None -> None)
                lens)

    let explodeIndexedOpt indexerOpt (lens : Lens<'a seq, 'w>) : Lens<(int * 'a) option, 'w> seq =
        Seq.initInfinite id |>
        Seq.map (fun index ->
            map (fun models ->
                match indexerOpt with
                | Some indexer ->
                    let indexed = Seq.map (fun item -> (indexer item, item)) models
                    Seq.tryFind (fun (index2, _) -> index = index2) indexed
                | None ->
                    match Seq.tryItem index models with
                    | Some model -> Some (index, model)
                    | None -> None)
                lens)

    let explodeIndexed indexer (lens : Lens<'a seq, 'w>) : Lens<(int * 'a) option, 'w> seq =
        explodeIndexedOpt (Some indexer) lens

    let dereference (lens : Lens<'a option, 'w>) : Lens<'a, 'w> =
        lens.Map Option.get

    let makeReadOnly<'a, 'w> name get this : Lens<'a, 'w> =
        { Name = name; ValidateOpt = None; GetWithoutValidation = get; SetOpt = None; This = this }

    let make<'a, 'w> name get set this : Lens<'a, 'w> =
        { Name = name; ValidateOpt = None; GetWithoutValidation = get; SetOpt = Some set; This = this }

[<AutoOpen>]
module LensOperators =

    let lens<'a, 'w> name get set this =
        Lens.make name get set this

    let lensOut<'a, 'w> name get this =
        Lens.makeReadOnly name get this

    let define (lens : Lens<'a, 'w>) (value : 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (DefineExpr value)

    let variable (lens : Lens<'a, 'w>) (variable : 'w -> 'a) =
        PropertyDefinition.makeValidated lens.Name typeof<'a> (VariableExpr (fun world -> variable (world :?> 'w) :> obj))

    let computed (lens : Lens<'a, 'w>) (get : 't -> 'w -> 'a) (setOpt : ('a -> 't -> 'w -> 'w) option) =
        let computedProperty =
            ComputedProperty.make
                typeof<'a>
                (fun (target : obj) (world : obj) -> get (target :?> 't) (world :?> 'w) :> obj)
                (match setOpt with
                 | Some set -> Some (fun value (target : obj) (world : obj) -> set (value :?> 'a) (target :?> 't) (world :?> 'w) :> obj)
                 | None -> None)
        PropertyDefinition.makeValidated lens.Name typeof<ComputedProperty> (ComputedExpr computedProperty)
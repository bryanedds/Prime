// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime.Tests
open System
open System.Numerics
open NUnit.Framework
open Prime
open Prime.Ecs

type [<Struct>] Position =
    { mutable Active : bool
      mutable Position : Vector2 }
    interface Position Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<Struct>] Velocity =
    { mutable Active : bool
      mutable Velocity : Vector2 }
    interface Velocity Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type [<Struct>] Shake =
    { mutable Active : bool
      mutable Origin : Vector2
      mutable Offset : Vector2 }
    interface Shake Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

module EcsTests =

    let [<Test>] runEcs () =

        // get ecs
        let ecs = Ecs ()

        // create movers query
        let movers = Query.make<Position, Velocity> ecs

        // create shakers query
        let shakers = Query.make<Position, Shake> ecs

        // create 10 movers the slow way
        for _ in 0 .. dec 10 do
            let mover = ecs.MakeEntity ()
            mover.Register { Active = true; Position = Vector2.Zero }
            mover.Register { Active = true; Velocity = Vector2.One }

        // create 20 more movers the fast way
        let moverComponents = [{ Active = true; Position = Vector2.Zero } :> obj; { Active = true; Velocity = Vector2.One } :> obj]
        let moverArchetypeId = ArchetypeId.make (moverComponents, Map.empty)
        ecs.RegisterEntities true 20 moverComponents moverArchetypeId |> ignore

        // create 30 shakers
        let shakerArchetypeId = ArchetypeId.make<Position, Shake> (subterms = Map.empty)
        let shakerComponents = [{ Active = true; Position = Vector2.Zero } :> obj; { Active = true; Origin = Vector2.Zero; Offset = Vector2.One } :> obj]
        ecs.RegisterEntities true 30 shakerComponents shakerArchetypeId |> ignore

        // define update for movers
        movers.SubscribeIteration (EcsEvents.Update, fun position velocity ->
            position.Position.X <- position.Position.X + velocity.Velocity.X
            position.Position.Y <- position.Position.Y + velocity.Velocity.Y)

        // define update for shakers
        shakers.SubscribeIteration (EcsEvents.Update, fun position shake ->
            position.Position.X <- shake.Origin.X + Random.Shared.NextSingle () * shake.Offset.X
            position.Position.Y <- shake.Origin.Y + Random.Shared.NextSingle () * shake.Offset.Y)

        // publish update event
        ecs.Publish EcsEvents.Update ()
﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System

[<RequireQualifiedAccess>]
module Xtension =

    // OPTIMIZATION: Xtension flag bit-masks; only for use by internal facilities.
    let [<Literal>] private ImperativeMask =                0b0001
    let [<Literal>] private ContainsRuntimePropertiesMask = 0b0010

    /// Provides a convenient way to implement both dynamic properties and designer properties.
    type [<ReferenceEquality>] Xtension =
        private
            { Properties : UMap<string, Property> // TODO: see if a quadratic searching dictionary could improve perf here.
              Flags : int }
            member this.Imperative with get () = this.Flags &&& ImperativeMask <> 0
            member this.ContainsRuntimeProperties with get () = this.Flags &&& ContainsRuntimePropertiesMask <> 0

        /// The dynamic look-up operator for an Xtension.
        /// Example:
        ///     let parallax : single = xtn?Parallax
        static member (?) (xtension, propertyName) : 'a =

            // try to find an existing property
            match UMap.tryFind propertyName xtension.Properties with
            | Some property ->

                // return property directly if the return type matches, otherwise the default value for that type
                match property.PropertyValue with
                | :? DesignerProperty as dp ->
                    match dp.DesignerValue with
                    | :? 'a as propertyValue -> propertyValue
                    | _ -> failwith ("Xtension property '" + propertyName + "' of type '" + property.PropertyType.Name + "' is not of the expected type '" + typeof<'a>.Name + "'.")
                | :? 'a as value -> value
                | _ -> failwith ("Xtension property '" + propertyName + "' of type '" + property.PropertyType.Name + "' is not of the expected type '" + typeof<'a>.Name + "'.")

            // can't find the required property.
            | None -> failwith ("No property '" + propertyName + "' found.")

        /// The dynamic assignment operator for an Xtension.
        /// Example:
        ///     let xtn = xtn.Position <- Vector2 (4.0, 5.0).
        static member (?<-) (xtension, propertyName, value : 'a) =
            match UMap.tryFind propertyName xtension.Properties with
            | Some property ->
#if DEBUG
                if property.PropertyType <> typeof<'a> then
                    failwith "Cannot change the type of an existing Xtension property."
#endif
                if xtension.Imperative then
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerValue <- value :> obj
                    | _ -> property.PropertyValue <- value :> obj
                    xtension
                else
                    match property.PropertyValue with
                    | :? DesignerProperty as dp ->
                        let property = { property with PropertyValue = { dp with DesignerValue = value }}
                        let properties = UMap.add propertyName property xtension.Properties
                        { xtension with Properties = properties }
                    | _ ->
                        let property = { property with PropertyValue = value :> obj }
                        let properties = UMap.add propertyName property xtension.Properties
                        { xtension with Properties = properties }
            | None -> failwith "Cannot add property to a sealed Xtension."

    /// Check whether the Xtension uses mutation.
    let getImperative (xtension : Xtension) = xtension.Imperative

    /// Check whether the Xtension contains any DesignerProperty's or ComputedProperty's in constant-time (via an
    /// internally-cached flag).
    let containsRuntimeProperties (xtension : Xtension) = xtension.ContainsRuntimeProperties

    /// Try to get a property from an Xtension.
    let tryGetProperty (name, xtension, propertyRef : _ outref) =
        UMap.tryGetValue (name, xtension.Properties, &propertyRef)

    /// Get a property from an xtension.
    let getProperty name xtension = UMap.find name xtension.Properties

    /// Attempt to set a property on an Xtension.
    let trySetProperty name property xtension =
        let mutable propertyRef = Unchecked.defaultof<_>
        match UMap.tryGetValue (name, xtension.Properties, &propertyRef) with
        | true ->
#if DEBUG
            if property.PropertyType <> propertyRef.PropertyType then
                failwith "Cannot change the type of an existing Xtension property."
#endif
            if xtension.Imperative then
                propertyRef.PropertyValue <- property.PropertyValue
                struct (true, xtension)
            else struct (true, { xtension with Properties = UMap.add name property xtension.Properties })
        | false -> struct (false, xtension)

    /// Set a property on an Xtension.
    let setProperty name property xtension =
        match trySetProperty name property xtension with
        | struct (true, xtension) -> xtension
        | struct (false, _) -> failwith "Cannot add property to a sealed Xtension."

    /// Attach a property to an Xtension.
    let attachProperty name property xtension =
        let isRuntimeProperty = if Reflection.isRuntimeProperty property then ContainsRuntimePropertiesMask else 0
        { xtension with Properties = UMap.add name property xtension.Properties; Flags = xtension.Flags ||| isRuntimeProperty }

    /// Attach multiple properties to an Xtension.
    let attachProperties properties xtension =
        let containsRuntimeProperties = if Reflection.containsRuntimeProperties properties then ContainsRuntimePropertiesMask else 0
        { xtension with Properties = UMap.addMany properties xtension.Properties; Flags = xtension.Flags ||| containsRuntimeProperties }

    /// Detach a property from an Xtension.
    let detachProperty name xtension =
        let properties = UMap.remove name xtension.Properties
        let containsRuntimeProperties = if Reflection.containsRuntimeProperties xtension.Properties then ContainsRuntimePropertiesMask else 0
        { xtension with
            Properties = properties
            Flags = xtension.Flags &&& ImperativeMask ||| containsRuntimeProperties }

    /// Detach multiple properties from an Xtension.
    let detachProperties names xtension =
        let properties = UMap.removeMany names xtension.Properties
        let containsRuntimeProperties = if Reflection.containsRuntimeProperties xtension.Properties then ContainsRuntimePropertiesMask else 0
        { xtension with
            Properties = properties
            Flags = xtension.Flags &&& ImperativeMask ||| containsRuntimeProperties }

    /// Make an Xtension.
    let make imperative properties =
        { Properties = properties
          Flags =
            (if imperative then ImperativeMask else 0) |||
            (if Reflection.containsRuntimeProperties properties then ContainsRuntimePropertiesMask else 0) }

    /// Make an empty Xtension.
    let makeEmpty imperative =
        make imperative (UMap.makeEmpty StringComparer.Ordinal (if imperative then Imperative else Functional))

    /// An Xtension that is imperative.
    let makeImperative () = make true (UMap.makeEmpty StringComparer.Ordinal Imperative)

    /// An Xtension that isn't imperative.
    let makeFunctional () = make false (UMap.makeEmpty StringComparer.Ordinal Functional)

    /// Make an Xtension by copying properties of another Xtension.
    let rec makeFromXtension (xtension : Xtension) = 
        xtension |>
        toSeq |>
        Seq.map (fun (n, p) ->
            let propertyValue =
                match p.PropertyValue with
                | :? DesignerProperty as dp -> { dp with DesignerType = dp.DesignerType } :> obj
                | value -> value
            (n, { p with PropertyValue = propertyValue })) |>
        ofSeq xtension.Imperative

    /// Convert an xtension to a sequence of its entries.
    and toSeq xtension =
        xtension.Properties :> _ seq

    /// Convert an xtension to a sequence of its entries.
    and ofSeq imperative seq =
        attachProperties seq (makeEmpty imperative)

/// Provides a convenient way to implement both dynamic properties and designer properties.
type Xtension = Xtension.Xtension
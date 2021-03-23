// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open Prime

[<RequireQualifiedAccess>]
module Xtension =

    // OPTIMIZATION: Xtension flag bit-masks; only for use by internal facilities.
    let [<Literal>] internal CanDefaultMask =   0b0000000001
    let [<Literal>] internal SealedMask =       0b0000000010
    let [<Literal>] internal ImperativeMask =   0b0000000100

    /// Xtensions are a dynamic, functional, and convenient way to implement both dynamic properties
    /// and designer properties.
    /// OPTIMIZATION: Booleans are packed into the Flags field.
    type [<NoEquality; NoComparison>] Xtension =
        private
            { Properties : UMap<string, Property>
              Flags : int }

            // Member properties; only for use by internal facilities.
            member this.CanDefault with get () = this.Flags &&& CanDefaultMask <> 0
            member this.Sealed with get () = this.Flags &&& SealedMask <> 0
            member this.Imperative with get () = this.Flags &&& ImperativeMask <> 0

        /// Try to get the default value for a given xtension member, returning None when defaulting is disallowed.
        static member private tryGetDefaultValue (this : Xtension) propertyName : 'a =
            if this.CanDefault then scdefaultof ()
            else failwith ("Xtension property '" + propertyName + "' does not exist and no default is permitted because CanDefault is false.")

        /// The dynamic look-up operator for an Xtension.
        /// Example:
        ///     let parallax : single = xtn?Parallax
        static member (?) (xtension, propertyName) : 'a =

            // check if dynamic member is an existing property
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

            // presume we're looking for a property that doesn't exist, so try to get the default value
            | None -> Xtension.tryGetDefaultValue xtension propertyName

        /// The dynamic assignment operator for an Xtension.
        /// Example:
        ///     let xtn = xtn.Position <- Vector2 (4.0, 5.0).
        static member (?<-) (xtension, propertyName, value : 'a) =
            if typeof<'a> = typeof<DesignerProperty> then
                failwith "Cannot directly set an Xtension property to a DesignerProperty."
            match UMap.tryFind propertyName xtension.Properties with
            | Some property ->
                if xtension.Sealed && property.PropertyType <> typeof<'a> then
                    failwith "Cannot change the type of a sealed Xtension's property."
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
            | None ->
                if xtension.Sealed then failwith "Cannot add property to a sealed Xtension."
                let property = { PropertyType = typeof<'a>; PropertyValue = value :> obj }
                let properties = UMap.add propertyName property xtension.Properties
                { xtension with Properties = properties }

    /// Make an extension.
    let make properties canDefault isSealed imperative =
        { Properties = properties
          Flags =
            (if canDefault then CanDefaultMask else 0) |||
            (if isSealed then SealedMask else 0) |||
            (if imperative then ImperativeMask else 0) }

    /// An Xtension that cannot default, is sealed, and is imperative.
    let makeImperative () = make (UMap.makeEmpty Imperative) false true true

    /// An Xtension that can default, isn't sealed, and isn't imperative.
    let makeEmpty () = make (UMap.makeEmpty Functional) true false false

    /// An Xtension that cannot default, is sealed, and isn't imperative.
    let makeSafe () = make (UMap.makeEmpty Functional) false true false

    /// An Xtension that cannot default, isn't sealed, and isn't imperative.
    let makeMixed () = make (UMap.makeEmpty Functional) false false false

    /// Check whether the extension uses mutation.
    let getImperative (xtension : Xtension) = xtension.Imperative

    /// Try to get a property from an xtension.
    let tryGetProperty (name, xtension, propertyRef : _ outref) =
        UMap.tryGetValue (name, xtension.Properties, &propertyRef)

    /// Get a property from an xtension.
    let getProperty name xtension = UMap.find name xtension.Properties

    /// Attempt to set a property on an Xtension.
    let trySetProperty name property xtension =
        let mutable propertyRef = Unchecked.defaultof<_>
        match UMap.tryGetValue (name, xtension.Properties, &propertyRef) with
        | true ->
            if xtension.Imperative then
                propertyRef.PropertyType <- property.PropertyType
                propertyRef.PropertyValue <- property.PropertyValue
                struct (true, xtension)
            else struct (true, { xtension with Properties = UMap.add name property xtension.Properties })
        | false ->
            if not xtension.Sealed
            then struct (true, { xtension with Properties = UMap.add name property xtension.Properties })
            else struct (false, xtension)

    /// Set a property on an Xtension.
    let setProperty name property xtension =
        match trySetProperty name property xtension with
        | struct (true, xtension) -> xtension
        | struct (false, _) -> failwith "Cannot add property to a sealed Xtension."

    /// Attach a property to an Xtension.
    let attachProperty name property xtension = { xtension with Properties = UMap.add name property xtension.Properties }

    /// Attach multiple properties to an Xtension.
    let attachProperties namesAndProperties xtension = { xtension with Properties = UMap.addMany namesAndProperties xtension.Properties }

    /// Detach a property from an Xtension.
    let detachProperty name xtension = { xtension with Properties = UMap.remove name xtension.Properties }

    /// Detach multiple properties from an Xtension.
    let detachProperties names xtension = { xtension with Properties = UMap.removeMany names xtension.Properties }

    /// Convert an xtension to a sequence of its entries.
    let toSeq xtension = xtension.Properties :> _ seq

    /// Convert an xtension to a sequence of its entries.
    let ofSeq seq = attachProperties seq (makeEmpty ())

/// Xtensions (and their supporting types) are a dynamic, functional, and convenient way
/// to implement dynamic properties.
type Xtension = Xtension.Xtension

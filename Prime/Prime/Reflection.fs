// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.ComponentModel
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic
open System.Text
open System.Reflection
open FSharp.Reflection

/// An evaluatable expression for defining a property.
type [<ReferenceEquality>] PropertyExpr =
    | DefineExpr of DefineExpr : obj
    | VariableExpr of VariableExpr : (obj -> obj)
    | ComputedExpr of ComputedProperty : ComputedProperty

    /// Evaluate a property expression.
    static member eval expr context =
        match expr with
        | DefineExpr value -> value
        | VariableExpr fn -> fn context
        | ComputedExpr cp -> cp :> obj

/// The definition of a data-driven property.
and [<ReferenceEquality>] PropertyDefinition =
    { PropertyName : string
      PropertyType : Type
      PropertyExpr : PropertyExpr }

    /// Validate a property definition.
    static member validate propertyDefinition =
        if propertyDefinition.PropertyName = "FacetNames" then failwith "FacetNames cannot be an intrinsic property."
        if propertyDefinition.PropertyName = "OverlayNameOpt" then failwith "OverlayNameOpt cannot be an intrinsic property."
        if Array.exists (fun gta -> gta = typeof<obj>) propertyDefinition.PropertyType.GenericTypeArguments then
            failwith
                ("Generic property definition lacking type information for property '" + propertyDefinition.PropertyName + "'. " +
                 "Use explicit typing on all values that carry incomplete type information such as empty lists, empty sets, and none options.")

    /// Make a property definition.
    static member make propertyName propertyType propertyExpr =
        { PropertyName = propertyName; PropertyType = propertyType; PropertyExpr = propertyExpr }

    /// Make a property definition, validating it in the process.
    static member makeValidated propertyName propertyType propertyExpr =
        let result = PropertyDefinition.make propertyName propertyType propertyExpr
        PropertyDefinition.validate result
        result

/// In tandem with the define literal, grants a nice syntax to define value properties.
and [<NoEquality; NoComparison>] ValueDescription =
    { ValueDescription : unit }
    
    /// Some magic syntax for composing value properties.
    static member (?) (_, propertyName) =
        fun (value : 'a) ->
            PropertyDefinition.makeValidated propertyName typeof<'a> (DefineExpr value)

/// In tandem with the variable literal, grants a nice syntax to define variable properties.
and [<NoEquality; NoComparison>] VariableDescription =
    { VariableDescription : unit }

    /// Some magic syntax for composing variable properties.
    static member (?) (_, propertyName) =
        fun (variable : 'w -> 'a) ->
            PropertyDefinition.makeValidated propertyName typeof<'a> (VariableExpr (fun context -> variable (context :?> 'w) :> obj))
    
/// Describes a property.
and PropertyDescriptor =
    { PropertyName : string
      PropertyType : Type }

/// A vanilla property.
and Property =
    { mutable PropertyType : Type
      mutable PropertyValue : obj }

/// A designer-defined property.
and DesignerProperty =
    { mutable DesignerType : Type
      mutable DesignerValue : obj }

/// A computed property.
and [<ReferenceEquality>] ComputedProperty =
    { ComputedType : Type
      ComputedGet : obj -> obj -> obj
      ComputedSetOpt : (obj -> obj -> obj -> obj) option }

    /// Make a computed property.
    static member make ty get setOpt =
        { ComputedType = ty
          ComputedGet = get
          ComputedSetOpt = setOpt }

    /// Make a readonly computed property.
    static member makeReadOnly ty get =
        ComputedProperty.make ty get None

[<AutoOpen>]
module ReflectionSyntax =

    /// In tandem with the ValueDescription type, grants a nice syntax to define value properties.
    let Define = { ValueDescription = () }

    /// In tandem with the VariableDescription type, grants a nice syntax to define variable properties.
    let Variable = { VariableDescription = () }

[<RequireQualifiedAccess>]
module Reflection =

    // NOTE: had to do some reflection hacking get this assembly as it was the only way I could
    // access ListModule.OfSeq dynamically.
    let private FSharpCoreAssembly =
        Array.find
            (fun (assembly : Assembly) -> assembly.FullName.StartsWith ("FSharp.Core,", StringComparison.Ordinal))
            (AppDomain.CurrentDomain.GetAssemblies ())

    let private UnionCaseNames =
        ConcurrentDictionary<Type, ConcurrentDictionary<string, UnionCaseInfo>> HashIdentity.Reference

    /// Check that a property is either a DesignerProperty or a ComputedProperty.
    let isRuntimeProperty property =
        property.PropertyValue :? DesignerProperty ||
        property.PropertyValue :? ComputedProperty

    /// Check that a UMap contains any runtime properties.
    let containsRuntimeProperties (properties : (string * Property) seq) =
        properties |> Seq.exists (snd >> isRuntimeProperty)

    let objToObjSeq (source : obj) =
        enumerable<obj> (source :?> IEnumerable)

    let objToObjArray (source : obj) =
        Seq.toArray (objToObjSeq source)

    let objToObjList (source : obj) =
        Seq.toList (objToObjSeq source)

    let objToKeyValuePair (source : obj) =
        let kvpType = source.GetType ()
        let key = (kvpType.GetProperty "Key").GetValue (source, null)
        let value = (kvpType.GetProperty "Value").GetValue (source, null)
        KeyValuePair (key, value)

    let objToOption (source : obj) =
        if notNull source then
            let optType = source.GetType ()
            let value = (optType.GetProperty "Value").GetValue (source, null)
            Some value
        else None

    let objToEither (source : obj) =
        let optType = source.GetType ()
        let value = (optType.GetProperty "Item").GetValue (source, null)
        if (optType.GetProperty "IsRight").GetValue (source, null) :?> bool
        then Right value
        else Left value

    let objToComparableSet (source : obj) =
        let iEnumerable = source :?> IEnumerable
        Set.ofSeq (enumerable<IComparable> iEnumerable)

    let objsToKeyValuePair (pairType : Type) fst snd =
        Activator.CreateInstance (pairType, [|fst; snd|])

    let objsToCollection collectionTypeName (sequenceType : Type) (objs : _ seq) =
        let gargs = if sequenceType.IsArray then [|sequenceType.GetElementType ()|] else (sequenceType.GetGenericArguments ())
        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod gargs
        match FSharpCoreAssembly.GetType collectionTypeName with
        | null ->
            let ofSeq = sequenceType.GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)
            ofSeq.Invoke (null, [|cast.Invoke (null, [|objs|])|])
        | fscType ->
            let ofSeq = (fscType.GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod gargs
            ofSeq.Invoke (null, [|cast.Invoke (null, [|objs|])|])

    let pairsToMapping collectionTypeName (mappingType : Type) (pairs : _ seq) =
        let gargs = mappingType.GetGenericArguments ()
        match gargs with
        | [|fstType; sndType|] ->
            let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
            let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|pairType|]
            match FSharpCoreAssembly.GetType collectionTypeName with
            | null ->
                let ofSeq = mappingType.GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)
                ofSeq.Invoke (null, [|cast.Invoke (null, [|pairs|])|])
            | fscType ->
                let ofSeq = (fscType.GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|fstType; sndType|]
                ofSeq.Invoke (null, [|cast.Invoke (null, [|pairs|])|])
        | _ -> failwithumf ()

    let objsToArray arrayType objs =
        objsToCollection "Microsoft.FSharp.Collections.ArrayModule" arrayType objs

    let objsToList listType objs =
        objsToCollection "Microsoft.FSharp.Collections.ListModule" listType objs

    let objsToSet setType objs =
        objsToCollection "Microsoft.FSharp.Collections.SetModule" setType objs

    let pairsToMap mapType objs =
        pairsToMapping "Microsoft.FSharp.Collections.MapModule" mapType objs

    let objsToFSet setType objs =
        objsToCollection "Microsoft.FSharp.Collections.FSetModule" setType objs

    let pairsToFMap mapType objs =
        pairsToMapping "Microsoft.FSharp.Collections.FMapModule" mapType objs

    let tryGetUnionCase (ty : Type) caseName =
        match UnionCaseNames.TryGetValue ty with
        | (true, cases) ->
            match cases.TryGetValue caseName with
            | (true, case) -> Some case
            | (false, _) -> None
        | (false, _) ->
            let cases = FSharpType.GetUnionCases (ty, true)
            let cases' = ConcurrentDictionary ([|for case in cases do KeyValuePair (case.Name, case)|], StringComparer.Ordinal)
            UnionCaseNames.[ty] <- cases'
            match cases'.TryGetValue caseName with
            | (true, case) -> Some case
            | (false, _) -> None

    let getUnionCases (ty : Type) =
        match UnionCaseNames.TryGetValue ty with
        | (true, cases) -> cases
        | (false, _) ->
            let cases = FSharpType.GetUnionCases (ty, true)
            let cases' = ConcurrentDictionary ([|for case in cases do KeyValuePair (case.Name, case)|], StringComparer.Ordinal)
            UnionCaseNames.[ty] <- cases'
            cases'

[<RequireQualifiedAccess>]
module Type =

    /// Try to get an existing type with the given unqualified name. Time-intensive.
    let TryGetTypeUnqualified name =
        match Type.GetType name with
        | null ->
            let allAssemblies = AppDomain.CurrentDomain.GetAssemblies ()
            let types =
                Array.choose
                    (fun (assembly : Assembly) ->
                        match assembly.GetType name with
                        | null -> None
                        | ty -> Some ty)
                    allAssemblies
            Array.tryHead types
        | ty -> Some ty

    /// Get an existing type with the given unqualified name. Time-intensive.
    let GetTypeUnqualified name =
        match TryGetTypeUnqualified name with
        | Some ty -> ty
        | None -> failwith ("Could not find type with unqualified name '" + name + "'.")

    /// Get the first property that is signalled to be preferred by the 'preference' predicate.
    let GetPropertyByPreference (preference, properties) =
        let preferredOpt = Array.tryFind preference properties
        if Array.isEmpty properties then null
        else
            match preferredOpt with
            | Some preferred -> preferred
            | None -> Array.head properties

[<AutoOpen>]
module TypeExtension =

    let private PropertyArrays = ConcurrentDictionary<Type, PropertyInfo array> (HashIdentity.Reference)
    let private PropertiesWritable = ConcurrentDictionary<string * Type, PropertyInfo> (HashIdentity.Structural)
    let private PropertiesWritableArray = ConcurrentDictionary<Type, PropertyInfo array> (HashIdentity.Structural)

    let private getProperties (ty : Type) =
        match PropertyArrays.TryGetValue ty with
        | (true, properties) -> properties
        | (false, _) ->
            let properties = ty.GetProperties ()
            PropertyArrays.TryAdd (ty, properties) |> ignore<bool>
            properties

    /// Type extension for Type.
    type Type with

        /// Attempt to get the default value for a type.
        /// Never returns null.
        /// Thread-safe if value ctor is.
        member this.TryGetDefaultValue () =
            if this.IsPrimitive then Some (Activator.CreateInstance this)
            elif this = typeof<string> then Some (String.Empty :> obj)
            elif this.Name = typedefof<_ array>.Name then Some (Reflection.objsToArray this [||])
            elif this.Name = typedefof<_ list>.Name then Some (Reflection.objsToList this [])
            elif this.Name = typedefof<_ Set>.Name then Some (Reflection.objsToSet this Set.empty)
            elif this.Name = typedefof<Map<_, _>>.Name then Some (Reflection.pairsToMap this Map.empty)
            elif FSharpType.IsUnion this then
                let unionCases = FSharpType.GetUnionCases this
                if (unionCases.[0].GetFields ()).Length = 0
                then Some (FSharpValue.MakeUnion (unionCases.[0], [||]))
                else None
            elif notNull (this.GetConstructor [||]) then Some (Activator.CreateInstance ())
            elif this.IsValueType then Some (Activator.CreateInstance ())
            else None

        /// Special overload to get the public instance properties of a type.
        /// Thread-safe.
        member this.GetProperties cached =
            if cached
            then getProperties this
            else this.GetProperties ()

        /// Get the default value for a type.
        /// Never returns null.
        /// Thread-safe if value ctor is.
        member this.GetDefaultValue () =
            match this.TryGetDefaultValue () with
            | Some value -> value
            | None -> failwith ("Could not get default value for type '" + this.Name + "'.")

        /// Get the type descriptor for this type as returned by the global TypeDescriptor.
        /// Thread-safe.
        member this.GetTypeDescriptor () =
            (TypeDescriptor.GetProvider this).GetTypeDescriptor this

        /// Try to get a custom type converter for the given type.
        /// Thread-safe if type converter ctor is.
        member this.TryGetCustomTypeConverter () =
            let globalConverterAttributes =
                [| for attribute in TypeDescriptor.GetAttributes this do
                    match attribute with
                    | :? TypeConverterAttribute as tca -> yield tca
                    | _ -> () |]
            let typeConverterAttributes =
                this.GetCustomAttributes (typeof<TypeConverterAttribute>, true) |>
                Array.map (fun attr -> attr :?> TypeConverterAttribute) |>
                Array.append globalConverterAttributes
            if not (Array.isEmpty typeConverterAttributes) then
                let typeConverterAttribute = Array.head typeConverterAttributes
                let typeConverterTypeName = typeConverterAttribute.ConverterTypeName
                let typeConverterType = Type.GetType typeConverterTypeName
                match typeConverterType.GetConstructor [|typeof<Type>|] with
                | null -> (typeConverterType.GetConstructor [||]).Invoke [||] :?> TypeConverter |> Some
                | constructor1 -> constructor1.Invoke [|this|] :?> TypeConverter |> Some
            else None

        /// Get a property with the given name that can be written to, or null.
        /// Thread-safe.
        member this.GetPropertyWritable propertyName =
            match PropertiesWritable.TryGetValue ((propertyName, this)) with
            | (true, result) -> result
            | (false, _) ->
                let propertyOpt =
                    Array.tryFind
                        (fun (property : PropertyInfo) -> property.Name = propertyName && property.CanWrite)
                        (this.GetProperties ())
                let result =
                    match propertyOpt with
                    | Some property -> property
                    | None -> null
                PropertiesWritable.TryAdd ((propertyName, this), result) |> ignore<bool>
                result

        /// Get all the properties that are signalled to be preferred by the 'preference' predicate.
        /// Thread-safe.
        member this.GetPropertiesByPreference preference =
            let propertiesGrouped =
                Array.groupBy
                    (fun (property : PropertyInfo) -> property.Name)
                    (this.GetProperties true)
            let propertyOpts =
                Array.map
                    (fun (_, properties) -> Type.GetPropertyByPreference (preference, properties))
                    propertiesGrouped
            Array.filter notNull propertyOpts

        /// Get all the properties, preferring those that can be written to if there is a name clash.
        /// Thread-safe.
        member this.GetPropertiesPreferWritable () =
            this.GetPropertiesByPreference (fun (property : PropertyInfo) -> property.CanWrite)

        /// Get all the properties that can be written to.
        /// Thread-safe.
        member this.GetPropertiesWritable () =
            match PropertiesWritableArray.TryGetValue this with
            | (true, properties) -> properties
            | (false, _) ->
                let properties =
                    Array.filter
                        (fun (property : PropertyInfo) -> property.CanWrite)
                        (this.GetProperties true)
                PropertiesWritableArray.TryAdd (this, properties) |> ignore<bool>
                properties

        /// Get the generic name of the type, EG - Option<String>
        /// Thread-safe.
        member this.GetGenericName () : string =
            let sb = StringBuilder ()
            let name = this.Name
            if this.IsGenericType then
                let gargs = this.GetGenericArguments () |> Array.map (fun garg -> garg.GetGenericName ())
                ignore (sb.Append (name.Substring (0, name.IndexOf '`')))
                ignore (sb.Append "<")
                ignore (sb.Append (String.Join (", ", gargs)))
                ignore (sb.Append ">")
                sb.ToString ()
            else name

namespace FSharp.Reflection
open System

[<RequireQualifiedAccess>]
module FSharpType =

    /// Check that the given type has null as an actual value.
    /// Thread-safe.
    let isNullTrueValue (ty : Type) =
        let isUnit = ty = typeof<unit>
        let isNullTrueValueByAttribute =
            ty.GetCustomAttributes(typeof<CompilationRepresentationAttribute>, true) |>
            Array.map (fun (attr : obj) -> attr :?> CompilationRepresentationAttribute) |>
            Array.exists (fun attr -> int attr.Flags &&& int CompilationRepresentationFlags.UseNullAsTrueValue <> 0)
        let result = isUnit || isNullTrueValueByAttribute
        result

    /// Check that a record is considered abstract.
    /// Thread-safe.
    let isRecordAbstract (ty : Type) =
        ty.GetCustomAttributes(typeof<CompilationMappingAttribute>, true) |>
        Array.map (fun (attr : obj) -> attr :?> CompilationMappingAttribute) |>
        Array.exists (fun attr -> int attr.SourceConstructFlags = (int SourceConstructFlags.RecordType ||| int SourceConstructFlags.NonPublicRepresentation))

    /// Check that a union is considered abstract.
    /// Thread-safe.
    let isUnionAbstract (ty : Type) =
        ty.GetCustomAttributes(typeof<CompilationMappingAttribute>, true) |>
        Array.map (fun (attr : obj) -> attr :?> CompilationMappingAttribute) |>
        Array.exists (fun attr -> int attr.SourceConstructFlags = (int SourceConstructFlags.SumType ||| int SourceConstructFlags.NonPublicRepresentation))
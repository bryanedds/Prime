// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open System.Linq
open FSharp.Reflection
open Prime

[<AutoOpen>]
module Operators =

    /// Memoized union tags.
    let private TagsMemo = Dictionary<obj, int> HashIdentity.Structural

    /// The constant function.
    /// No matter what you pass it, it evaluates to the first argument.
    let constant a _ = a

    /// The constend function.
    /// No matter what you pass it, it evaluates to the second argument.
    let constend _ b = b

    /// The tautology function.
    /// No matter what you pass it, it evaluates to true.
    let tautology _ = true

    /// The tautology function with two arguments.
    /// No matter what you pass it, it evaluates to true.
    let tautology2 _ _ = true

    /// The tautology function with three arguments.
    /// No matter what you pass it, it evaluates to true.
    let tautology3 _ _ _ = true

    /// The absurdity function.
    /// No matter what you pass it, it evaluates to false.
    let absurdity _ = false

    /// The absurdity function with two arguments.
    /// No matter what you pass it, it evaluates to false.
    let absurdity2 _ _ = false

    /// Curry up two values.
    let inline curry f a b = f (a, b)

    /// Uncurry two values.
    let inline uncurry f (a, b) = f a b

    /// Transforms a function by flipping the order of its arguments.
    let inline flip f a b = f b a

    /// Transforms a function by flipping the order of its arguments.
    let inline flip3 f a b c = f c a b

    /// Transforms a function by flipping the order of its arguments.
    let inline flip4 f a b c d = f d a b c

    /// Transforms a function by flipping the order of its arguments.
    let inline flip5 f a b c d e = f e a b c d

    /// Apply a function to an argument.
    let inline apply f a = f a

    /// Test for null.
    let inline isNull a = match a with null -> true | _ -> false

    /// Test for non-null.
    let inline notNull a = match a with null -> false | _ -> true

    /// Get the .NET type of a target.
    let inline getType<'a> (target : 'a) = target.GetType ()

    /// Get the .NET type name of a target.
    let inline getTypeName<'a> target = (getType<'a> target).Name

    /// Get the union tag for the give case value.
    /// OPTIMIZATION: memoizes zero-field unions for speed.
    let getTag<'u> (unionCase : 'u) =
        match TagsMemo.TryGetValue unionCase with
        | (true, tag) -> tag
        | (false, _) ->
            let (unionCaseInfo, _) = FSharpValue.GetUnionFields (unionCase, typeof<'u>)
            let tag = unionCaseInfo.Tag
            if Array.isEmpty (unionCaseInfo.GetFields ()) then TagsMemo.Add (unionCase, tag)
            tag

    /// (=) as a function.
    let inline eq<'a when 'a : equality> (left : 'a) (right : 'a) = left = right

    /// Compare two strings.
    let inline strCmp str str2 = String.CompareOrdinal (str, str2)

    /// Test for string equality.
    let inline strEq str str2 = String.Equals (str, str2)

    /// Test for string inequality.
    let inline strNeq str str2 = strCmp str str2 <> 0

    /// Test for object equality.
    /// OPTIMIZATION: always tests reference equality first.
    let inline objEq (a : obj) (b : obj) =
        match a with
        | :? Array -> a = b // NOTE: arrays are given special deep equality semantics in F#.
        | _ -> obj.ReferenceEquals (a, b) || obj.Equals (a, b)

    /// Test for object inequality.
    /// OPTIMIZATION: always tests reference equality first.
    let inline objNeq (a : obj) (b : obj) =
        match a with
        | :? Array -> a <> b // NOTE: arrays are given special deep equality semantics in F#.
        | _ -> not (obj.ReferenceEquals (a, b) || obj.Equals (a, b))

    /// Test for reference equality.
    let inline refEq<'a> (a : 'a) (b : 'a) = obj.ReferenceEquals (a, b)

    /// Test for reference inequality.
    let inline refNeq<'a> (a : 'a) (b : 'a) = not (obj.ReferenceEquals (a, b))

    /// Test for equality generically, usually faster than (=).
    let inline genEq<'a when 'a : equality> (a : 'a) (b : 'a) = LanguagePrimitives.GenericEquality a b

    /// Test for inequality generically, usually faster than (=).
    let inline genNeq<'a when 'a : equality> (a : 'a) (b : 'a) = not (LanguagePrimitives.GenericEquality a b)

    /// Inspect two options for equality.
    let inline optEq aOpt bOpt =
        match aOpt with
        | Some a -> match bOpt with Some b -> a = b | None -> false
        | None -> match bOpt with Some _ -> false | None -> true

    /// Inspect two options for inequality.
    let inline optNeq aOpt bOpt =
        not (optEq aOpt bOpt)

    /// Inspect two voptions for equality.
    let inline voptEq aOpt bOpt =
        match aOpt with
        | ValueSome a -> match bOpt with ValueSome b -> a = b | ValueNone -> false
        | ValueNone -> match bOpt with ValueSome _ -> false | ValueNone -> true

    /// Inspect two voptions for inequality.
    let inline voptNeq aOpt bOpt =
        not (voptEq aOpt bOpt)

    /// Test for sequence equality.
    let inline seqEq<'a> (seq : 'a seq) (seq2 : 'a seq) = Enumerable.SequenceEqual (seq, seq2)

    /// Test for sequence equality.
    let inline seqNeq<'a> (seq : 'a seq) (seq2 : 'a seq) = not (Enumerable.SequenceEqual (seq, seq2))

    /// Cast as a function.
    let inline cast<'a> (target : obj) =
        target :?> 'a

    /// Rotate bits left.
    let rotl bits (i : uint32) =
        (i <<< bits) ||| (i >>> (32 - bits))

    /// Rotate bits right.
    let rotr bits (i : uint32) =
        (i >>> bits) ||| (i <<< (32 - bits))

    /// Rotate bits left.
    let rotl64 bits (i : uint64) =
        (i <<< bits) ||| (i >>> (64 - bits))

    /// Rotate bits right.
    let rotr64 bits (i : uint64) =
        (i >>> bits) ||| (i <<< (64 - bits))

    /// Short-hand for linq enumerable cast.
    let inline enumerable<'a> enumeratable =
        System.Linq.Enumerable.Cast<'a> enumeratable

    /// Get the enumerator for a sequence.
    let inline enumerator (enumeratable : _ seq) =
        enumeratable.GetEnumerator ()

    /// Convert a value to an Enum.
    let inline enum<'a, 'e when 'e : enum<'a>> (value : 'a) =
        LanguagePrimitives.EnumOfValue<'a, 'e> value

    /// Add a custom TypeConverter to an existing type.
    let assignTypeConverter<'t, 'c> () =
        TypeDescriptor.AddAttributes (typeof<'t>, TypeConverterAttribute typeof<'c>) |> ignore

    /// The bracket function for automatic resource handling.
    let bracket make action destroy =
        let resource = make ()
        let result =
            try action resource
            finally destroy resource
        result

    /// Make a Guid.
    let inline makeGuid () =
        Guid.NewGuid ()

    /// Fail with an unexpected match failure.
    let failwithumf () =
        let stackTrace = StackTrace ()
        let frame = stackTrace.GetFrame 1
        let meth = frame.GetMethod ()
        let line = frame.GetFileLineNumber ()
        let fileName = frame.GetFileName ()
        failwithf "Unexpected match failure in '%s' on line %i in file %s." meth.Name line fileName

    /// Fail with a 'NotImplementedException'.
    let failwithnie () =
        let stackTrace = StackTrace ()
        let frame = stackTrace.GetFrame 1
        let meth = frame.GetMethod ()
        let line = frame.GetFileLineNumber ()
        let fileName = frame.GetFileName ()
        raise (NotImplementedException (sprintf "Not implemented exception in '%s' on line %i in file %s." meth.Name line fileName))

    /// Test for object equality.
    /// OPTIMIZATION: always tests reference equality first.
    let inline (===) (a : obj) (b : obj) =
        objEq a b

    /// Test for object inequality.
    /// OPTIMIZATION: always tests reference inequality first.
    let inline (=/=) (a : obj) (b : obj) =
        objNeq a b

    /// Sequences two functions like Haskell ($).
    let inline ($) f g = f g

namespace System.IO
open System

// TODO: Find a better place for this?
[<RequireQualifiedAccess>]
module Path =

    /// Simplify a path.
    let Simplify (path : string) = Uri(Uri("http://example.com/"), path).AbsolutePath.TrimStart('/') |> Uri.UnescapeDataString
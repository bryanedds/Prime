// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =

    /// Make a dictionary with a single entry.
    let inline singleton key value =
        List.toDict [(key, value)]

    /// Map over a dictionary. A new dictionary is produced.
    let map (mapper : KeyValuePair<'k, 'v> -> 'v) (dictionary : Dictionary<'k, 'v>) =
        let result = Dictionary<'k, 'v> dictionary.Comparer
        for kvp in dictionary do result.Add (kvp.Key, mapper kvp)
        result

    /// Try to find a value in a dictonary.
    let inline tryFind key (dictionary : Dictionary<'k, 'v>) =
        match dictionary.TryGetValue key with
        | (true, value) -> Some value
        | (false, _) -> None

    /// Try to get a value in a dictonary without allocating.
    let inline tryGetValue (key, dictionary : Dictionary<'k, 'v>, value : 'v byref) =
        dictionary.TryGetValue (key, &value)

[<AutoOpen>]
module DictionaryExtension =

    /// Dictionary extension methods.
    type Dictionary<'k, 'v> with

        /// Force the addition of an entry, replacing the existing one if necessary.
        member inline this.ForceAdd (key, value) =
            this.[key] <- value

        /// Try to add an entry, returning false upon failure.
        member inline this.TryAdd (key, value) =
            if not (this.ContainsKey key)
            then this.Add (key, value); true
            else false

[<AutoOpen>]
module DictionaryOperators =

    /// Like dict, but returns a concrete Dictionary instance with structural hashing.
    /// NOTE: Also uses forced adding, allowing multiple of the same key in the kvps.
    let dictPlus<'k, 'v when 'k : equality> (kvps : ('k * 'v) seq) =
        let dictionary = Dictionary<'k, 'v> HashIdentity.Structural
        for (key, value) in kvps do dictionary.ForceAdd (key, value)
        dictionary
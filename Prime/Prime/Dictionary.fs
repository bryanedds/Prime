// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =

    /// Make a dictionary with a single entry.
    let inline singleton comparer key value =
        List.toDict comparer [(key, value)]

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
    let inline tryGetValue (key, dictionary : Dictionary<'k, 'v>, value : 'v outref) =
        dictionary.TryGetValue (key, &value)
        
    /// Hash a dictionary.
    let hash (dictionary : Dictionary<_, _>) =
        let mutable h = 0
        for entry in dictionary do
            h <- h ^^^ entry.Key.GetHashCode () ^^^ (entry.Value.GetHashCode () * 13)
        h

[<AutoOpen>]
module DictionaryExtension =

    /// Dictionary extension methods.
    type Dictionary<'k, 'v> with

        /// Try to add a keyed value, returning false if the key is already present.
        member inline this.TryAdd (key, value) =
            if not (this.ContainsKey key)
            then this.Add (key, value); true
            else false

        /// Convert entries to struct pairs.
        member this.Pairs =
            this |> Seq.map (fun entry -> struct (entry.Key, entry.Value))

[<AutoOpen>]
module DictionaryOperators =

    /// Like dict, but returns a concrete Dictionary instance with structural hashing.
    /// NOTE: Also uses forced adding, allowing multiple of the same key in the kvps.
    let dictPlus<'k, 'v> (comparer : 'k IEqualityComparer) (kvps : ('k * 'v) seq) =
        let dictionary = Dictionary<'k, 'v> comparer
        for (key, value) in kvps do dictionary.[key] <- value
        dictionary

    /// Convert entries to struct pairs.
    let inline pairs (dict : Dictionary<'k, 'v>) = dict.Pairs
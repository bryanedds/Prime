// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SDictionary =

    /// A dictionary that is split into 32 smaller dictionaries in order to avoid allocating from the LOH.
    type [<ReferenceEquality>] SDictionary<'k, 'v> =
        private
            { Dictionaries_ : Dictionary<'k, 'v> array
              Comparer_ : 'k IEqualityComparer }

        member this.Count =
            let mutable count = 0
            for set in this.Dictionaries_ do
                count <- count + set.Count
            count

        member this.Comparer =
            this.Comparer_

        member this.Item
            with get (key : 'k) =
                let hashCode = this.Comparer_.GetHashCode key
                let index = Math.Abs (hashCode % 32)
                this.Dictionaries_.[index].[key]
            and set (key : 'k) (value : 'v) =
                let hashCode = this.Comparer_.GetHashCode key
                let index = Math.Abs (hashCode % 32)
                this.Dictionaries_.[index].[key] <- value

        member this.ContainsKey key =
            let hashCode = this.Comparer_.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            this.Dictionaries_.[index].ContainsKey key

        member this.TryFind key =
            let hashCode = this.Comparer_.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            match this.Dictionaries_.[index].TryGetValue key with
            | (true, value) -> Some value
            | (false, _) -> None

        member this.TryGetValue (key, valueRef : _ outref) =
            let hashCode = this.Comparer_.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            this.Dictionaries_.[index].TryGetValue (key, &valueRef)

        member this.Add (key, value) =
            let hashCode = this.Comparer_.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            this.Dictionaries_.[index].Add (key, value)

        member this.Remove key =
            let hashCode = this.Comparer_.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            this.Dictionaries_.[index].Remove key

        member this.Clear () =
            for dict in this.Dictionaries_ do
                dict.Clear ()

        member this.GetEnumerator () =
            (Seq.concat this.Dictionaries_).GetEnumerator ()

        interface IEnumerable<KeyValuePair<'k, 'v>> with
            member this.GetEnumerator () = (Seq.concat this.Dictionaries_).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.Dictionaries_).GetEnumerator () :> IEnumerator

    let make (comparer : 'k IEqualityComparer) =
        let dicts = Array.init 32 (fun _ -> Dictionary<'k, 'v> comparer)
        { Dictionaries_ = dicts
          Comparer_ = comparer }

    let makeFromSegmentedDictionary (sdict : SDictionary<'k, 'v>) =
        { Dictionaries_ = Array.init 32 (fun i -> Dictionary<'k, 'v> (sdict.Dictionaries_.[i], sdict.Comparer_))
          Comparer_ = sdict.Comparer_ }

    let count (sdict : SDictionary<'k, 'v>) =
        sdict.Count

    let isEmpty sdict =
        count sdict = 0

    let notEmpty sdict =
        count sdict > 0

    let containsKey key (sdict : SDictionary<'k, 'v>) =
        sdict.ContainsKey key

    let tryFind (key, sdict : SDictionary<'k, 'v>) =
        sdict.TryFind key

    let tryGetValue (key, sdict : SDictionary<'k, 'v>, valueRef : _ outref) =
        sdict.TryGetValue (key, valueRef)

    let add key value (sdict : SDictionary<'k, 'v>) =
        sdict.Add (key, value)

    let remove key (sdict : SDictionary<'k, 'v>) =
        sdict.Remove key

    let clear (sdict : SDictionary<'k, 'v>) =
        sdict.Clear ()

    let toSeq sdict =
        sdict.Dictionaries_ |>
        Seq.concat |>
        Seq.map (fun kvp -> (kvp.Key, kvp.Value))

    let ofSeq comparer seq =
        let sdict = make comparer
        for (k, v) in seq do add k v sdict
        sdict

    let singleton comparer key value =
        let sdict = make comparer
        add key value sdict
        sdict

    let map<'k, 'v, 'u when 'k : equality and 'u : equality> comparer (mapper : 'k -> 'v -> 'u) (sdict : SDictionary<'k, 'v>) =
        toSeq sdict |>
        Seq.map (fun (k, v) -> (k, mapper k v)) |>
        ofSeq comparer

    let filter pred sdict =
        ofSeq sdict.Comparer_ (Seq.filter pred (toSeq sdict))

    let fold folder sdict =
        Seq.fold folder (toSeq sdict)

/// A dictionary that is split into 32 smaller dictionaries in order to avoid allocating from the LOH.
type SDictionary<'k, 'v> = SDictionary.SDictionary<'k, 'v>
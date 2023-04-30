// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

// TODO: document this!

[<RequireQualifiedAccess>]
module SHashSet =

    type [<ReferenceEquality>] SHashSet<'a when 'a : equality> =
        private
            { HashSets_ : 'a HashSet array
              Comparer_ : 'a IEqualityComparer }

        member this.Count =
            let mutable count = 0
            for set in this.HashSets_ do
                count <- count + set.Count
            count

        member this.Comparer =
            this.Comparer_

        member this.Contains item =
            let hashCode = hash item
            let index = Math.Abs (hashCode % 32)
            this.HashSets_.[index].Contains item

        member this.Add item =
            let hashCode = hash item
            let index = Math.Abs (hashCode % 32)
            this.HashSets_.[index].Add item

        member this.Remove item =
            let hashCode = hash item
            let index = Math.Abs (hashCode % 32)
            this.HashSets_.[index].Remove item

        member this.Clear () =
            for set in this.HashSets_ do
                set.Clear ()

        member this.SetEquals hashSet =
            let mutable result = true
            for i in 0 .. dec 32 do
                result <- result && this.HashSets_.[i].SetEquals hashSet.HashSets_.[i]
            result

        member this.UnionWith hashSet =
            for i in 0 .. dec 32 do
                this.HashSets_.[i].UnionWith hashSet.HashSets_.[i]

        member this.IntersectWith hashSet =
            for i in 0 .. dec 32 do
                this.HashSets_.[i].IntersectWith hashSet.HashSets_.[i]

        member this.SymmetricExceptWith hashSet =
            for i in 0 .. dec 32 do
                this.HashSets_.[i].SymmetricExceptWith hashSet.HashSets_.[i]

        member this.ExceptWith hashSet =
            for i in 0 .. dec 32 do
                this.HashSets_.[i].ExceptWith hashSet.HashSets_.[i]

        member this.GetEnumerator () =
            (Seq.concat this.HashSets_).GetEnumerator ()

        interface 'a IEnumerable with
            member this.GetEnumerator () = (Seq.concat this.HashSets_).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.HashSets_).GetEnumerator () :> IEnumerator

    let make (comparer : 'a IEqualityComparer) =
        let hashSets = Array.init 32 (fun _ -> HashSet<'a> comparer)
        { HashSets_ = hashSets
          Comparer_ = comparer }

    let makeFromSegmentedHashSet (sset : SHashSet<'a>) =
        { HashSets_ = Array.init 32 (fun i -> HashSet<'a> (sset.HashSets_.[i], sset.Comparer_))
          Comparer_ = sset.Comparer_ }

    let count (sset : 'a SHashSet) =
        sset.Count

    let isEmpty sset =
        count sset = 0

    let notEmpty sset =
        count sset > 0

    let contains item (sset : 'a SHashSet) =
        sset.Contains item

    let add item (sset : 'a SHashSet) =
        sset.Add item

    let remove item (sset : 'a SHashSet) =
        sset.Remove item

    let clear (sset : 'a SHashSet) =
        sset.Clear ()

    let toSeq sset =
        Seq.concat sset.HashSets_

    let ofSeq comparer seq =
        let sset = make comparer
        for item in seq do add item sset |> ignore<bool>
        sset

    let singleton comparer item =
        let sset = make comparer
        add item sset |> ignore<bool>
        sset

    let map<'a, 'b when 'a : equality and 'b : equality> comparer (mapper : 'a -> 'b) (sset : 'a SHashSet) =
        ofSeq comparer (Seq.map mapper (toSeq sset))

    let filter pred sset =
        ofSeq sset.Comparer_ (Seq.filter pred (toSeq sset))

    let fold folder sset =
        Seq.fold folder (toSeq sset)

type SHashSet<'a when 'a : equality> = SHashSet.SHashSet<'a>
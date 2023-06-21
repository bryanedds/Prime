// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

// TODO: document this!

[<RequireQualifiedAccess>]
module SUSet =

    type [<ReferenceEquality>] SUSet<'a when 'a : equality> =
        private
            { mutable Set : 'a STSet }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () =
                let struct (seq, tset) = STSet.toSeq this.Set
                this.Set <- tset
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a seq).GetEnumerator () :> IEnumerator

    let makeFromSeq<'a when 'a : equality> comparer config items =
        { Set = STSet.makeFromSeq<'a> comparer config items }

    let makeEmpty<'a when 'a : equality> comparer config =
        { Set = STSet.makeEmpty<'a> comparer config }

    let getComparer set =
        let struct (result, tset) = STSet.getComparer set.Set
        set.Set <- tset
        result

    let getConfig set =
        let struct (result, tset) = STSet.getConfig set.Set
        set.Set <- tset
        result

    let add value set =
        { Set = STSet.add value set.Set }

    let remove value set =
        { Set = STSet.remove value set.Set }

    let clear set =
        { Set = STSet.clear set.Set }

    /// Add all the given values to the set.
    let addMany values set =
        { Set = STSet.addMany values set.Set }

    /// Remove all the given values from the set.
    let removeMany values set =
        { Set = STSet.removeMany values set.Set }

    let length set =
        let struct (result, tset) = STSet.length set.Set
        set.Set <- tset
        result

    let isEmpty set =
        let struct (result, tset) = STSet.isEmpty set.Set
        set.Set <- tset
        result

    let notEmpty set =
        not (isEmpty set)

    let contains value set =
        let struct (result, tset) = STSet.contains value set.Set
        set.Set <- tset
        result

    let toSeq (set : _ SUSet) =
        set :> _ seq

    let toHashSet (set : _ SUSet) =
        let struct (hashSet, tset) = STSet.toHashSet set.Set
        set.Set <- tset
        hashSet

    let ofSeq comparer config values =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty comparer config)
            values

    let fold folder state set =
        let struct (result, tset) = STSet.fold folder state set.Set
        set.Set <- tset
        result

    let map mapper set =
        let struct (result, tset) = STSet.map mapper set.Set
        set.Set <- tset
        { Set = result }

    let filter pred set =
        let struct (result, tset) = STSet.filter pred set.Set
        set.Set <- tset
        { Set = result }

    let equals set set2 =
        let struct (result, tset, tset2) = STSet.equals set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    let unionFast set set2 =
        let struct (result, tset, tset2) = STSet.unionFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    let intersectFast set set2 =
        let struct (result, tset, tset2) = STSet.intersectFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    let disjointFast set set2 =
        let struct (result, tset, tset2) = STSet.disjointFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    let differenceFast set set2 =
        let struct (result, tset, tset2) = STSet.differenceFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    let union config set set2 =
        let struct (result, tset, tset2) = STSet.union config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    let intersect config set set2 =
        let struct (result, tset, tset2) = STSet.intersect config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    let disjoint config set set2 =
        let struct (result, tset, tset2) = STSet.disjoint config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    let difference config set set2 =
        let struct (result, tset, tset2) = STSet.difference config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    let singleton<'a when 'a : equality> comparer config item =
        { Set = STSet.singleton<'a> comparer config item }

type SUSet<'a when 'a : equality> = 'a SUSet.SUSet
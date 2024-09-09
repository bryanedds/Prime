// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SUSet =

    /// A hashing set that supports transaction-based rewinding with a more convenient interface than STSet.
    type [<ReferenceEquality; DefaultValue "[]">] SUSet<'a when 'a : equality> =
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

    /// Create a SUSet containing the given sequence of values.
    let makeFromSeq<'a when 'a : equality> comparer config items =
        { Set = STSet.makeFromSeq<'a> comparer config items }

    /// Create an empty SUSet.
    let makeEmpty<'a when 'a : equality> comparer config =
        { Set = STSet.makeEmpty<'a> comparer config }

    /// Get the comparer function used to determine uniqueness in a SUSet.
    let getComparer set =
        let struct (result, tset) = STSet.getComparer set.Set
        set.Set <- tset
        result

    /// Get the semantic configuration of the SUSet.
    let getConfig set =
        let struct (result, tset) = STSet.getConfig set.Set
        set.Set <- tset
        result

    /// Add an element to a SUSet.
    let add value set =
        { Set = STSet.add value set.Set }

    /// Remove all matching elements from a SUSet.
    let remove value set =
        { Set = STSet.remove value set.Set }

    /// Remove all elements from a SUSet.
    let clear set =
        { Set = STSet.clear set.Set }

    /// Check that a SUSet has no elements.
    let isEmpty set =
        let struct (result, tset) = STSet.isEmpty set.Set
        set.Set <- tset
        result

    /// Check that a SUSet has one or more elements.
    let notEmpty set =
        not (isEmpty set)

    /// Get the length of a SUSet (constant-time).
    let length set =
        let struct (result, tset) = STSet.length set.Set
        set.Set <- tset
        result

    /// Determine that a SUSet contains the given value.
    let contains value set =
        let struct (result, tset) = STSet.contains value set.Set
        set.Set <- tset
        result

    /// Add all the given values to a SUSet.
    let addMany values set =
        { Set = STSet.addMany values set.Set }

    /// Remove all the given values from a SUSet.
    let removeMany values set =
        { Set = STSet.removeMany values set.Set }
        
    /// Make a SUSet from a sequence of values.
    let ofSeq comparer config values =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty comparer config)
            values

    /// Convert a SUSet to a seq. Note that entire set is iterated eagerly since the underlying SHashSet could
    /// otherwise opaquely change during iteration.
    let toSeq (set : _ SUSet) =
        set :> _ seq

    /// Convert a SUSet to a SHashSet.
    let toHashSet (set : _ SUSet) =
        let struct (hashSet, tset) = STSet.toHashSet set.Set
        set.Set <- tset
        hashSet

    /// Fold over the elements of a SUSet.
    let fold folder state set =
        let struct (result, tset) = STSet.fold folder state set.Set
        set.Set <- tset
        result

    /// Map the elements of a SUSet.
    let map mapper set =
        let struct (result, tset) = STSet.map mapper set.Set
        set.Set <- tset
        { Set = result }

    /// Filter the elements of a SUSet.
    let filter pred set =
        let struct (result, tset) = STSet.filter pred set.Set
        set.Set <- tset
        { Set = result }

    /// Determine equality of two SUSets.
    let equals set set2 =
        let struct (result, tset, tset2) = STSet.equals set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct a union SHashSet.
    let unionFast set set2 =
        let struct (result, tset, tset2) = STSet.unionFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct an intersection SHashSet.
    let intersectFast set set2 =
        let struct (result, tset, tset2) = STSet.intersectFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct a disjoint SHashSet.
    let disjointFast set set2 =
        let struct (result, tset, tset2) = STSet.disjointFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct a difference SHashSet.
    let differenceFast set set2 =
        let struct (result, tset, tset2) = STSet.differenceFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct a union SUSet.
    let union config set set2 =
        let struct (result, tset, tset2) = STSet.union config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    /// Construct an intersection SUSet.
    let intersect config set set2 =
        let struct (result, tset, tset2) = STSet.intersect config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    /// Construct a disjoint SUSet.
    let disjoint config set set2 =
        let struct (result, tset, tset2) = STSet.disjoint config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    /// Construct a difference SUSet.
    let difference config set set2 =
        let struct (result, tset, tset2) = STSet.difference config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    /// Make a SUSet with a single element.
    let singleton<'a when 'a : equality> comparer config item =
        { Set = STSet.singleton<'a> comparer config item }

/// A hashing set that supports transaction-based rewinding with a more convenient interface than STSet.
type SUSet<'a when 'a : equality> = 'a SUSet.SUSet
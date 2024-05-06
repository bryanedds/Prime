// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module USet =

    /// A hashing set that supports transaction-based rewinding with a more convenient interface than TSet.
    type [<ReferenceEquality>] 'a USet =
        private
            { mutable Set : 'a TSet }

        /// Determine that a USet contains the given value.
        member this.Contains value =
            let struct (result, tset) = TSet.contains value this.Set
            this.Set <- tset
            result

        interface 'a IEnumerable with
            member this.GetEnumerator () =
                let struct (seq, tset) = TSet.toSeq this.Set
                this.Set <- tset
                seq.GetEnumerator ()

        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a seq).GetEnumerator () :> IEnumerator

    /// Create a USet containing the given sequence of values.
    let makeFromSeq<'a> comparer config items =
        { Set = TSet.makeFromSeq<'a> comparer config items }

    /// Create an empty USet.
    let makeEmpty<'a> comparer config =
        { Set = TSet.makeEmpty<'a> comparer config }

    /// Get the comparer function used to determine uniqueness in a USet.
    let getComparer set =
        let struct (result, tset) = TSet.getComparer set.Set
        set.Set <- tset
        result

    /// Get the semantic configuration of the USet.
    let getConfig set =
        let struct (result, tset) = TSet.getConfig set.Set
        set.Set <- tset
        result

    /// Add an element to a USet.
    let add value set =
        { Set = TSet.add value set.Set }

    /// Remove all matching elements from a USet.
    let remove value set =
        { Set = TSet.remove value set.Set }

    /// Remove all elements from a USet.
    let clear set =
        { Set = TSet.clear set.Set }

    /// Check that a USet has no elements.
    let isEmpty set =
        let struct (result, tset) = TSet.isEmpty set.Set
        set.Set <- tset
        result

    /// Check that a USet has one or more elements.
    let notEmpty set =
        not (isEmpty set)

    /// Get the length of a USet (constant-time).
    let length set =
        let struct (result, tset) = TSet.length set.Set
        set.Set <- tset
        result

    /// Determine that a USet contains the given value.
    let contains value (set : 'a USet) =
        set.Contains value

    /// Add all the given values to a USet.
    let addMany values set =
        { Set = TSet.addMany values set.Set }

    /// Remove all the given values from a USet.
    let removeMany values set =
        { Set = TSet.removeMany values set.Set }
        
    /// Make a USet from a sequence of values.
    let ofSeq comparer config values =
        Seq.fold
            (fun map value -> add value map)
            (makeEmpty comparer config)
            values

    /// Convert a sequence of values to a USet assuming structural comparison and functional representation.
    let ofSeq1 pairs =
        ofSeq HashIdentity.Structural Functional pairs

    /// Convert a USet to a seq. Note that entire set is iterated eagerly since the underlying HashMap could
    /// otherwise opaquely change during iteration.
    let toSeq (set : _ USet) =
        set :> _ seq

    /// Convert a USet to a HashSet.
    let toHashSet (set : _ USet) =
        let struct (hashSet, tset) = TSet.toHashSet set.Set
        set.Set <- tset
        hashSet

    /// Fold over the elements of a USet.
    let fold folder state set =
        let struct (result, tset) = TSet.fold folder state set.Set
        set.Set <- tset
        result

    /// Map the elements of a USet.
    let map mapper set =
        let struct (result, tset) = TSet.map mapper set.Set
        set.Set <- tset
        { Set = result }

    /// Filter the elements of a USet.
    let filter pred set =
        let struct (result, tset) = TSet.filter pred set.Set
        set.Set <- tset
        { Set = result }

    /// Determine equality of two USets.
    let equals set set2 =
        let struct (result, tset, tset2) = TSet.equals set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct a union HashSet.
    let unionFast set set2 =
        let struct (result, tset, tset2) = TSet.unionFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct an intersection HashSet.
    let intersectFast set set2 =
        let struct (result, tset, tset2) = TSet.intersectFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct a disjoint HashSet.
    let disjointFast set set2 =
        let struct (result, tset, tset2) = TSet.disjointFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct a difference HashSet.
    let differenceFast set set2 =
        let struct (result, tset, tset2) = TSet.differenceFast set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        result

    /// Construct a union USet.
    let union config set set2 =
        let struct (result, tset, tset2) = TSet.union config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    /// Construct an intersection USet.
    let intersect config set set2 =
        let struct (result, tset, tset2) = TSet.intersect config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    /// Construct a disjoint USet.
    let disjoint config set set2 =
        let struct (result, tset, tset2) = TSet.disjoint config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    /// Construct a difference USet.
    let difference config set set2 =
        let struct (result, tset, tset2) = TSet.difference config set.Set set2.Set
        set.Set <- tset
        set2.Set <- tset2
        { Set = result }

    /// Make a USet with a single element.
    let singleton<'a> comparer config item =
        { Set = TSet.singleton<'a> comparer config item }

/// A hashing set that supports transaction-based rewinding with a more convenient interface than TSet.
type 'a USet = 'a USet.USet
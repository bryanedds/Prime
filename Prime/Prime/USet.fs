// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module USet =

    /// A hashing set that supports transaction-based rewinding with a more convenient interface than TSet.
    type [<ReferenceEquality; DefaultValue "[]">] 'a USet =
        private
            { mutable Set : 'a TSet }

        /// Get the length of a USet (constant-time).
        member this.Length =
            let struct (result, tset) = TSet.length this.Set
            this.Set <- tset
            result

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

        interface 'a ICollection with
            member this.IsReadOnly =
                false
            member this.Count =
                this.Length
            member this.Add item =
                let tset = TSet.add item this.Set
                this.Set <- tset
            member this.Remove item =
                let result = this.Contains item
                let tset = TSet.remove item this.Set
                this.Set <- tset
                result
            member this.Contains item =
                this.Contains item
            member this.Clear () =
                let tset = TSet.clear this.Set
                this.Set <- tset
            member this.CopyTo (array, arrayIndex) =
                let tset = TSet.copyTo array arrayIndex this.Set
                this.Set <- tset

    /// Create a USet containing the given sequence of values.
    let makeFromSeq<'a> comparer config items =
        { Set = TSet.makeFromSeq<'a> comparer config items }

    /// Create an empty USet.
    let makeEmpty<'a> comparer config =
        { Set = TSet.makeEmpty<'a> comparer config }

    /// Get the comparer function used to determine uniqueness in a USet.
    let comparer set =
        let struct (result, tset) = TSet.comparer set.Set
        set.Set <- tset
        result

    /// Get the semantic configuration of the USet.
    let config set =
        let struct (result, tset) = TSet.config set.Set
        set.Set <- tset
        result

    /// Add an element to a USet.
    let add value set =
        { Set = TSet.add value set.Set }

    /// Remove all matching elements from a USet.
    let remove value set =
        { Set = TSet.remove value set.Set }

    /// Copy the elements of a USet to an array, starting at the given index.
    let copyTo (array : 'a array, arrayIndex : int) (set : 'a USet) =
        let tset = TSet.copyTo array arrayIndex set.Set
        set.Set <- tset

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

    /// Convert a USet to a seq. Note that the entire set is iterated eagerly when functional.
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

    /// Iterate over the elements of a USet.
    let iter action set =
        let tset = TSet.iter action set.Set
        set.Set <- tset

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
// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module USet =

    type [<NoEquality; NoComparison>] USet<'a when 'a : equality> =
        private
            { mutable Set : 'a TSet }
    
        interface IEnumerable<'a> with
            member this.GetEnumerator () =
                let struct (seq, tset) = TSet.toSeq this.Set
                this.Set <- tset
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'a>).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module USet =

        let makeFromSeq<'a when 'a : equality> config items =
            { Set = TSet.makeFromSeq<'a> config items }

        let makeEmpty<'a when 'a : equality> config =
            { Set = TSet.makeEmpty<'a> config }

        let getConfig set =
            let struct (result, tset) = TSet.getConfig set.Set
            set.Set <- tset
            result

        let add value set =
            { Set = TSet.add value set.Set }

        let remove value set =
            { Set = TSet.remove value set.Set }

        let clear set =
            { Set = TSet.clear set.Set }
    
        /// Add all the given values to the set.
        let addMany values set =
            { Set = TSet.addMany values set.Set }
    
        /// Remove all the given values from the set.
        let removeMany values set =
            { Set = TSet.removeMany values set.Set }

        let isEmpty set =
            let struct (result, tset) = TSet.isEmpty set.Set
            set.Set <- tset
            result

        let notEmpty set =
            not (isEmpty set)

        let contains value set =
            let struct (result, tset) = TSet.contains value set.Set
            set.Set <- tset
            result

        let toSeq (set : _ USet) =
            set :> _ seq

        let fold folder state set =
            let struct (result, tset) = TSet.fold folder state set.Set
            set.Set <- tset
            result

        let map mapper set =
            let struct (result, tset) = TSet.map mapper set.Set
            set.Set <- tset
            { Set = result }

        let filter pred set =
            let struct (result, tset) = TSet.filter pred set.Set
            set.Set <- tset
            { Set = result }

type USet<'a when 'a : equality> = USet.USet<'a>
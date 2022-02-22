// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System.Collections.Generic

[<RequireQualifiedAccess>]
module HashSet =

    /// Make a hash set with a single item.
    let inline singleton comparer item =
        List.toHashSet comparer [item]

[<AutoOpen>]
module HashSetOperators =

    /// Make a concrete HashSet instance populated with the given items and using vanilla hashing.
    let hashSet<'a> items =
        let hashSet = HashSet ()
        for item in items do hashSet.Add item |> ignore
        hashSet

    /// Make a concrete HashSet instance populated with the given items and using structural hashing.
    let hashSetPlus<'a> (comparer : 'a IEqualityComparer) items =
        let hashSet = HashSet comparer
        for item in items do hashSet.Add item |> ignore
        hashSet

    type 'a HashSet with

        member this.SetEqualsFast (that : 'a HashSet) =
            if this.Count = that.Count then
                let comparer = this.Comparer
                let mutable result = true
                let mutable enr = this.GetEnumerator ()
                let mutable enr2 = that.GetEnumerator ()
                let mutable enrGoing = enr.MoveNext ()
                let mutable enr2Going = enr2.MoveNext ()
                while enrGoing do
                    if enr2Going then
                        let item = enr.Current
                        let item2 = enr2.Current
                        if comparer.Equals (item, item2) then
                            enrGoing <- enr.MoveNext ()
                            enr2Going <- enr2.MoveNext ()
                        else
                            result <- false
                            enrGoing <- false
                    else
                        result <- false
                        enrGoing <- false
                result
            else false
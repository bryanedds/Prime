// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Linq

type EqualityComparerPlus<'TItem, 'TKey> (getKey) =

    inherit EqualityComparer<'TItem> ()

    let keyComparer = EqualityComparer<'TKey>.Default

    override this.Equals (x : 'TItem, y : 'TItem) =
        if isNull (box x) && isNull (box y) then true
        elif isNull (box x) || isNull (box y) then false
        else keyComparer.Equals (getKey x, getKey y)

    override this.GetHashCode (item : 'TItem) =
        if notNull (box item)
        then keyComparer.GetHashCode (getKey item)
        else 0

[<AutoOpen>]
module TopologicalSort =

    type IEnumerable<'T> with

        member source.RemapDependencies<'T, 'TKey> (getDependencies : 'T -> IEnumerable<'TKey>, getKey : 'T -> 'TKey) : 'T -> IEnumerable<'T> =
            let map = source.ToDictionary getKey
            fun item ->
                let dependencies = getDependencies item
                if notNull dependencies
                then dependencies.Select (fun key -> map.[key])
                else null

        member source.Group<'T, 'TKey> (getDependencies : 'T -> IEnumerable<'TKey>, getKey : 'T -> 'TKey) : (bool * IList<ICollection<'T>>) =
            let collection = match source with :? ICollection<'T> as collection -> collection | _ -> source.ToArray ()
            collection.Group<'T>(collection.RemapDependencies (getDependencies, getKey), null)

        member source.Group<'T, 'TKey> (getDependencies : 'T -> IEnumerable<'T>, getKey : 'T -> 'TKey) : (bool * IList<ICollection<'T>>) =
            let comparer = EqualityComparerPlus<'T, 'TKey> getKey :> IEqualityComparer<'T>
            source.Group<'T> (getDependencies, comparer)

        member source.Group<'T> (getDependencies : 'T -> IEnumerable<'T>, comparer : IEqualityComparer<'T>) : (bool * IList<ICollection<'T>>) =
            let mutable cycleFound = false
            let sorted = List<ICollection<'T>> ()
            let visited = Dictionary<'T, int> comparer
            for item in source do
                let (cycleFound2, _) = IEnumerable.Visit(item, getDependencies, sorted, visited)
                cycleFound <- cycleFound || cycleFound2
            (cycleFound, sorted)

        member source.Sort<'T, 'TKey> (getDependencies : 'T -> IEnumerable<'TKey>, getKey : 'T -> 'TKey) : 'T List =
            let collection = match source with :? ICollection<'T> as collection -> collection | _ -> source.ToArray ()
            collection.Sort<'T> (collection.RemapDependencies<'T, 'TKey> (getDependencies, getKey), null)

        member source.Sort<'T, 'TKey> (getDependencies : 'T -> IEnumerable<'T>, getKey : 'T -> 'TKey) : 'T List =
            source.Sort (getDependencies, EqualityComparerPlus<'T, 'TKey> getKey)

        member source.Sort<'T> (getDependencies : 'T -> IEnumerable<'T>, comparer : IEqualityComparer<'T>) : 'T List =
            let sorted = List<'T> ()
            let visited = Dictionary<'T, bool> comparer
            for item in source do
                IEnumerable.Visit (item, getDependencies, sorted, visited) |> ignore
            sorted

        static member Visit<'T> (item : 'T, getDependencies : 'T -> IEnumerable<'T>, sorted : List<ICollection<'T>>, visited : Dictionary<'T, int>) : (bool * int) =
            let mutable inProcess = -1
            let mutable cycleFound = false
            let mutable level = 0
            let alreadyVisited = visited.TryGetValue (item, &level)
            if alreadyVisited then
                if level = inProcess then
                    cycleFound <- true
            else
                level <- inProcess
                visited.[item] <- level
                let dependencies = getDependencies item
                if notNull dependencies then
                    for dependency in dependencies do
                        let (cycleFound2, depLevel) = IEnumerable.Visit (dependency, getDependencies, sorted, visited)
                        if cycleFound2 then cycleFound <- true
                        level <- Math.Max (level, depLevel)
                visited.[item] <- inc level
                while sorted.Count <= level do
                    sorted.Add (Collection<'T> ())
                sorted.[level].Add item
            (cycleFound, level)

        static member Visit<'T> (item : 'T, getDependencies : 'T -> IEnumerable<'T>, sorted : List<'T>, visited : Dictionary<'T, bool>) : bool =
            let mutable cycleFound = false
            let mutable inProcess = false
            let alreadyVisited = visited.TryGetValue (item, &inProcess)
            if alreadyVisited then
                if inProcess then
                    cycleFound <- true
            else
                visited.[item] <- true
                let dependencies = getDependencies item
                if notNull dependencies then
                    for dependency in dependencies do
                        IEnumerable.Visit (dependency, getDependencies, sorted, visited) |> ignore
                visited.[item] <- false
                sorted.Add item
            cycleFound
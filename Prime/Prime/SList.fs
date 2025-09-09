// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SList =

    /// An list that is split into smaller lists in order to avoid allocating from the LOH.
    type [<ReferenceEquality; DefaultValue "[]">] 'a SList =
        private
            { mutable TotalLength_ : int
              mutable Capacity_ : int
              mutable Lists_ : 'a List List }

        static member internal Make (capacity : int) =
            let size = sizeof<'a>
            let listCapacity = Constants.Runtime.LohSize / size / 2 // divide by two since we seem to need some major slop to avoid LOH allocation...
            let lists = List 1 // chances are we'll only need one sublist in most cases
            lists.Add (List<'a> (min capacity listCapacity))
            { TotalLength_ = 0; Capacity_ = listCapacity; Lists_ = lists }

        member this.Length =
            this.TotalLength_

        member this.Item
            with get (i : int) =
                if i < this.TotalLength_ then
                    let j = i / this.Capacity_
                    let k = i % this.Capacity_
                    this.Lists_.[j].[k]
                else raise (IndexOutOfRangeException "Index out of range.")
            and set (i : int) (value : 'a) =
                if i < this.TotalLength_ then
                    let j = i / this.Capacity_
                    let k = i % this.Capacity_
                    this.Lists_.[j].[k] <- value

        member this.GetEnumerator () =
            (Seq.concat this.Lists_).GetEnumerator ()

        member this.Add item =
            let lastList = this.Lists_.[dec this.Lists_.Count]
            if lastList.Count < this.Capacity_ then
                lastList.Add item
            else
                let newList = List this.Capacity_ // since we filled one list, let's presume to fill another
                newList.Add item
                this.Lists_.Add newList
            this.TotalLength_ <- inc this.TotalLength_

        member this.AddRange (seq : 'a seq) =
            for item in seq do
                this.Add item

        member this.Remove (item : 'a) =
            let mutable result = false
            let list' = SList.Make this.Lists_.Count
            for list in this.Lists_ do
                for item' in list do
                    if EqualityComparer.Equals (item, item')
                    then result <- true
                    else list'.Add item'
            this.TotalLength_ <- list'.TotalLength_
            this.Capacity_ <- list'.Capacity_
            this.Lists_ <- list'.Lists_
            result

        member this.Contains item =
            let mutable result = false
            let mutable enr = this.Lists_.GetEnumerator ()
            while not result && enr.MoveNext () do
                let list = enr.Current
                result <- list.Contains item
            result

        member this.Clear () =
            let firstList = this.Lists_.[0]
            firstList.Clear ()
            this.Lists_.Clear ()
            this.Lists_.Add firstList
            this.TotalLength_ <- 0

        member this.CopyTo (arr : 'a array, index : int) =
            let mutable i = 0
            let mutable enr = this.GetEnumerator ()
            while enr.MoveNext () do
                arr.[i + index] <- enr.Current
                i <- inc i

        interface 'a IEnumerable with
            member this.GetEnumerator () = (Seq.concat this.Lists_).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.Lists_).GetEnumerator () :> IEnumerator

        interface 'a ICollection with
            member this.IsReadOnly = false
            member this.Count = this.TotalLength_
            member this.Add item = this.Add item
            member this.Remove item = this.Remove item
            member this.Contains item = this.Contains item
            member this.Clear () = this.Clear ()
            member this.CopyTo (arr, index) = this.CopyTo (arr, index)

    let make<'a> () : 'a SList =
        SList.Make 0 // same as initial capacity for Generic.List.

    let makeWithCapacity<'a> (capacity : int) : 'a SList =
        SList.Make capacity

    let makeFromSegmentedList (list : 'a SList) =
        let lists = List list.Capacity_
        for list in list.Lists_ do
            lists.Add (List list)
        { TotalLength_ = list.TotalLength_
          Capacity_ = list.Capacity_
          Lists_ = lists }

    let isEmpty slist =
        slist.TotalLength_ = 0

    let notEmpty slist =
        slist.TotalLength_ > 0

    let length slist =
        slist.TotalLength_

    let item index (slist : 'a SList) =
        slist.[index]

    let add item (slist : 'a SList) =
        slist.Add item

    let addMany (seq : 'a seq) (slist : 'a SList) =
        slist.AddRange seq

    let clear (slist : 'a SList) =
        slist.Clear ()

    let append left (right : 'a SList) =
        addMany right left

    let skip count (slist : 'a SList) =
        if count > slist.TotalLength_ then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = makeWithCapacity (slist.TotalLength_ - count)
        for i in count .. dec slist.TotalLength_ do
            add slist.[i] result
        result

    let take count (slist : 'a SList) =
        if count > slist.TotalLength_ then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = makeWithCapacity count
        for i in 0 .. dec slist.TotalLength_ - count do
            add slist.[i] result
        result

    let fold folder state slist =
        let mutable state = state
        for item in slist do
            state <- folder state item
        state

    let map mapper (slist : 'a SList) =
        let result = makeWithCapacity slist.TotalLength_
        for item in slist do
            add (mapper item) result
        result

    let map2 mapper left right =
        if left.TotalLength_ <> right.TotalLength_ then raise (ArgumentException ("SList length does not match.", nameof right))
        let result = makeWithCapacity left.TotalLength_
        for i in 0 .. dec left.TotalLength_ do
            add (mapper left.[i] right.[i]) result
        result

    let filter predicate slist =
        let result = make ()
        for item in slist do
            if predicate item then
                add item result
        result

    let iter op slist =
        for item in slist do
            op item

    let exists predicate (slist : 'a SList) =
        let mutable found = false
        let mutable enr = slist.GetEnumerator ()
        while not found && enr.MoveNext () do
            if predicate enr.Current then
                found <- true
        found

    let notExists predicate (slist : 'a SList) =
        not (exists predicate slist)

    let partition discriminator slist =
        let pass = make ()
        let fail = make ()
        for item in slist do
            if discriminator item
            then add item pass
            else add item fail
        (pass, fail)

    let singleton item =
        let result = makeWithCapacity 4 // matches the first allocation capacity of Generic.List.
        add item result
        result

    let ofSeq (seq : 'a seq) =
        let result =
            match seq with // OPTIMIZATION: reserve some predicated capacity.
            | :? ('a array) as arr -> makeWithCapacity<'a> arr.Length
            | :? ('a List) as list -> makeWithCapacity<'a> list.Capacity
            | _ -> make<'a> ()
        for item in seq do
            add item result
        result

    let ofList (list : 'a list) =
        ofSeq list

/// An list that is split into smaller lists in order to avoid allocating from the LOH.
type 'a SList = 'a SList.SList
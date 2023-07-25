// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SList =

    /// An list that is split into smaller list in order to avoid allocating from the LOH.
    type [<ReferenceEquality>] 'a SList =
        private
            { mutable TotalLength_ : int
              mutable Capacity_ : int
              mutable Lists_ : 'a List List }

        static member internal Make () =
            let size = sizeof<'a>
            let listCapacity = Constants.Runtime.LohSize / size / 2 // divide by two since we seem to need some major slop to avoid LOH allocation...
            { TotalLength_ = 0; Capacity_ = listCapacity; Lists_ = List [List<'a> ()] }

        member this.Length =
            this.TotalLength_

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
            let list' = SList.Make ()
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
            Seq.concat this.Lists_

        interface 'a IEnumerable with
            member this.GetEnumerator () = (Seq.concat this.Lists_).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.Lists_).GetEnumerator () :> IEnumerator

    let make<'a> () : 'a SList =
        SList.Make ()

    let makeFromSegmentedList list =
        let lists = List ()
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
        let result = make ()
        for i in count .. dec slist.TotalLength_ do
            add slist.[i] result
        result

    let take count (slist : 'a SList) =
        if count > slist.TotalLength_ then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = make ()
        for i in 0 .. dec slist.TotalLength_ - count do
            add slist.[i] result
        result

    let map mapper slist =
        let result = make ()
        for item in slist do
            add (mapper item) result
        result

    let map2 mapper left right =
        if left.TotalLength_ <> right.TotalLength_ then raise (ArgumentException ("SList length does not match.", nameof right))
        let result = make ()
        for i in 0 .. dec left.TotalLength_ do
            add (mapper left.[i] right.[i]) result
        result

    let foreach op slist =
        for item in slist do
            op item

    let filter predicate slist =
        let result = make ()
        for item in slist do
            if predicate item then
                add item result
        result

    let partition discriminator slist =
        let pass = make ()
        let fail = make ()
        for item in slist do
            if discriminator item
            then add item pass
            else add item fail
        (pass, fail)

    let fold folder state slist =
        let mutable state = state
        for item in slist do
            state <- folder state item
        state

    let singleton item =
        let result = make ()
        add item result
        result

    let ofSeq (seq : 'a seq) =
        let result = make<'a> ()
        for item in seq do
            add item result
        result

    let ofList (list : 'a list) =
        ofSeq list

/// An list that is split into smaller list in order to avoid allocating from the LOH.
type 'a SList = 'a SList.SList
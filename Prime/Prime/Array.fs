// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System

[<RequireQualifiedAccess>]
module Array =

    /// Add a value to the front of an array.
    let cons (elem : 'a) (arr : 'a array) =
        let tailLength = Array.length arr
        let arr2 = Array.zeroCreate (inc tailLength)
        arr2.[0] <- elem
        Array.Copy (arr, 0, arr2, 1, tailLength)
        arr2

    /// Add a value to the end of an array.
    let add (elem : 'a) (arr : 'a array) =
        let tailLength = Array.length arr
        let arr2 = Array.zeroCreate (inc tailLength)
        arr2.[tailLength] <- elem
        Array.Copy (arr, 0, arr2, 0, tailLength)
        arr2

    /// Remove the first matching element of the array (stable).
    let remove pred (arr : 'a array) =
        match Array.tryFindIndex pred arr with
        | Some index ->
            let arr2 = Array.zeroCreate (dec arr.Length) : 'a array
            Array.Copy (arr, 0, arr2, 0, index)
            Array.Copy (arr, inc index, arr2, index, arr2.Length - index)
            arr2
        | None -> arr

    /// Remove an item at the given array index (stable).
    let removeAt index (arr : 'a array) =
        if index < arr.Length && index >= 0 then
            let arr' = Array.zeroCreate (dec arr.Length)
            Array.blit arr 0 arr' 0 index
            Array.blit arr (inc index) arr' index (dec (arr.Length - index))
            arr'
        else raise (IndexOutOfRangeException "Cannot removeAt from an Array when index is out of range.")

    /// Replace the first matching element of the array (stable).
    let replace pred item (arr : 'a array) =
        match Array.tryFindIndex pred arr with
        | Some index ->
            let arr2 = Array.copy arr
            arr2.[index] <- item
            arr2
        | None -> arr

    /// Implement a fold while folder results in Some.
    let foldWhile folder (state : 's) (arr : 't array) =
        Seq.foldWhile folder state arr

    /// Implement a fold while folder results in Right.
    let foldWhileRight folder (state : Either<_, _>) (arr : 't array) =
        Seq.foldWhileRight folder state arr

    /// Implement a fold until folder results in Nome.
    let foldUntil folder (state : 's) (arr : 't array) =
        Seq.foldUntil folder state arr

    /// Implement a fold until folder results in Right.
    let foldUntilRight folder (state : Either<_, _>) (arr : 't array) =
        Seq.foldUntilRight folder state arr

    /// Combines map and fold. Builds a new array whose elements are the results of applying the given function to each
    /// of the elements of the input array. The function is also used to accumulate a final value.
    let foldMap<'T, 'State, 'Result> folder state array =
        Array.mapFold<'T, 'State, 'Result> (flip folder) state array

    /// Check that an array is not empty.
    let inline notEmpty arr =
        not (Array.isEmpty arr)

    /// Check that a predicate passes for NO items in an array.
    let inline notExists pred arr =
        not (Array.exists pred arr)

    /// Foldi for arrays.
    let foldi folder state (array : _ array) =
        Seq.foldi folder state array

    /// Convert option elements to definite elements.
    let inline definitize opts =
        Array.choose id opts

    /// Convert option elements to definite elements, returning an additional flag to indicate that all elements were some.
    let definitizePlus (opts : _ array) =
        let (flag, seq) = Seq.definitizePlus opts
        (flag, Array.ofSeq seq)

    /// A more tolerant and open-minded take.
    let tryTake (count : int) (arr : _ array) =
        Seq.tryTake count arr |> Array.ofSeq

    /// A more tolerant and open-minded skip.
    let trySkip (count : int) (arr : _ array) =
        Seq.trySkip count arr |> Array.ofSeq

    /// Try to find a value.
    let rec tryFindPlus pred (arr : _ seq) =
        Seq.tryFindPlus pred arr

    /// Get all but the last item from a array.
    let allButLast arr =
        tryTake (Array.length arr - 1) arr

    /// Pad an array with count instances of the given element, removing items from back if count is negative.
    let pad count elem arr =
        if count = 0 then arr
        elif count > 0 then Array.append arr (Array.init count (fun _ -> elem))
        else Array.take (Array.length arr + count) arr

    /// Pad an array with count instances of its last item.
    let padWithLast count arr =
        pad count (Array.last arr) arr

    /// Pad an array with instances of its last item so that it is proportion to another array.
    let padWithLastToProportion arr arr2 =
        let deficit = Array.length arr2 - Array.length arr
        padWithLast deficit arr

    /// Perform a sort on elements, preserving order of equal elements.
    let sortStableWith sorter (arr : _ array) =
        arr |> Seq.sortWith sorter |> Array.ofSeq

    /// Perform a sort on elements, preserving order of equal elements.
    let sortStableBy by (arr : _ array) =
        arr |> Seq.sortBy by |> Array.ofSeq

    /// Perform a sort on elements, preserving order of equal elements.
    let sortStable (arr : _ array) =
        arr |> Seq.sort |> Array.ofSeq

    /// Hash an array.
    let hash (arr : _ array) =
        let mutable hashCode = 0
        for i in 0 .. dec arr.Length do
            hashCode <- hashCode ^^^ hash arr.[i]
        hashCode
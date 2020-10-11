﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module HSet =

    /// A hash-value pair, implemented with a struct for efficiency.
    type [<StructuralEquality; NoComparison>] private Hv<'a when 'a : equality> =
        struct
            new (h, v) = { H = h; V = v }
            val H : int
            val V : 'a
            end

    /// Hash set node.
    type [<StructuralEquality; NoComparison; CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>]
        private HNode<'a when 'a : equality> =
        | Nil
        | Singleton of 'a Hv
        | Multiple of 'a HNode array
        | Gutter of Hv<'a> array

    [<RequireQualifiedAccess>]
    module private HNode =
    
        /// OPTIMIZATION: Array.Clone () is not used since it's been profiled to be slower
        let inline private cloneArray (arr : 'a HNode array) =
            let arr' = Array.zeroCreate 16 : 'a HNode array // NOTE: there's an unecessary check against the size here, but that's the only inefficiency
                                                            // TODO: use Array.zeroCreateUnchecked if / when it becomes available
            Array.Copy (arr, 0, arr', 0, 16) // param checks are inefficient, but hopefully there's at least a memcpy underneath...
            arr'
    
        let inline private hashToIndex h dep =
            (h >>> (dep * 4)) &&& 0xF

        let private addToGutter (entry : Hv<'a>) (gutter : Hv<'a> array) =
            let gutterLength = gutter.Length
            let gutter2 = Array.zeroCreate (inc gutterLength) : Hv<'a> array
            Array.Copy (gutter, 0, gutter2, 0, gutterLength)
            gutter2.[gutterLength] <- entry
            gutter2

        let private removeFromGutter (v : 'a) (gutter : Hv<'a> array) =
            match Array.FindLastIndex (gutter, fun (entry2 : Hv<'a>) -> entry2.V.Equals v) with
            | -1 -> gutter
            | index ->
                let gutter2 = Array.zeroCreate (dec gutter.Length) : Hv<'a> array
                Array.Copy (gutter, 0, gutter2, 0, index)
                Array.Copy (gutter, inc index, gutter2, index, gutter2.Length - index)
                gutter2

        let private containedByGutter (v : 'a) (gutter : Hv<'a> array) =
            Array.exists (fun (entry2 : Hv<'a>) -> v.Equals entry2.V) gutter

        let empty =
            Nil
    
        let isEmpty node =
            match node with
            | Nil -> true
            | _ -> false
    
        /// OPTIMIZATION: Requires an empty array to use the source of new array clones in order to avoid Array.create.
        let rec add (hv : 'a Hv) (earr : 'a HNode array) (dep : int) (node : 'a HNode) : 'a HNode =
    
            // lower than max depth, non-clashing
            if dep < 9 then
    
                // handle non-clash cases
                match node with
                | Nil ->
    
                    // make singleton entry
                    Singleton hv
    
                | Singleton hv' ->
                    
                    // if additional entry; convert Singleton to Multiple
                    let idx = hashToIndex hv.H dep
                    let idx' = hashToIndex hv'.H dep
                    if idx <> idx' then
                        let arr = cloneArray earr
                        arr.[idx] <- Singleton hv
                        arr.[idx'] <- Singleton hv'
                        Multiple arr
    
                    // if replace entry; remain Singleton
                    elif hv.V.Equals hv'.V then
                        Singleton hv
    
                    // if add entry with same idx; add both in new node
                    else
                        let dep' = dep + 1
                        let node' = add hv earr dep' Nil
                        let node' = add hv' earr dep' node'
                        let arr = cloneArray earr
                        arr.[idx] <- node'
                        Multiple arr
    
                | Multiple arr ->
    
                    // add entry with recursion
                    let idx = hashToIndex hv.H dep
                    let entry = arr.[idx]
                    let arr = cloneArray arr
                    arr.[idx] <- add hv earr (dep + 1) entry
                    Multiple arr
    
                | Gutter _ ->
    
                    // logically should never hit here
                    failwithumf ()
    
            // clashing
            else
                
                // handle clash cases
                match node with
                | Nil -> Gutter (Array.singleton hv)
                | Singleton hv' -> Gutter [|hv'; hv|]
                | Multiple _ -> failwithumf () // should never hit here
                | Gutter gutter -> Gutter (addToGutter hv gutter)
    
        let rec remove (h : int) (v : 'a) (dep : int) (node : 'a HNode) : 'a HNode =
            match node with
            | Nil -> node
            | Singleton hv -> if hv.V.Equals v then Nil else node
            | Multiple arr ->
                let idx = hashToIndex h dep
                let entry = arr.[idx]
                let arr = cloneArray arr
                arr.[idx] <- remove h v (dep + 1) entry
                if Array.forall isEmpty arr then Nil else Multiple arr // does not collapse Multiple to Singleton, tho could?
            | Gutter gutter ->
                let gutter = removeFromGutter v gutter
                if Array.isEmpty gutter then Nil else Gutter gutter

        let rec contains (h : int) (v : 'a) (dep : int) (node : 'a HNode) =
            match node with
            | Nil -> false
            | Singleton hv -> hv.V.Equals v
            | Multiple arr -> let idx = hashToIndex h dep in contains h v (dep + 1) arr.[idx]
            | Gutter gutter -> containedByGutter v gutter
    
        let rec fold folder state node =
            match node with
            | Nil -> state
            | Singleton hv -> folder state hv.V
            | Multiple arr -> Array.fold (fold folder) state arr
            | Gutter gutter -> Array.fold (fun state (hv : Hv<_>) -> folder state hv.V) state gutter
    
        /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
        let rec toSeq node =
            seq {
                match node with
                | Nil -> yield! Seq.empty
                | Singleton hv -> yield hv.V
                | Multiple arr -> for n in arr do yield! toSeq n
                | Gutter gutter -> yield! Array.map (fun (hv : Hv<_>) -> hv.V) gutter }

    /// A fast persistent hash set.
    /// Works in effectively constant-time for look-ups and updates.
    type [<StructuralEquality; NoComparison>] HSet<'a when 'a : equality> =
        private
            { Node : 'a HNode
              EmptyArray : 'a HNode array }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () = (HNode.toSeq this.Node).GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () = (HNode.toSeq this.Node).GetEnumerator () :> IEnumerator

    /// Create an empty HSet.
    let makeEmpty () =
        { Node = HNode.empty
          EmptyArray = Array.create 16 Nil }

    /// Check that an HSet is empty.
    let isEmpty set =
        HNode.isEmpty set.Node

    /// Check that an HSet is empty.
    let notEmpty set =
        not (HNode.isEmpty set.Node)

    /// Add a value with the key to an HSet.
    let add (value : 'a) set =
        let hv = Hv (value.GetHashCode (), value)
        let node = HNode.add hv set.EmptyArray 0 set.Node
        { set with Node = node }

    /// Add a list of values with associated keys to an HSet.
    let addMany entries set =
        Seq.fold (fun set (value : 'a) -> add value set) set entries

    /// Remove a value with the given key from an HSet.
    let remove (value : 'a) set =
        let h = value.GetHashCode ()
        { set with HSet.Node = HNode.remove h value 0 set.Node }

    /// Remove all values with the given keys from an HSet.
    let removeMany keys set =
        Seq.fold (fun set (value : 'a) -> remove value set) set keys

    /// Check that an HSet contains a value.
    let contains value set =
        let h = value.GetHashCode ()
        HNode.contains h value 0 set.Node
        
    /// Combine the contents of two HSets.
    let concat set set2 =
        Seq.fold (fun set value -> add value set) set set2

    /// Fold over an HSet.
    let fold folder state (set : 'a HSet) =
        HNode.fold folder state set.Node

    /// Map over an HSet.
    let map mapper (set : 'a HSet) =
        fold
            (fun state value -> add (mapper value) state)
            (makeEmpty ())
            set

    /// Filter an HSet.
    let filter pred (set : 'a HSet) =
        fold
            (fun state value -> if pred value then add value state else state)
            (makeEmpty ())
            set

    /// Convert an HSet to a sequence of pairs of keys and values.
    /// NOTE: This function seems to profile as being very slow. I don't know if it's the seq / yields syntax or what.
    /// Don't use it unless you need its laziness or if performance won't be affected significantly.
    let toSeq (set : 'a HSet) =
        set :> 'a IEnumerable

    /// Convert a sequence of keys and values to an HSet.
    let ofSeq pairs =
        Seq.fold
            (fun set value -> add value set)
            (makeEmpty ())
            pairs

/// A very fast persistent hash set.
/// Works in effectively constant-time for look-ups and updates.
type HSet<'a when 'a : equality> = 'a HSet.HSet
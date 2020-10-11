﻿namespace Prime
open System
open System.Collections
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module OMap =

    /// An ordered persistent map based on UMap and FStack.
    /// NOTE: currently not supported by SymbolicConverter!
    type [<NoEquality; NoComparison>] OMap<'k, 'v when 'k : equality> =
        private
            { Indices : UMap<'k, int>
              Entries : struct ('k * 'v) FStack }

        interface IEnumerable<struct ('k * 'v)> with
            member this.GetEnumerator () =
                (this.Entries :> _ seq).GetEnumerator ()

        interface IEnumerable with
            member this.GetEnumerator () =
                (this.Entries :> IEnumerable).GetEnumerator ()

        member this.Item
            with get (key : 'k) =
                let index = this.Indices.[key]
                this.Entries.[index]

    /// Create an empty OMap.
    let makeEmpty<'k, 'v when 'k : equality> config =
        { Indices = UMap.makeEmpty<'k, int> config
          Entries = (FStack.empty : struct ('k * 'v) FStack) }

    /// Check that an OMap is empty.
    let isEmpty map =
        FStack.isEmpty map.Entries

    /// Check that an OMap is empty.
    let notEmpty map =
        FStack.notEmpty map.Entries

    /// Add a value with the key to an OMap.
    let add (key : 'k) (value : 'v) map =
        match UMap.tryFind key map.Indices with
        | Some index ->
            { Indices = UMap.add key index map.Indices
              Entries = FStack.replaceAt index struct (key, value) map.Entries }
        | None ->
            { Indices = UMap.add key (FStack.length map.Entries) map.Indices
              Entries = FStack.conj struct (key, value) map.Entries }

    /// Remove a value with the given key from an OMap.
    let remove (key : 'k) map =
        match UMap.tryFind key map.Indices with
        | Some index -> { Indices = UMap.remove key map.Indices; Entries = FStack.removeAt index map.Entries }
        | None -> map

    /// Remove a value with the given key from an OMap.
    let removeBy (by : 'v -> bool) (map : OMap<'k, 'v>) =
        let map =
            FStack.fold (fun map struct (k, v) ->
                if by v
                then { map with Indices = UMap.remove k map.Indices }
                else map)
                map
                map.Entries
        { map with Entries = FStack.remove (fun struct (_, v) -> by v) map.Entries }

    /// Add all the given entries to an OMap.
    let addMany entries map =
        Seq.fold (fun map (key : 'k, value : 'v) -> add key value map) map entries

    /// Remove all values with the given keys from an OMap.
    let removeMany keys map =
        Seq.fold (fun map (key : 'k) -> remove key map) map keys

    /// Try to find a value with the given key in an OMap.
    /// Constant-time complexity with approx. 1/3 speed of Dictionary.TryGetValue.
    let tryFind (key : 'k) map : 'v option =
        match UMap.tryFind key map.Indices with
        | Some index -> map.Entries.[index] |> snd' |> Some
        | None -> None

    /// Find a value with the given key in an OMap.
    /// Constant-time complexity with approx. 1/3 speed of Dictionary.GetValue.
    let find (key : 'k) map : 'v =
        snd' map.Entries.[map.Indices.[key]]

    /// Try to find a value with the predicate.
    let tryFindBy by map =
        let foundOpt = FStack.tryFind (fun struct (key, value) -> by key value) map.Entries
        Option.map snd' foundOpt

    /// Find a value with the given predicate.
    let findBy by map =
        match tryFindBy by map with
        | Some found -> found
        | None -> raise (KeyNotFoundException "Could not find key in OMap.")

    /// Check that an OMap contains a value with the given key.
    let containsKey key map =
        UMap.containsKey key map.Indices
        
    /// Combine the contents of two OMaps, taking an item from the second map in the case of a key conflict.
    let concat map map2 =
        Seq.fold (flip (uncurry add)) map map2

    /// Fold over an OMap.
    let fold folder state (map : OMap<'k, 'v>) =
        Seq.fold (fun a struct (k, v) -> folder a k v) state map.Entries

    /// Map over an OMap.
    let map mapper map =
        fold
            (fun state key value -> add key (mapper key value) state)
            (makeEmpty (UMap.getConfig map.Indices))
            map

    /// Filter an OMap.
    let filter pred map =
        fold
            (fun state key value -> if pred key value then add key value state else state)
            (makeEmpty (UMap.getConfig map.Indices))
            map

    /// Convert an OMap to a sequence of pairs of keys and values.
    let toSeq (map : OMap<'k, 'v>) =
        map :> IEnumerable<struct ('k * 'v)>

    /// Convert a sequence of keys and values to an OMap.
    let ofSeq pairs config =
        Seq.fold
            (fun map (key, value) -> add key value map)
            (makeEmpty config)
            pairs

    /// Make an OMap with a single entry.
    let makeSingleton<'k, 'v when 'k : equality> key value config =
        let empty = makeEmpty<'k, 'v> config
        add key value empty

/// An ordered persistent map based on UMap and FStack.
/// NOTE: currently not supported by SymbolicConverter!
type OMap<'k, 'v when 'k : equality> = OMap.OMap<'k, 'v>
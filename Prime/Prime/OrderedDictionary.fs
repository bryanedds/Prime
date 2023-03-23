namespace Prime
open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open System.Linq

/// <summary>
/// Debugging view for OrderedDictionary.
/// </summary>
type [<Sealed>] OrderedDictionaryDebugView<'TKey, 'TValue> (dictionary : OrderedDictionary<'TKey, 'TValue>) =

    [<DebuggerBrowsable (DebuggerBrowsableState.RootHidden)>]
    member this.Items : KeyValuePair<'TKey, 'TValue> array = dictionary.ToArray ()

/// <summary>
/// Wraps the keys in an OrderDictionary.
/// </summary>
and [<Sealed>] OrderedDictionaryValueCollection<'TValue> (values : List<'TValue>) =

    interface ICollection<'TValue> with

        member this.CopyTo (array : 'TValue array, arrayIndex : int) = values.CopyTo (array, arrayIndex)
        member this.Count = values.Count

        [<EditorBrowsable (EditorBrowsableState.Never)>]
        member this.Contains item = values.Contains item

        [<EditorBrowsable (EditorBrowsableState.Never)>]
        member this.Add _ = raise (NotSupportedException "")

        [<EditorBrowsable (EditorBrowsableState.Never)>]
        member this.Clear () = raise (NotSupportedException "")

        [<EditorBrowsable (EditorBrowsableState.Never)>]
        member this.IsReadOnly = true

        [<EditorBrowsable (EditorBrowsableState.Never)>]
        member this.Remove _ = raise (NotSupportedException "")

        member this.GetEnumerator () : IEnumerator = values.GetEnumerator ()
        member this.GetEnumerator () : IEnumerator<'TValue> = values.GetEnumerator ()

/// <summary>
/// Wraps the keys in an OrderDictionary.
/// </summary>
and [<Sealed>] OrderedDictionaryKeyCollection<'TKey> (dictionary : Dictionary<'TKey, int>) =

    interface ICollection<'TKey> with

        member this.CopyTo (array : 'TKey array, arrayIndex : int) = dictionary.Keys.CopyTo (array, arrayIndex)
        member this.Count = dictionary.Count

        [<EditorBrowsable (EditorBrowsableState.Never)>]
        member this.Contains item = dictionary.ContainsKey item

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member this.Add _ = raise (NotSupportedException "")

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member this.Clear () = raise (NotSupportedException "")

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member this.IsReadOnly = true

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member this.Remove _ = raise (NotSupportedException "")

        member this.GetEnumerator () : IEnumerator = dictionary.Keys.GetEnumerator ()
        member this.GetEnumerator () : IEnumerator<'TKey> = dictionary.Keys.GetEnumerator ()

/// <summary>
/// Represents a dictionary that tracks the order that items were added.
/// </summary>
/// <typeparam name="TKey">The type of the dictionary keys.</typeparam>
/// <typeparam name="TValue">The type of the dictionary values.</typeparam>
/// <remarks>
/// This dictionary makes it possible to get the index of a key and a key based on an index.
/// It can be costly to find the index of a key because it must be searched for linearly.
/// It can be costly to insert a key/value pair because other key's indexes must be adjusted.
/// It can be costly to remove a key/value pair because other keys' indexes must be adjusted.
/// Ported from - https://github.com/jehugaleahsa/truncon.collections.OrderedDictionary/blob/master/Truncon.Collections/OrderedDictionary.cs
/// </remarks>
and [<Sealed; DebuggerDisplay "Count = {Count}"; DebuggerTypeProxy (typedefof<OrderedDictionaryDebugView<_, _>>)>]
    OrderedDictionary<'TKey, 'TValue> (capacity : int, comparerOpt : IEqualityComparer<'TKey>) =

    let comparer = match comparerOpt with null -> EqualityComparer<'TKey>.Default : IEqualityComparer<'TKey> | _ -> comparerOpt
    let dictionary = Dictionary<'TKey, int> (capacity, comparer)
    let keys = List<'TKey> capacity
    let values = List<'TValue> capacity
    let mutable version = 0

    /// <summary>
    /// Initializes a new instance of an OrderedDictionary.
    /// </summary>
    new () = OrderedDictionary (0, null)

    /// <summary>
    /// Initializes a new instance of an OrderedDictionary.
    /// </summary>
    /// <param name="capacity">The initial capacity of the dictionary.</param>
    /// <exception cref="System.ArgumentOutOfRangeException">The capacity is less than zero.</exception>
    new (capacity : int) = OrderedDictionary (capacity, null)

    /// <summary>
    /// Initializes a new instance of an OrderedDictionary.
    /// </summary>
    /// <param name="comparer">The equality comparer to use to compare keys.</param>
    new (comparer : IEqualityComparer<'TKey>) = OrderedDictionary (0, comparer)

    /// <summary>
    /// Gets the equality comparer used to compare keys.
    /// </summary>
    member this.Comparer = dictionary.Comparer

    /// <summary>
    /// Inserts the given key/value pair at the specified index.
    /// </summary>
    /// <param name="index">The index to insert the key/value pair.</param>
    /// <param name="key">The key to insert.</param>
    /// <param name="value">The value to insert.</param>
    /// <exception cref="System.ArgumentException">The given key already exists in the dictionary.</exception>
    /// <exception cref="System.ArgumentNullException">The key is null.</exception>
    /// <exception cref="System.ArgumentOutOfRangeException">The index is negative -or- larger than the size of the dictionary.</exception>
    member this.Insert (index, key, value) =
        if index < 0 || index > values.Count then
            raise (ArgumentOutOfRangeException (nameof index, index, ""))
        dictionary.Add (key, index)
        for keyIndex in index .. dec keys.Count do
            let otherKey = keys.[keyIndex]
            dictionary.[otherKey] <- inc dictionary.[otherKey]
        keys.Insert (index, key)
        values.Insert (index, value)
        version <- inc version

    /// <summary>
    /// Gets the key at the given index.
    /// </summary>
    /// <param name="index">The index of the key to get.</param>
    /// <returns>The key at the given index.</returns>
    /// <exception cref="System.ArgumentOutOfRangeException">The index is negative -or- larger than the number of keys.</exception>
    member this.GetKey index = keys.[index]

    /// <summary>
    /// Gets the index of the given key.
    /// </summary>
    /// <param name="key">The key to get the index of.</param>
    /// <returns>The index of the key in the dictionary -or- -1 if the key is not found.</returns>
    /// <remarks>The operation runs in O(n).</remarks>
    member this.IndexOf key =
        match dictionary.TryGetValue key with
        | (true, index) -> index
        | (false, _) -> -1

    /// <summary>
    /// Gets the values in the dictionary.
    /// </summary>
    member this.ValueEnumerator : List<'TValue>.Enumerator = values.GetEnumerator ()

    /// <summary>
    /// Gets or sets the value at the given index.
    /// </summary>
    /// <param name="index">The index of the value to get.</param>
    /// <returns>The value at the given index.</returns>
    /// <exception cref="System.ArgumentOutOfRangeException">The index is negative -or- beyond the length of the dictionary.</exception>
    member this.Item
        with get index = values.[index]
        and set index value = values.[index] <- value
            
    interface ICollection<KeyValuePair<'TKey, 'TValue>> with

        member this.Count = dictionary.Count
        member this.IsReadOnly = false

        member this.Clear () =
            dictionary.Clear ()
            keys.Clear ()
            values.Clear ()
            version <- inc version

        member this.Add (item : KeyValuePair<'TKey, 'TValue>) =
            (this :> IDictionary<'TKey, 'TValue>).Add (item.Key, item.Value)

        member this.Contains (item : KeyValuePair<'TKey, 'TValue>) =
            match dictionary.TryGetValue item.Key with
            | (true, index) when obj.Equals (values.[index], item.Value) -> true
            | (_, _) -> false

        member this.CopyTo (array : KeyValuePair<'TKey, 'TValue> array, arrayIndex : int) =
            if isNull null then
                raise (ArgumentNullException (nameof array))
            if arrayIndex < 0 then
                raise (ArgumentOutOfRangeException (nameof arrayIndex, arrayIndex, ""))
            let mutable index = 0
            let mutable arrayIndex = 0
            while index <> keys.Count && arrayIndex < array.Length do
                let key = keys.[index]
                let value = values.[index]
                array.[arrayIndex] <- KeyValuePair<'TKey, 'TValue> (key, value)
                index <- inc index
                arrayIndex <- inc arrayIndex

        member this.Remove (item : KeyValuePair<'TKey, 'TValue>) =
            match dictionary.TryGetValue item.Key with
            | (true, index) -> (this :> IList<KeyValuePair<'TKey, 'TValue>>).RemoveAt index; true
            | (false, _) -> false

        member this.GetEnumerator () : IEnumerator<KeyValuePair<'TKey, 'TValue>> =
            let startVersion = version
            let kvps =
                seq {
                    for index in 0 .. dec keys.Count do
                        let key = keys.[index]
                        let value = values.[index]
                        yield KeyValuePair (key, value)
                        if version <> startVersion then raise (InvalidOperationException "") }
            kvps.GetEnumerator ()

        member this.GetEnumerator () : IEnumerator =
            let startVersion = version
            let kvps =
                seq {
                    for index in 0 .. dec keys.Count do
                        let key = keys.[index]
                        let value = values.[index]
                        yield KeyValuePair (key, value)
                        if version <> startVersion then raise (InvalidOperationException "") }
            kvps.GetEnumerator ()

    interface IList<KeyValuePair<'TKey, 'TValue>> with

        member this.Item
            with get index =
                let key = keys.[index]
                let value = values.[index]
                KeyValuePair<'TKey, 'TValue> (key, value)
            and set index value =
                let key = keys.[index]
                if dictionary.Comparer.Equals (key, value.Key)
                then dictionary.[value.Key] <- index
                else
                    dictionary.Add (value.Key, index) // will throw if key already exists
                    dictionary.Remove key |> ignore<bool>
                keys.[index] <- value.Key
                values.[index] <- value.Value

        member this.RemoveAt index =
            let key = keys.[index]
            for keyIndex in inc index .. dec keys.Count do
                let otherKey = keys.[keyIndex]
                dictionary.[otherKey] <- dec dictionary.[otherKey]
            dictionary.Remove key |> ignore<bool>
            keys.RemoveAt index
            values.RemoveAt index
            version <- inc version

        member this.IndexOf (item : KeyValuePair<'TKey, 'TValue>) =
            match dictionary.TryGetValue item.Key with
            | (true, index) when obj.Equals (values.[index], item.Value) -> index
            | (_, _) -> -1

        member this.Insert (index, item : KeyValuePair<'TKey, 'TValue>) =
            this.Insert (index, item.Key, item.Value)

    interface IDictionary<'TKey, 'TValue> with

        member this.Keys = OrderedDictionaryKeyCollection dictionary
        member this.Values = OrderedDictionaryValueCollection values

        member this.Item
            with get key = values.[dictionary.[key]]
            and set key value =
                match dictionary.TryGetValue key with
                | (true, index) -> values.[index] <- value
                | (false, _) -> (this :> IDictionary<'TKey, 'TValue>).Add (key, value)

        member this.ContainsKey key = dictionary.ContainsKey key

        member this.TryGetValue (key, valueRef : 'TValue byref) =
            match dictionary.TryGetValue key with
            | (true, index) -> valueRef <- values.[index]; true
            | (false, _) -> valueRef <- Unchecked.defaultof<'TValue>; false

        member this.Add (key, value) =
            dictionary.Add (key, values.Count)
            keys.Add key
            values.Add value
            version <- inc version

        member this.Remove key =
            match dictionary.TryGetValue key with
            | (true, index) -> (this :> IList<KeyValuePair<'TKey, 'TValue>>).RemoveAt index; true
            | (false, _) -> false
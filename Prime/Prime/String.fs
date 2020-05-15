// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open System.Collections.Generic
open System.Text

[<RequireQualifiedAccess>]
module String =

    /// Check that a string is empty.
    let inline isEmpty str =
        String.length str = 0

    /// Check that a string is not empty.
    let inline notEmpty str =
        String.length str > 0

    /// Take a substring from a string.
    let take n (str : string) =
        str |> Seq.take n |> Seq.toArray |> String

    /// Take a substring from a string.
    let tryTake n (str : string) =
        str |> Seq.tryTake n |> Seq.toArray |> String

    /// Skip a potion of a string.
    let skip n (str : string) =
        str |> Seq.skip n |> Seq.toArray |> String

    /// Skip a potion of a string.
    let trySkip n (str : string) =
        str |> Seq.trySkip n |> Seq.toArray |> String

    let inline join (insert : string) (strs : string seq) =
        String.Join (insert, strs)

    /// Check that a string is a guid.
    let isGuid str =
        fst (Guid.TryParse str)

    /// Convert a bool to a string that works well in code.
    let boolToCodeString (bool : bool) =
        if bool then "true" else "false"

    /// Convert an int64 to a string that works well in code.
    let int64ToCodeString (num : int64) =
        let numStr = string num
        numStr + "L"

    /// Convert a single to a string that works well in code.
    let singleToCodeString (num : single) =
        if not (Single.IsNaN num) then
            let decimaled = num.ToString ("N7")
            let cleaned = decimaled.TrimEnd('0').Replace(",","")
            let zeroed = if cleaned.EndsWith "." then cleaned + "0" else cleaned
            zeroed + "f"
        else string num

    /// Convert a double to a string that works well in code.
    let doubleToCodeString (num : double) =
        if not (Double.IsNaN num) then
            let decimaled = num.ToString ("N15")
            let cleaned = decimaled.TrimEnd('0').Replace(",","")
            if cleaned.EndsWith "." then cleaned + "0" else cleaned
        else string num

    /// Convert a number to a string that works well in code.
    let numberToCodeString (num : obj) =
        match num with
        | :? bool as bool -> boolToCodeString bool
        | :? char as char -> string char
        | :? int as int -> string int
        | :? int64 as int64 -> int64ToCodeString int64
        | :? single as single -> singleToCodeString single
        | :? double as double -> doubleToCodeString double
        | _ -> failwithumf ()

    /// Converts a string into a list of characters.
    let explode (str : string) =
        let rec loop n acc =
            if n = 0 then acc
            else
                let n = n - 1
                loop n (str.[n] :: acc)
        loop (String.length str) []
    
    /// Converts a list of characters into a string.
    let implode chars =
        let sb = StringBuilder ()
        List.iter (fun (chr : char) -> sb.Append chr |> ignore) chars
        sb.ToString ()

    /// Capitalize a string.
    let capitalize (str : string) =
        match str.ToCharArray () |> List.ofArray with
        | [] -> str
        | [head] -> [|Char.ToUpperInvariant head|] |> String
        | head :: tail -> Char.ToUpperInvariant head :: tail |> Array.ofList |> String

    /// Textualize a string for usage as text.
    let textualize (str : string) =
        str.Replace ('_', '\"')

    /// Get the string with the given ending.
    let withEnd str target =
        let length = String.length str
        let endLength = String.length target
        if endLength >= length then (false, String.Empty)
        else
            let beginLength = length - endLength
            let beginStr = str.Substring (0, beginLength)
            let endStr = str.Substring (beginLength, endLength)
            (endStr = target, beginStr)

    /// Convert a string to an array of characters.
    let inline toArray str =
        Array.ofList (explode str)

    /// Surround a string with another surrounding string.
    let inline surround (sur : string) (str : string) =
        sur + str + sur

    /// Contract escaped characters in a string.
    let unescape (str : string) =
        let unescaped =
            Seq.fold (fun (escaped, chars) y ->
                if escaped then
                    let chr = 
                        match y with
                        | '0' -> '\u0000'
                        | '\\' -> '\\'
                        | 'a' -> '\a'
                        | 'b' -> '\b'
                        | 'f' -> '\u000c'
                        | 'n' -> '\n'
                        | 'r' -> '\r'
                        | 't' -> '\t'
                        | 'v' -> '\v'
                        | '\"' -> '_'
                        | c -> c
                    (false, chr :: chars)
                elif y = '\\' then (true, chars)
                else (false, y :: chars))
                (false, [])
                str 
        unescaped |> snd |> List.rev |> implode

    /// Expand escaped characters in a string.
    let escape (str : string) =
        // NOTE: doing escape character substitution in-place with a linked-list may prevent speed issues
        str
            .Replace("\\", "\\\\") // NOTE: this line must come first
            .Replace("\u0000", "\\0")
            .Replace("\a", "\\a")
            .Replace("\b", "\\b")
            .Replace("\f", "\\f")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t")
            .Replace("\v", "\\v")

    /// Check for equality an array of strings lexicographically.
    let equateMany (strs : string array) (strs2 : string array) =
        if strs.Length = strs2.Length then
            let enr = (strs :> IEnumerable<string>).GetEnumerator ()
            let enr2 = (strs2 :> IEnumerable<string>).GetEnumerator ()
            let mutable result = true
            while result && enr.MoveNext () do
                enr2.MoveNext () |> ignore
                result <- strEq enr.Current enr2.Current
            result
         else false

    /// Check for equality an array of strings lexicographically.
    let equateManyOpts (strs : string option array) (strs2 : string option array) =
        if strs.Length = strs2.Length then
            let enr = (strs :> IEnumerable<string option>).GetEnumerator ()
            let enr2 = (strs2 :> IEnumerable<string option>).GetEnumerator ()
            let mutable result = true
            while result && enr.MoveNext () do
                enr2.MoveNext () |> ignore
                result <- enr.Current = enr2.Current // generic eq is slower here, but it probably won't matter
            result
         else false
    
    /// Compare an array of strings lexicographically.
    let compareMany (strs : string array) (strs2 : string array) =
        let length = strs.Length // avoid copy warning
        let lengthCmp = length.CompareTo strs2.Length
        if lengthCmp = 0 then
            let enr = (strs :> IEnumerable<string>).GetEnumerator ()
            let enr2 = (strs2 :> IEnumerable<string>).GetEnumerator ()
            let mutable result = 0
            while result = 0 && enr.MoveNext () do
                enr2.MoveNext () |> ignore
                result <- strCmp enr.Current enr2.Current
            result
         else lengthCmp
    
    /// Hash an array of names.
    let hashMany (strs : string array) =
        let mutable hashValue = 0 // OPTIMIZATION: mutation for speed
        for name in strs do hashValue <- hashValue ^^^ hash name
        hashValue
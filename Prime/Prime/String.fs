// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open System.Text
open System.Text.RegularExpressions

[<AutoOpen>]
module StringExtensions =

    let private regex = Regex "([A-Z][a-z]+)"

    type String with

        /// Separate a string into words according to capitalization (but not punctuation).
        member this.Spaced =
            let mutable first = true
            let builder = StringBuilder ()
            for match_ in regex.Matches this do
                if not first then builder.Append " " |> ignore<StringBuilder>
                builder.Append match_.Value |> ignore<StringBuilder>
                first <- false
            builder.ToString ()

[<RequireQualifiedAccess>]
module String =

    /// The empty string.
    let [<Literal>] empty = ""

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
    let isGuid (str : string) =
        fst (Guid.TryParse str)

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

    /// Uncapitalize a string.
    let uncapitalize (str : string) =
        match str.ToCharArray () |> List.ofArray with
        | [] -> str
        | [head] -> [|Char.ToLowerInvariant head|] |> String
        | head :: tail -> Char.ToLowerInvariant head :: tail |> Array.ofList |> String

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
        let length = strs.Length
        if length = strs2.Length then
            let mutable result = true
            let mutable i = 0
            while result && i < length do
                result <- strEq strs.[i] strs2.[i]
                i <- inc i
            result
         else false

    /// Check for equality an array of strings lexicographically.
    let equateManyOpts (strs : string option array) (strs2 : string option array) =
        let length = strs.Length
        if length = strs2.Length then
            let mutable result = true
            let mutable i = 0
            while result && i < length do
                result <- objEq strs.[i] strs2.[i]
                i <- inc i
            result
         else false

    /// Compare an array of strings lexicographically.
    let compareMany (strs : string array) (strs2 : string array) =
        let mutable result = 0
        let mutable i = 0
        while result = 0 && i < strs.Length && i < strs2.Length do
            result <- strCmp strs.[i] strs2.[i]
            i <- inc i
        if result = 0 then
            if strs.Length < strs2.Length then result <- -1
            elif strs.Length > strs2.Length then result <- 1
            else result <- 0
        result

    /// Hash an array of names.
    let hashMany (strs : string array) =
        let mutable hashValue = 0 // OPTIMIZATION: mutation for speed
        for name in strs do hashValue <- hashValue ^^^ hash name
        hashValue
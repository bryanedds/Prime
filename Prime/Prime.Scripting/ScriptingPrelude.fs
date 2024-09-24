// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System

[<RequireQualifiedAccess>]
module String =

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


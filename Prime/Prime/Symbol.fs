// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open FParsec
open Csv
open Prime

type [<StructuralEquality; StructuralComparison>] SymbolSource =
    { FilePathOpt : string option
      Text : string }

type [<StructuralEquality; StructuralComparison>] SymbolState =
    { SymbolSource : SymbolSource }

type [<StructuralEquality; StructuralComparison>] SymbolOrigin =
    { Source : SymbolSource
      Start : Position
      Stop : Position }

    static member printStart origin =
        "[Ln: " + string origin.Start.Line + ", Col: " + string origin.Start.Column + "]"

    static member printStop origin =
        "[Ln: " + string origin.Stop.Line + ", Col: " + string origin.Stop.Column + "]"

    static member printContext origin =
        try // there's more than one thing that can go wrong in here...
            let sourceLines = origin.Source.Text.Split '\n'
            let problemLineIndex = int origin.Start.Line - 1
            let problemLinesStartCount = problemLineIndex - Math.Max (0, problemLineIndex - 3)
            let problemLinesStart =
                sourceLines |>
                Array.trySkip (problemLineIndex - problemLinesStartCount) |>
                Array.take (inc problemLinesStartCount) |>
                String.concat "\n" |>
                fun str -> if String.isEmpty str then "" else str + "\n"
            let problemLinesStop =
                sourceLines |>
                Array.skip (inc problemLineIndex) |>
                Array.tryTake 4 |>
                String.concat "\n" |>
                fun str -> if String.isEmpty str then "" else "\n" + str
            let problemUnderline =
                String.replicate (int origin.Start.Column - 1) " " +
                if origin.Start.Line = origin.Stop.Line
                then String.replicate (int origin.Stop.Column - int origin.Start.Column) "^"
                else "^^^^^^^" // just use lots of carets...
            problemLinesStart + problemUnderline + problemLinesStop
        with exn ->
            // ...and I don't feel like dealing with all the specifics.
            "Error creating violation context."

    static member print origin =
        "At location: " + SymbolOrigin.printStart origin + " thru " + SymbolOrigin.printStop origin + "\n" +
        "In context:\n" +
        "\n" +
        SymbolOrigin.printContext origin

    static member tryPrint originOpt =
        match originOpt with
        | Some origin -> SymbolOrigin.print origin
        | None -> "Error origin unknown or not applicable."

/// A lisp-style symbolic type.
type [<StructuralEquality; StructuralComparison>] Symbol =
    | Atom of string * SymbolOrigin option
    | Number of string * SymbolOrigin option
    | Text of string * SymbolOrigin option
    | Quote of Symbol * SymbolOrigin option
    | Symbols of Symbol list * SymbolOrigin option

    /// Try to get the origin of the symbol if it has one.
    static member getOriginOpt symbol =
        match symbol with
        | Atom (_, originOpt)
        | Number (_, originOpt)
        | Text (_, originOpt)
        | Quote (_, originOpt)
        | Symbols (_, originOpt) -> originOpt
        
    /// Set the origin of a symbol.
    static member setOriginOpt originOpt symbol =
        match symbol with
        | Atom (str, _) -> Atom (str, originOpt)
        | Number (str, _) -> Number (str, originOpt)
        | Text (str, _) -> Text (str, originOpt)
        | Quote (sym, _) -> Quote (sym, originOpt)
        | Symbols (syms, _) -> Symbols (syms, originOpt)

[<RequireQualifiedAccess>]
module Symbol =

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] WhitespaceChars = "\t " + NewlineChars
    let [<Literal>] IndexChar = '.'
    let [<Literal>] IndexStr = "."
    let [<Literal>] OpenSymbolsChar = '['
    let [<Literal>] OpenSymbolsStr = "["
    let [<Literal>] CloseSymbolsChar = ']'
    let [<Literal>] CloseSymbolsStr = "]"
    let [<Literal>] OpenStringChar = '\"'
    let [<Literal>] OpenStringStr = "\""
    let [<Literal>] CloseStringChar = '\"'
    let [<Literal>] CloseStringStr = "\""
    let [<Literal>] QuoteChar = '`'
    let [<Literal>] QuoteStr = "`"
    let [<Literal>] LineCommentChar = ';'
    let [<Literal>] LineCommentStr = ";"
    let [<Literal>] OpenMultilineCommentStr = "#|"
    let [<Literal>] CloseMultilineCommentStr = "|#"
    let [<Literal>] IndexExpansion = "Index"
    let [<Literal>] ReservedChars = "(){}\\#$:,"
    let [<Literal>] StructureCharsNoStrNoIndex = "[]`"
    let [<Literal>] StructureCharsNoStr = StructureCharsNoStrNoIndex + IndexStr
    let [<Literal>] StructureCharsNoIndex = "\"" + StructureCharsNoStrNoIndex
    let [<Literal>] StructureChars = "\"" + StructureCharsNoStr
    let (*Literal*) IllegalNameChars = ReservedChars + StructureChars + WhitespaceChars
    let (*Literal*) IllegalNameCharsArray = Array.ofSeq IllegalNameChars
    let [<Literal>] NumberFormat =
        NumberLiteralOptions.AllowMinusSign |||
        NumberLiteralOptions.AllowPlusSign |||
        NumberLiteralOptions.AllowExponent |||
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowHexadecimal |||
        NumberLiteralOptions.AllowSuffix

    let isWhitespaceChar chr = isAnyOf WhitespaceChars chr
    let isStructureChar chr = isAnyOf StructureChars chr
    let isExplicit (str : string) = str.StartsWith OpenStringStr && str.EndsWith CloseStringStr

    let distill (str : string) =
        if str.StartsWith OpenStringStr && str.EndsWith CloseStringStr
        then str.Substring (1, str.Length - 2)
        else str

    let skipLineComment = skipChar LineCommentChar >>. skipRestOfLine true
    let skipMultilineComment =
        // TODO: make multiline comments nest.
        between
            (skipString OpenMultilineCommentStr)
            (skipString CloseMultilineCommentStr)
            (skipCharsTillString CloseMultilineCommentStr false System.Int32.MaxValue)
    
    let skipWhitespace = skipLineComment <|> skipMultilineComment <|> skipAnyOf WhitespaceChars
    let skipWhitespaces = skipMany skipWhitespace
    let followedByWhitespaceOrStructureCharOrAtEof = nextCharSatisfies (fun chr -> isWhitespaceChar chr || isStructureChar chr) <|> eof
    
    let startIndex = skipChar IndexChar
    let startQuote = skipChar QuoteChar
    let openSymbols = skipChar OpenSymbolsChar
    let closeSymbols = skipChar CloseSymbolsChar
    let openString = skipChar OpenStringChar
    let closeString = skipChar CloseStringChar
    
    let isNumberParser = numberLiteral NumberFormat "number" >>. eof
    let isNumber str = match run isNumberParser str with Success (_, _, position) -> position.Index = int64 str.Length | Failure _ -> false
    let shouldBeExplicit str = Seq.exists (fun chr -> Char.IsWhiteSpace chr || Seq.contains chr StructureCharsNoStr) str

    let readAtomChars = many1 (noneOf (StructureChars + WhitespaceChars))
    let readStringChars = many (noneOf [CloseStringChar])
    let (readSymbol : Parser<Symbol, SymbolState>, private readSymbolRef : Parser<Symbol, SymbolState> ref) = createParserForwardedToRef ()

    let readAtom =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! chars = readAtomChars
            let! stop = getPosition
            do! skipWhitespaces
            let str = chars |> String.implode |> fun str -> str.TrimEnd ()
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Atom (str, originOpt) }

    let readNumber =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! number = numberLiteral NumberFormat "number"
            do! followedByWhitespaceOrStructureCharOrAtEof
            let! stop = getPosition
            do! skipWhitespaces
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            let suffix =
                (if number.SuffixChar1 <> (char)65535 then string number.SuffixChar1 else "") + 
                (if number.SuffixChar2 <> (char)65535 then string number.SuffixChar2 else "") + 
                (if number.SuffixChar3 <> (char)65535 then string number.SuffixChar3 else "") + 
                (if number.SuffixChar4 <> (char)65535 then string number.SuffixChar4 else "")
            return Number (number.String + suffix, originOpt) }

    let readString =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! openString
            let! escaped = readStringChars
            do! closeString
            let! stop = getPosition
            do! skipWhitespaces
            let str = String.implode escaped
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Text (str, originOpt) }

    let readQuote =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! startQuote
            let! quoted = readSymbol
            let! stop = getPosition
            do! skipWhitespaces
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Quote (quoted, originOpt) }

    let readSymbols =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! openSymbols
            do! skipWhitespaces
            let! symbols = many readSymbol
            do! closeSymbols
            let! stop = getPosition
            do! skipWhitespaces
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Symbols (symbols, originOpt) }

    let readIndex =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! startIndex
            do! skipWhitespaces
            let! stop = getPosition
            do! skipWhitespaces
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return fun target indexer ->
                match indexer with
                | Symbols ([Number _ as number], _) -> Symbols ([Atom (IndexExpansion, originOpt); number; target], originOpt)
                | _ -> Symbols ([Atom (IndexExpansion, originOpt); indexer; target], originOpt) }

    let readSymbolBirecursive =
        attempt readQuote <|>
        attempt readString <|>
        attempt readNumber <|>
        attempt readAtom <|>
        readSymbols

    do readSymbolRef :=
        chainl1 readSymbolBirecursive readIndex

    let rec writeSymbol symbol =
        match symbol with
        | Atom (str, _) ->
            let str = distill str
            if Seq.isEmpty str then OpenStringStr + CloseStringStr
            elif not (isExplicit str) && shouldBeExplicit str then OpenStringStr + str + CloseStringStr
            elif isExplicit str && not (shouldBeExplicit str) then str.Substring (1, str.Length - 2)
            else str
        | Number (str, _) -> distill str
        | Text (str, _) -> OpenStringStr + distill str + CloseStringStr
        | Quote (symbol, _) -> QuoteStr + writeSymbol symbol
        | Symbols (symbols, _) ->
            match symbols with
            | [Atom (str, _); Number _ as indexer; target] when str = IndexExpansion ->
                writeSymbol target + IndexStr + OpenSymbolsStr + writeSymbol indexer + CloseSymbolsStr
            | [Atom (str, _); indexer; target] when str = IndexExpansion ->
                writeSymbol target + IndexStr + writeSymbol indexer
            | _ ->
                OpenSymbolsStr + String.concat " " (List.map writeSymbol symbols) + CloseSymbolsStr

    /// Convert a string to a symbol, with the following parses:
    /// 
    /// (* Atom values *)
    /// None
    /// CharacterAnimationFacing
    /// 
    /// (* Number values *)
    /// 0.0f
    /// -5
    ///
    /// (* String value *)
    /// "String with quoted spaces."
    ///
    /// (* Quoted value *)
    /// `[Some 1]
    /// 
    /// (* Symbols values *)
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]]
    ///
    /// ...and so on.
    let fromString str filePathOpt =
        let symbolState = { SymbolSource = { FilePathOpt = filePathOpt; Text = str }}
        match runParserOnString (skipWhitespaces >>. readSymbol) symbolState String.Empty str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error

    /// Read a symbol from a CSV (comma-separated value) string.
    let fromStringCsv stripHeader csvStr (filePathOpt : string option) =
        let csvOptions = CsvOptions ()
        csvOptions.HeaderMode <- if stripHeader then HeaderMode.HeaderPresent else HeaderMode.HeaderAbsent
        let symbols =
            CsvReader.ReadFromText (csvStr, csvOptions) |>
            Seq.map (fun line -> Seq.toList line.Values) |>
            Seq.toList |>
            List.map (fun values -> Symbols (List.map (fun str -> if str = "" then Text ("", None) else fromString str None) values, None))
        let symbolSource = { FilePathOpt = filePathOpt; Text = csvStr }
        let symbolOrigin = { Source = symbolSource; Start = Position ("", 0L, 0L, 0L); Stop = Position ("", 0L, 0L, 0L) }
        let symbol = Symbols (symbols, Some symbolOrigin)
        symbol

    /// Convert a symbol to a string, with the following unparses:
    /// 
    /// (* Atom values *)
    /// None
    /// CharacterAnimationFacing
    /// 
    /// (* Number values *)
    /// 0.0f
    /// -5
    ///
    /// (* String value *)
    /// "String with quoted spaces."
    ///
    /// (* Quoted value *)
    /// `[Some 1]
    /// 
    /// (* Symbols values *)
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]]
    ///
    /// ...and so on.
    let rec toString symbol = writeSymbol symbol

type ConversionException (message : string, symbolOpt : Symbol option) =
    inherit Exception (message)
    member this.SymbolOpt = symbolOpt
    override this.ToString () =
        message + "\n" +
        (match symbolOpt with Some symbol -> SymbolOrigin.tryPrint (Symbol.getOriginOpt symbol) + "\n" | _ -> "") +
        base.ToString ()

[<AutoOpen>]
module ConversionExceptionOperators =
    let failconv message symbolOpt =
        raise (ConversionException (message, symbolOpt))
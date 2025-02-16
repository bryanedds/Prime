// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System
open System.Reflection
open System.Text

/// Pretty prints Symbols, as well as strings by converting them to Symbols.
type PrettyPrinter =
    { TitleWords : string Set
      HeaderWords : string Set
      DetailWords : string Set
      ThresholdMin : int
      ThresholdMax : int }

    static member defaultPrinter =
        { TitleWords = Set.empty
          HeaderWords = Set.empty
          DetailWords = Set.empty
          ThresholdMin = Constants.PrettyPrinter.DefaultThresholdMin
          ThresholdMax = Constants.PrettyPrinter.DefaultThresholdMax }

[<RequireQualifiedAccess>]
module PrettyPrinter =

    type private PrettySymbol =
        | PrettyAtom of bool * bool * bool * string * Symbol
        | PrettyNumber of string * Symbol
        | PrettyText of string * Symbol
        | PrettyIndex of int * PrettySymbol * PrettySymbol
        | PrettyQuote of int * PrettySymbol
        | PrettySymbols of bool * bool * int * PrettySymbol list

    let rec private getTitled prettySymbol =
        match prettySymbol with
        | PrettyAtom (titled, _, _, _, _) -> titled
        | _ -> false

    let rec private getHeadered prettySymbol =
        match prettySymbol with
        | PrettyAtom (_, headered, _, _, _) -> headered
        | _ -> false

    let rec private getDetailed prettySymbol =
        match prettySymbol with
        | PrettyAtom (_, _, detailed, _, _) -> detailed
        | _ -> false

    let rec private getMaxDepth prettySymbol =
        match prettySymbol with
        | PrettyAtom _ -> 0
        | PrettyNumber _ -> 0
        | PrettyText _ -> 0
        | PrettyIndex _ -> 0
        | PrettyQuote (maxDepth, _) -> maxDepth
        | PrettySymbols (_, _, maxDepth, _) -> maxDepth

    let rec private symbolToPrettySymbol symbol prettyPrinter =
        match symbol with
        | Atom (str, _) ->
            PrettyAtom
                (Set.contains str prettyPrinter.TitleWords,
                 Set.contains str prettyPrinter.HeaderWords,
                 Set.contains str prettyPrinter.DetailWords,
                 str,
                 symbol)
        | Number (str, _) -> PrettyNumber (str, symbol)
        | Text (str, _) -> PrettyText (str, symbol)
        | Quote (quoted, _) ->
            let prettyQuoted = symbolToPrettySymbol quoted prettyPrinter
            let maxDepth = getMaxDepth prettyQuoted
            PrettyQuote (inc maxDepth, prettyQuoted)
        | Symbols (symbols, _) ->
            match symbols with
            | [Atom ("Index", _); indexer; target] ->
                let prettyIndexer = symbolToPrettySymbol indexer prettyPrinter
                let prettyTarget = symbolToPrettySymbol target prettyPrinter
                PrettyIndex (0, prettyIndexer, prettyTarget)
            | _ ->
                let prettySymbols = List.map (flip symbolToPrettySymbol prettyPrinter) symbols
                let titled = match prettySymbols with head :: _ -> getTitled head | [] -> false
                let headered = match prettySymbols with head :: _ -> getHeadered head | [] -> false
                let detailed = match prettySymbols with head :: _ -> getDetailed head | [] -> false
                let empty = List.isEmpty symbols
                let maxDepths = 0 :: List.map getMaxDepth prettySymbols
                let maxDepth = List.max maxDepths
                let maxDepth = if headered || detailed || empty then maxDepth else maxDepth + 1
                PrettySymbols (titled, headered, maxDepth, prettySymbols)

    let rec private prettySymbolsToPrettyStr titled headered depth unfolding symbols prettyPrinter (stringBuilder : StringBuilder) =
        if unfolding then
            let symbolsLength = List.length symbols
            stringBuilder.Append Symbol.OpenSymbolsStr |> ignore
            List.iteri (fun i prettySymbol ->
                let whitespace =
                    if titled then
                        if i > 1 then "\n" + String.init (inc depth) (fun _ -> " ")
                        elif i > 0 then " "
                        else ""
                    elif headered then
                        if i = dec symbolsLength then "\n" + String.init (inc depth) (fun _ -> " ")
                        elif i > 0 then " "
                        else ""
                    else
                        if i > 0 then "\n" + String.init (inc depth) (fun _ -> " ")
                        else ""
                stringBuilder.Append whitespace |> ignore
                prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter stringBuilder)
                symbols
            stringBuilder.Append Symbol.CloseSymbolsStr |> ignore
        else
            let symbolsLength = List.length symbols
            stringBuilder.Append Symbol.OpenSymbolsStr |> ignore
            List.iteri (fun i prettySymbol ->
                prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter stringBuilder
                if i < dec symbolsLength then stringBuilder.Append " " |> ignore)
                symbols
            stringBuilder.Append Symbol.CloseSymbolsStr |> ignore

    and private prettySymbolToPrettyStr depth prettySymbol prettyPrinter stringBuilder =
        match prettySymbol with
        | PrettyAtom (_, _, _, _, symbol)
        | PrettyNumber (_, symbol)
        | PrettyText (_, symbol) -> Symbol.buildSymbol symbol stringBuilder
        | PrettyIndex (depth, prettyIndexer, prettyTarget) ->
            match prettyIndexer with
            | PrettyNumber _ ->
                prettySymbolToPrettyStr depth prettyTarget prettyPrinter stringBuilder
                stringBuilder.Append Symbol.IndexStr |> ignore
                stringBuilder.Append Symbol.OpenSymbolsStr |> ignore
                prettySymbolToPrettyStr depth prettyIndexer prettyPrinter stringBuilder
                stringBuilder.Append Symbol.CloseSymbolsStr |> ignore
            | _ ->
                prettySymbolToPrettyStr depth prettyTarget prettyPrinter stringBuilder
                stringBuilder.Append Symbol.IndexStr |> ignore
                prettySymbolToPrettyStr depth prettyIndexer prettyPrinter stringBuilder
        | PrettyQuote (_, prettySymbol) ->
            stringBuilder.Append Symbol.QuoteStr |> ignore
            prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter stringBuilder
        | PrettySymbols (titled, headered, maxDepth, symbols) ->
            let unfolding = depth < prettyPrinter.ThresholdMin || maxDepth > prettyPrinter.ThresholdMax
            prettySymbolsToPrettyStr titled headered depth unfolding symbols prettyPrinter stringBuilder


    /// Pretty print a symbol using the given pretty printer configuation.
    let prettyPrintSymbol symbol prettyPrinter =
        let prettySymbol = symbolToPrettySymbol symbol prettyPrinter
        let stringBuilder = StringBuilder ()
        prettySymbolToPrettyStr 0 prettySymbol prettyPrinter stringBuilder
        stringBuilder.ToString ()

    /// Pretty print a value using the given pretty printer configuation.
    let prettyPrintValue<'a> (value : 'a) prettyPrinter =
        prettyPrintSymbol (valueToSymbol<'a> value) prettyPrinter

    /// Pretty print a symbolic string using the given pretty printer configuation.
    let prettyPrint str prettyPrinter =
        let symbol = Symbol.ofString str None
        prettyPrintSymbol symbol prettyPrinter

/// Configures the pretty printing behavior of a type.
type [<AttributeUsage (AttributeTargets.Enum ||| AttributeTargets.Struct ||| AttributeTargets.Class); AllowNullLiteral>]
    SyntaxAttribute
        (keywords0 : string,
         keywords1 : string,
         titleWordsStr : string,
         headerWordsStr : string,
         detailWordsStr : string,
         prettyPrinterThresholdMin : int,
         prettyPrinterThresholdMax : int) =
    inherit Attribute ()
    member this.Keywords0 = keywords0
    member this.Keywords1 = keywords1
    member this.PrettyPrinter =
        { TitleWords = Set.ofArray (titleWordsStr.Split ' ')
          HeaderWords = Set.ofArray (headerWordsStr.Split ' ')
          DetailWords = Set.ofArray (detailWordsStr.Split ' ')
          ThresholdMin = prettyPrinterThresholdMin
          ThresholdMax = prettyPrinterThresholdMax }
    static member defaultValue (ty : Type) =
        match ty.GetCustomAttribute<SyntaxAttribute> true with
        | null ->
            SyntaxAttribute
                ("", "", "", "", "",
                 PrettyPrinter.defaultPrinter.ThresholdMin,
                 PrettyPrinter.defaultPrinter.ThresholdMax)
        | syntax -> syntax
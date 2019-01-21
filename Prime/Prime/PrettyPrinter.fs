// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open System.Reflection
open Prime

/// Pretty prints Symbols, as well as strings by converting them to Symbols.
type PrettyPrinter =
    { TitleWords : string Set
      HeaderWords : string Set
      DetailWords : string Set
      ThresholdMin : int
      ThresholdMax : int }

    static member defaulted =
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
        | PrettyString of string * Symbol
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
        | PrettyString _ -> 0
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
        | String (str, _) -> PrettyString (str, symbol)
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
                let maxDepths = 0 :: List.map getMaxDepth prettySymbols
                let maxDepth = List.max maxDepths
                let maxDepth = if headered || detailed then maxDepth else maxDepth + 1
                PrettySymbols (titled, headered, maxDepth, prettySymbols)

    let rec private prettySymbolsToPrettyStr titled headered depth unfolding symbols prettyPrinter =
        if unfolding then
            let symbolsLength = List.length symbols
            let prettyStrs =
                List.mapi (fun i prettySymbol ->
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
                    let text = prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter
                    whitespace + text)
                    symbols
            let prettyStr = Symbol.OpenSymbolsStr + String.concat "" prettyStrs + Symbol.CloseSymbolsStr
            prettyStr
        else
            let prettyStrs =
                List.map (fun prettySymbol ->
                    prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter)
                    symbols
            let prettyStr = Symbol.OpenSymbolsStr + String.concat " " prettyStrs + Symbol.CloseSymbolsStr
            prettyStr

    and private prettySymbolToPrettyStr depth prettySymbol prettyPrinter =
        match prettySymbol with
        | PrettyAtom (_, _, _, _, symbol)
        | PrettyNumber (_, symbol)
        | PrettyString (_, symbol) -> Symbol.writeSymbol symbol
        | PrettyIndex (depth, prettyIndexer, prettyTarget) ->
            let prettyIndexerStr = prettySymbolToPrettyStr depth prettyIndexer prettyPrinter
            let prettyTargetStr = prettySymbolToPrettyStr depth prettyTarget prettyPrinter
            match prettyIndexer with
            | PrettyNumber _ -> prettyTargetStr + Symbol.IndexStr + Symbol.OpenSymbolsStr + prettyIndexerStr + Symbol.CloseSymbolsStr
            | _ -> prettyTargetStr + Symbol.IndexStr + prettyIndexerStr
        | PrettyQuote (_, prettySymbol) ->
            let prettyStr = prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter
            Symbol.QuoteStr + prettyStr
        | PrettySymbols (titled, headered, maxDepth, symbols) ->
            let unfolding = depth < prettyPrinter.ThresholdMin || maxDepth > prettyPrinter.ThresholdMax
            prettySymbolsToPrettyStr titled headered depth unfolding symbols prettyPrinter

    let prettyPrintSymbol symbol prettyPrinter =
        let prettySymbol = symbolToPrettySymbol symbol prettyPrinter
        prettySymbolToPrettyStr 0 prettySymbol prettyPrinter

    let prettyPrint str prettyPrinter =
        let symbol = Symbol.fromString str
        prettyPrintSymbol symbol prettyPrinter

type [<AttributeUsage (AttributeTargets.Class); AllowNullLiteral>]
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
    static member getOrDefault (ty : Type) =
        match ty.GetCustomAttribute<SyntaxAttribute> true with
        | null ->
            SyntaxAttribute
                ("", "", "", "", "",
                 PrettyPrinter.defaulted.ThresholdMin,
                 PrettyPrinter.defaulted.ThresholdMax)
        | syntax -> syntax
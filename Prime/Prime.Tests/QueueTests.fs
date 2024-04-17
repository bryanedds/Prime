// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open NUnit.Framework
open Prime
module QueueTests =

    let [<Test>] canSymbolizeQueue () =
        let value = FQueue.ofSeq [0; 1; 2]
        let symbol = valueToSymbol value
        let value' = symbolToValue<int FQueue> symbol
        Assert.Equal (value, value')
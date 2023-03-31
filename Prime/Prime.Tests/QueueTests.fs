// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open Xunit
open Prime
module QueueTests =

    let [<Fact>] canSymbolizeQueue () =
        let value = Queue.ofSeq [0; 1; 2]
        let symbol = valueToSymbol value
        let value' = symbolToValue<int Queue> symbol
        Assert.Equal<int Queue> (value, value')
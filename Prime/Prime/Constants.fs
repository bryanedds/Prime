// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime
open System

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Runtime =

        let [<Literal>] LohSize = 85000 // the approximate size of the .NET Large-Object Heap in bytes, although this seems misleading for some uses...

    [<RequireQualifiedAccess>]
    module Scripting =
    
        let [<Literal>] ViolationSeparator = '/'
        let [<Literal>] ViolationSeparatorStr = "/"
        let [<Literal>] PreludeFilePath = "Prelude.amsl"

    [<RequireQualifiedAccess>]
    module PrettyPrinter =
    
        let [<Literal>] DefaultThresholdMin = 0
        let [<Literal>] StructuredThresholdMin = 3
        let [<Literal>] SimpleThresholdMax = 1
        let [<Literal>] DefaultThresholdMax = 2
        let [<Literal>] DetailedThresholdMax = 3
        let [<Literal>] CompositionalThresholdMax = 4
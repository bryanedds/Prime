// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Ecs
open System
open Prime

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Ecs =

        let [<Literal>] ArrayReserve = 256 // just large enough to amortize cache misses
        let [<Literal>] ParallelTaskSizeMinimum = 1024
        let [<Literal>] PreallocateAmount = ArrayReserve
        let [<Literal>] IntraComponentPrefix = "@"
        let [<Literal>] UnscheduledEventSuffix = "!U"
        let [<Literal>] ScheduledEventSuffix = "!S"
﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open Xunit
open Prime
module RandTests =

    let [<Literal>] Samples = 32768

    let makeSamples (next : Rand -> ('n * Rand)) =
        let mutable rand = Rand.make ()
        [for _ in 0 .. Samples - 1 do
            let (n, rand') = next rand
            rand <- rand'
            yield n]

    let [<Fact>] nextDoubleIsInRange () =
        let samples = makeSamples Rand.nextDouble
        let avg = List.average samples
        Assert.InRange (avg, 0.49, 0.51)

    let [<Fact>] nextSingleIsInRange () =
        let samples = makeSamples Rand.nextSingle
        let avg = List.average samples
        Assert.InRange (avg, 0.49f, 0.51f)

    let [<Fact>] nextIntIsInRange () =
        let samples = makeSamples Rand.nextInt
        let sampleDoubles = List.map double samples
        let avg = List.average sampleDoubles
        Assert.InRange (avg, 1003741823.0, 1143741823.0)
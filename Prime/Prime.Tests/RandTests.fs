// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open NUnit.Framework
open Prime
module RandTests =

    let [<Literal>] Samples = 32768

    let makeSamples (next : Rand -> ('n * Rand)) =
        let mutable rand = Rand.make ()
        [for _ in 0 .. Samples - 1 do
            let (n, rand') = next rand
            rand <- rand'
            yield n]

    let [<Test>] nextDoubleIsInRange () =
        let samples = makeSamples Rand.nextDouble
        let avg = List.average samples
        Assert.Greater (avg, 0.49)
        Assert.Less (avg, 0.51)

    let [<Test>] nextSingleIsInRange () =
        let samples = makeSamples Rand.nextSingle
        let avg = List.average samples
        Assert.Greater (avg, 0.49)
        Assert.Less (avg, 0.51)

    let [<Test>] nextIntIsInRange () =
        let samples = makeSamples Rand.nextInt
        let sampleDoubles = List.map double samples
        let avg = List.average sampleDoubles
        Assert.Greater (avg, 1003741823.0)
        Assert.Less (avg, 1143741823.0)
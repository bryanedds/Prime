// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime.Tests
open System
open Prime
open Prime.Tests
module Program =

    let [<EntryPoint; STAThread>] main _ =
        TimingTests.run ()
        0
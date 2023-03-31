// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open Prime.Tests
module Program =

    let [<EntryPoint; STAThread>] main _ =
        TimingTests.run ()
        0
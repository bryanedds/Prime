// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime.Tests
open System
open Prime.Tests
module Program =

    let [<EntryPoint; STAThread>] main _ =
        SymbolTests.canConvertStringToRecordAbstract ()
        TimingTests.run ()
        0
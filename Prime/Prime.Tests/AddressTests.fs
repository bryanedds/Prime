// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime.Tests
open System
open Xunit
open Prime
module AddressTests =

    let [<Fact>] addressConverterConvertToStringWorks () =
        let converter = AddressConverter (typeof<unit Address>)
        let address = rtoa<unit> [|"Some"; "Thing"|]
        Assert.Equal ("Some/Thing" :> obj, converter.ConvertToString address)

    let [<Fact>] addressConverterConvertFromStringWorks () =
        let converter = AddressConverter (typeof<unit Address>)
        Assert.Equal (rtoa<unit> [|"On"; "The"; "Wing"|] :> obj, converter.ConvertFromString "On/The/Wing")
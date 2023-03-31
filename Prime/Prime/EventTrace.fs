// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Prime
open System
open Prime

/// Conveys debugging information about an event.
type [<StructuralEquality; StructuralComparison; CLIMutable>] EventInfo =
    { ModuleName : string
      FunctionName : string
      MoreInfo : string }

    /// Make event information.
    static member make moduleName functionName moreInfo =
        { ModuleName = moduleName
          FunctionName = functionName
          MoreInfo = moreInfo }

/// Conveys an event's trace information.
type EventTrace = EventInfo list

[<RequireQualifiedAccess>]
module EventTrace =

    /// Record an event trace.
    let record moduleName functionName moreInfo eventTrace : EventTrace =
        EventInfo.make moduleName functionName moreInfo :: eventTrace

    /// The empty event trace.
    let empty : EventTrace = []
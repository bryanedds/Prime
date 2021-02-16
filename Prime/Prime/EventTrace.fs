// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Prime
open System
open Prime

/// Conveys debugging info about an event.
type [<StructuralEquality; StructuralComparison; CLIMutable>] EventInfo =
    { ModuleName : string
      FunctionName : string
      MoreInfo : string }

    /// Record event information.
    static member make moduleName functionName =
        { ModuleName = moduleName
          FunctionName = functionName
          MoreInfo = String.Empty }

    /// Record event information with greater detail.
    static member make3 moduleName functionName moreInfo =
        { ModuleName = moduleName
          FunctionName = functionName
          MoreInfo = moreInfo }

/// Conveys an event's trace information.
type EventTrace = EventInfo list

[<RequireQualifiedAccess>]
module EventTrace =

    /// Trace event information.
    let trace moduleName functionName eventTrace : EventTrace =
        EventInfo.make moduleName functionName :: eventTrace

    /// Trace event information with greater detail.
    let trace4 moduleName functionName moreInfo eventTrace : EventTrace =
        EventInfo.make3 moduleName functionName moreInfo :: eventTrace

    /// The empty event trace.
    let empty : EventTrace = []
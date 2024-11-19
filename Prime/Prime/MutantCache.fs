// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds.

namespace Prime

[<RequireQualifiedAccess>]
module MutantCache =

    /// Presents a purely-functional interface to a mutable object / record / whatever.
    /// If it is not satisfactorily efficient to run a clone operation on the mutant for every get,
    /// just pass in the id function for make's cloneMutant arg, but make sure to NEVER mutate the
    /// returned mutant!
    type [<ReferenceEquality>] 'm MutantCache =
        private
            { CloneMutant : 'm -> 'm
              mutable ValidMutantOpt : 'm option }

    let private rebuildCache (discardMutant : 'm -> unit) (rebuildMutant : unit -> 'm) (mutantCache : 'm MutantCache) =
        match mutantCache.ValidMutantOpt with
        | Some mutant -> discardMutant mutant
        | None -> ()
        let validMutant = rebuildMutant ()
        mutantCache.ValidMutantOpt <- None
        let mutantCache = { mutantCache with ValidMutantOpt = Some validMutant }
        (validMutant, mutantCache)

    let private getMutantUncloned discardMutant rebuildMutant (mutantCache : 'm MutantCache) =
        match mutantCache.ValidMutantOpt with
        | Some mutant -> (mutant, mutantCache)
        | None -> rebuildCache discardMutant rebuildMutant mutantCache

    /// <summary>Get the underlying mutant (mutable object / record / whatever).</summary>
    /// <param name="discardMutant">A function that discard any desired resources from the current mutant in case the current mutant is out of date.</param>
    /// <param name="rebuildMutant">A function that rebuilds the mutant from scratch in case the current mutant is out of date.</param>
    /// <param name="mutantCache">The mutant cache.</param>
    let getMutant discardMutant rebuildMutant (mutantCache : 'm MutantCache) =
        let (mutantUncloned, mutantCache) = getMutantUncloned discardMutant rebuildMutant mutantCache
        let mutantCloned = mutantCache.CloneMutant mutantUncloned
        (mutantCloned, mutantCache)

    /// <summary>Mutate the underlying mutant (mutable object / record / whatever).</summary>
    /// <param name="rebuildMutant">A function that rebuilds the mutant from scratch in case the current underlying mutant is out of date.</param>
    /// <param name="mutateMutant">A function that mutates the underlying mutant.</param>
    /// <param name="mutantCache">The mutant cache.</param>
    let mutateMutant discardMutant rebuildMutant mutateMutant (mutantCache : 'm MutantCache) =
        let (mutant, mutantCache) = getMutantUncloned discardMutant rebuildMutant mutantCache
        mutantCache.ValidMutantOpt <- None
        let mutant = mutateMutant mutant
        { mutantCache with ValidMutantOpt = Some mutant }

    /// <summary>Make a mutant cache.</summary>
    /// <param name="cloneMutant">
    /// A function to clone the mutant before presenting it to the outside world.
    /// Feel free to pass id if you can ensure that the presented mutant will never be mutated externally.
    /// </param>
    /// <param name="mutant">The mutant (mutable object / record / whatever) to be cached.</param>
    let make cloneMutant (mutant : 'm) =
        { CloneMutant = cloneMutant
          ValidMutantOpt = Some mutant }

/// Presents a purely-functional interface to a mutable object / record / whatever.
/// If it is not satisfactorily efficient to run a clone operation on the mutant for every get,
/// just pass in the id function for make's cloneMutant arg, but make sure to NEVER mutate the
/// returned mutant!
type 'm MutantCache = 'm MutantCache.MutantCache
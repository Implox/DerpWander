module WorldOptions

open PlantOptionAlg
open DerpOptionAlg

open System

type WorldSize = int

/// The number of states in each Derp's brain.
type StateCount = int

/// The number of Derps in a world.
type DerpCount = int

/// The threshold for a derp being mutated
type MutationThreshold = float

/// Flags for plant growth patterns in the world.
type GrowthPatternOption =
    | Clumps
    | NearBottom
    | Rows
    | Random

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GrowthPatternOption =
    let resolve (pattern : GrowthPatternOption) =
        match pattern with
        | Clumps -> PlantOptionAlg.PlantGrowth.clump
        | NearBottom -> PlantOptionAlg.PlantGrowth.nearBottom
        | Rows -> failwith "Not Implemented"
        | Random -> PlantOptionAlg.PlantGrowth.random

/// Flags for where an eaten plant respawns.
type PlantRespawnOption =
    | Nearby
    | Anywhere
    | Never

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PlantRespawnOption =
    let resolve (respawnOption : PlantRespawnOption) =
        match respawnOption with
        | Nearby -> failwith "Not Implemented"
        | Anywhere -> PlantOptionAlg.PlantRespawn.anywhereRespawn

/// Flags for where Derps spawn every new generation.
type DerpRespawnOption =
    | Random
    | Center
    | SameAsParent

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DerpRespawnOption =
    let resolve (respawnOption : DerpRespawnOption) =
        match respawnOption with
        | Random        -> DerpOptionAlg.random
        | Center        -> failwith "Not Implemented"
        | SameAsParent  -> failwith "Not Implemented"

/// Flags for how fast the world updates (in milliseconds per frame).
type GenSpeed =
    | FastestNoDisp = 0
    | Fastest       = 1
    | Faster        = 125
    | Fast          = 250
    | Normal        = 500
    | Slow          = 1000
    | Slowest       = 2000

/// Represents a complete set of options for a World.
type OptionSet (worldSize: WorldSize, derpCount : DerpCount, stateCount : StateCount, 
                           growthOption : GrowthPatternOption, plantRespawnOption : PlantRespawnOption, 
                           derpRespawnOption : DerpRespawnOption, speed : GenSpeed, threshold : MutationThreshold) =

    let mutable speed = speed

    /// The square side length of the world.
    member this.WorldSize = worldSize

    /// The number of Derps in the world.
    member this.DerpCount = derpCount

    /// The number of states for each Derp's brain.
    member this.StateCount = stateCount

    /// The function used to generate plant growth in the world.
    member this.PlantGrowthFunc = GrowthPatternOption.resolve growthOption

    /// The function used to respawn eaten plants.
    member this.PlantRespawnFunc = PlantRespawnOption.resolve plantRespawnOption

    /// The function used to respawn derps after a generation.
    member this.DerpRespawnOp = DerpRespawnOption.resolve derpRespawnOption

    /// The GenSpeed option for the world.
    member this.Speed
        with get () = speed
        and set value = speed <- value

    /// The percent probability threshold for the mutation of a Derp in the world.
    member this.Threshold = threshold
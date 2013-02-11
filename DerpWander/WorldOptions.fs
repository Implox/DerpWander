module WorldOptions

open PlantOptionAlg
open DerpOptionAlg

type WorldSize = int

/// The number of states in each Derp's brain.
type StateCount = int

/// The number of Derps in a world.
type DerpCount = int

/// Flags for plant growth patterns in the world.
type GrowthPatternOption =
    | Clumps
    | NearBottom
    | Rows
    | Random

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GrowthPatternOption =
    let Resolve (pattern : GrowthPatternOption) =
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
    let Resolve (respawnOption : PlantRespawnOption) =
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
    let Resolve (respawnOption : DerpRespawnOption) =
        match respawnOption with
        | Random        -> DerpOptionAlg.random
        | Center        -> failwith "Not Implemented"
        | SameAsParent  -> failwith "Not Implemented"

/// Flags for how fast the world updates.
type GenSpeed =
    | Fastest = 50
    | Faster  = 125
    | Fast    = 250
    | Normal  = 500
    | Slow    = 1000
    | Slowest = 2000

/// Represents a complete set of options for a World.
type [<Struct>] OptionSet (worldSize: WorldSize, derpCount : DerpCount, stateCount : StateCount, 
                           growthOption : GrowthPatternOption, plantRespawnOption : PlantRespawnOption, 
                           derpRespawnOption : DerpRespawnOption, speed : GenSpeed) = 
    member this.WorldSize = worldSize
    member this.DerpCount = derpCount
    member this.StateCount = stateCount
    member this.PlantGrowthFunc = GrowthPatternOption.Resolve growthOption
    member this.PlantRespawnFunc = PlantRespawnOption.Resolve plantRespawnOption
    member this.DerpRespawnOp = DerpRespawnOption.Resolve derpRespawnOption
    member this.Speed = speed

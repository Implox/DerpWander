module WorldOptions

open PlantOptionAlg
open DerpOptionAlg

open System

/// Flags for plant growth patterns in the world.
type GrowthPatternOption = Clumped | NearBottom | Rows | Random

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GrowthPatternOption =
    let toFunc (pattern : GrowthPatternOption) =
        match pattern with
        | Clumped -> PlantOptionAlg.PlantGrowth.clumped
        | NearBottom -> PlantOptionAlg.PlantGrowth.nearBottom
        | Rows -> failwith "Not Implemented"
        | Random -> PlantOptionAlg.PlantGrowth.random

/// Flags for where an eaten plant respawns.
type PlantRespawnOption = Nearby | Anywhere | Never

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PlantRespawnOption =
    let toFunc (respawnOption : PlantRespawnOption) =
        match respawnOption with
        | Nearby -> PlantOptionAlg.PlantRespawn.anywhere
        | Anywhere -> PlantOptionAlg.PlantRespawn.anywhere
        | Never -> PlantOptionAlg.PlantRespawn.never

/// Flags for where Derps spawn every new generation.
type DerpRespawnOption = Random | Center | SameAsParent

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DerpRespawnOption =
    let toFunc (respawnOption : DerpRespawnOption) =
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
type OptionSet (worldSize : int * int, derpPairCount : int, stateCount : int, 
                growthOption : GrowthPatternOption, plantRespawnOption : PlantRespawnOption,
                derpRespawnOption : DerpRespawnOption, speed : GenSpeed,
                mutationThreshold : float, plantRespawnThreshold : float) =

    let mutable speed = speed

    /// The square side length of the world.
    member this.WorldSize = worldSize

    /// The number of Derps in the world.
    member this.DerpCount = 
        let derpcount = derpPairCount * 2
        if derpcount >= (this.WorldSize |> (fun (x, y) -> x*y)) 
            then failwith "Too many derps for world size"
        else derpcount

    /// The number of states for each Derp's brain.
    member this.StateCount = stateCount

    /// The function used to generate plant growth in the world.
    member this.PlantGrowthFunc = GrowthPatternOption.toFunc growthOption

    /// The function used to respawn eaten plants.
    member this.PlantRespawnFunc = PlantRespawnOption.toFunc plantRespawnOption

    /// The function used to respawn derps after a generation.
    member this.DerpRespawnOp = DerpRespawnOption.toFunc derpRespawnOption

    /// The GenSpeed option for the world.
    member this.Speed
        with get () = speed
        and set value = speed <- value

    /// The percent probability threshold for the mutation of a Derp in the world.
    member this.MutationThreshold = mutationThreshold

    /// The percent probability threshold for a plant respawning after being eaten.
    member this.PlantRespawnThreshold = plantRespawnThreshold
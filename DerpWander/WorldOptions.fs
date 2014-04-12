module WorldOptions

open Util
open PlantOptionAlg
open DerpOptionAlg

open System

let plantGrowthFuncs = 
    [("Clumped", PlantOptionAlg.PlantGrowth.clumped);
     ("NearBottom", PlantOptionAlg.PlantGrowth.nearBottom);
     ("Random", PlantOptionAlg.PlantGrowth.random)]

let plantRespawnFuncs =
    [("Nearby", PlantOptionAlg.PlantRespawn.nearby);
     ("Anywhere", PlantOptionAlg.PlantRespawn.anywhere);
     ("Never", PlantOptionAlg.PlantRespawn.never)]

let derpRespawnFuncs =
    [("Random", DerpOptionAlg.random)]

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
                growthOption : string, plantRespawnOption : string,
                derpRespawnOption : string, speed : GenSpeed,
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
    member this.PlantGrowthFunc = growthOption |> mappedBy plantGrowthFuncs

    /// The function used to respawn eaten plants.
    member this.PlantRespawnFunc = plantRespawnOption |> mappedBy plantRespawnFuncs

    /// The function used to respawn derps after a generation.
    member this.DerpRespawnFunc = derpRespawnOption |> mappedBy derpRespawnFuncs

    /// The GenSpeed option for the world.
    member this.Speed
        with get () = speed
        and set value = speed <- value

    /// The percent probability threshold for the mutation of a Derp in the world.
    member this.MutationThreshold = mutationThreshold

    /// The percent probability threshold for a plant respawning after being eaten.
    member this.PlantRespawnThreshold = plantRespawnThreshold
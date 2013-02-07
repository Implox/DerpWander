module WorldOptions

open PlantOptionAlg
open DerpOptionAlg

/// Flags for plant growth patterns in the world.
type GrowthPatternOption =
    | Clumps
    | NearBottom
    | Rows
    | Random
with
    /// Returns the given growth pattern option's respective function.
    static member Match (pattern : GrowthPatternOption) =
        match pattern with
        | Clumps -> PlantOptionAlg.clump
        | NearBottom -> PlantOptionAlg.nearBottom
        | Rows -> failwith "Not Implemented"
        | Random -> PlantOptionAlg.random

/// Flags for where an eaten plant respawns.
type PlantRespawnOption =
    | Nearby
    | Anywhere
    | Never
with
    static member Match (respawnOption : PlantRespawnOption) =
        match respawnOption with
        | Nearby -> failwith "Not Implemented"
        | Anywhere -> PlantOptionAlg.anywhereRespawn

/// Flags for where Derps spawn every new generation.
type DerpRespawnOption =
    | Random
    | Center
    | SameAsParent
with
    static member Match (respawnOption : DerpRespawnOption) =
        match respawnOption with
        | Random        -> DerpOptionAlg.random
        | Center        -> failwith "Not Implemented"
        | SameAsParent  -> failwith "Not Implemented"

/// Flags for how fast the world updates.
type GenSpeedOption =
    | Fastest
    | Fast
    | Normal
    | Slow
    | Slowest
with
    /// Resolves a GenSpeedOption to a value in miliseconds per frame.
    static member Match (speedOption : GenSpeedOption) =
        match speedOption with
        | Fastest   -> 125
        | Fast      -> 250
        | Normal    -> 500
        | Slow      -> 1000
        | Slowest   -> 2000

/// Represents a complete set of options for a World.
type OptionSet (growthOption : GrowthPatternOption, plantRespawnOption : PlantRespawnOption, derpRespawnOption : DerpRespawnOption, speedOption : GenSpeedOption) = 
    member this.PlantGrowthFunc = GrowthPatternOption.Match growthOption
    member this.PlantRespawnFunc = PlantRespawnOption.Match plantRespawnOption
    member this.DerpRespawnOp = DerpRespawnOption.Match derpRespawnOption
    member this.Speed = GenSpeedOption.Match speedOption
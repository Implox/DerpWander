module WorldOptions

open Util
open PlantOptionAlg
open DerpOptionAlg

open System

let plantGrowthFuncs = 
    [| ("Clumped",     PlantOptionAlg.PlantGrowth.clumped);
       ("Near Bottom", PlantOptionAlg.PlantGrowth.nearBottom);
       ("Random",      PlantOptionAlg.PlantGrowth.random) |]

let plantRespawnFuncs =
    [| ("Nearby",   PlantOptionAlg.PlantRespawn.nearby);
       ("Anywhere", PlantOptionAlg.PlantRespawn.anywhere);
       ("Never",    PlantOptionAlg.PlantRespawn.never) |]

let derpSpawnFuncs =
    [| ("Random", DerpOptionAlg.random) |]

let genSpeeds =
    [| ("Paused",              -1);
       ("Fastest (No Display)", 0);
       ("Fastest",              1);
       ("Faster",             125);
       ("Fast",               250);
       ("Normal",             500);
       ("Slow",              1000);
       ("Slowest",           2000) |]

/// Module containing general world options
module GeneralOptions =
    type T = { worldSize        : int * int;
               derpCount        : int;
               mutable genSpeed : string }

    let create size pairCount speed = 
        { worldSize = size; 
          derpCount = pairCount * 2; 
          genSpeed  = speed }

    /// The combined width and height of a the world
    let size options = options.worldSize

    /// The number of pairs of derps in the world
    let derpCount options = options.derpCount

    /// The speed (in milliseconds per step) of world simulation.
    let genSpeed options = options.genSpeed
    let speedValue options = genSpeed options |> mappedBy genSpeeds

/// Module containing options relevant to Derps in the world
module DerpOptions =
    type T = { genomeMutationThreshold : double;
               codonMutationThreshold  : double;
               dominanceThreshold      : double;
               stateCount              : int;
               spawnOption             : string }

    let create genomeThreshold codonThreshold dominanceThreshold stateCount respawnOption =
        { genomeMutationThreshold = genomeThreshold;
          codonMutationThreshold  = codonThreshold;
          dominanceThreshold      = dominanceThreshold;
          stateCount              = stateCount;
          spawnOption             = respawnOption }

    /// The likelihood of a Derp's genome being mutated during mating
    let genomeMutationThreshold options = options.genomeMutationThreshold

    /// The likelihood of an individual codon within a Derp's genome being mutated during mating
    let codonMutationThreshold options = options.codonMutationThreshold

    /// The likelihood of a more fit member's genes being selected
    /// over those of a less fit member.
    let dominanceThreshold options = options.dominanceThreshold

    /// The number of states in a Derp brain
    let stateCount options = options.stateCount

    /// The spawning pattern for Derps
    let spawnOption options = options.spawnOption
    let spawnFunc options = spawnOption options |> mappedBy derpSpawnFuncs

/// Module containing options relevant to plants in the world
module PlantOptions =
    type T = { respawnThreshold : double;
               growthOption     : string;
               respawnOption    : string }

    let create respawnThreshold growth respawn =
        { respawnThreshold = respawnThreshold;
          growthOption     = growth;
          respawnOption    = respawn }

    /// The pattern of initial plant growth at the start of a generation
    let growthOption options = options.growthOption
    let growthFunc options = growthOption options |> mappedBy plantGrowthFuncs

    /// The pattern of new plants spawning in the middle of a generation
    let respawnOption options = options.respawnOption
    let respawnFunc options = respawnOption options |> mappedBy plantRespawnFuncs

    /// The likelihood of a given plant respawning after being eaten
    let respawnThreshold options = options.respawnThreshold

/// Module containing a complete set of necessary options for a world
module CompleteOptionSet =
    type T = { generalOptions : GeneralOptions.T;
               derpOptions    : DerpOptions.T;
               plantOptions   : PlantOptions.T }

    let create gen derp plant =
        { generalOptions = gen;
          derpOptions = derp;
          plantOptions = plant }
    
    /// The set of all general world options
    let generalOptions options = options.generalOptions
    
    /// The set of all derp-related options
    let derpOptions options = options.derpOptions

    /// The set of all plant-related options
    let plantOptions options = options.plantOptions
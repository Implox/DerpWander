module GeneticAlgo

open System

open Util

/// Represents a member of a population
type Chromosome<'a> (value : 'a [], target : 'a [], fitness : 'a [] -> 'a [] -> int) =
    member this.Value = value
    member this.Target = target
    member this.Fitness = fitness value target

type Population<'a> = Chromosome<'a> list

/// Sorts chromosomes by their fitness values (best fitness is 0).
let rank (population : Population<'a>) = (population |> List.sortBy (fun chromo -> chromo.Fitness))

/// Mutates the chromosomes in a population using given mutator function.
let mutate (population : Population<'a>) mutator threshold fitness target =
    population.Head :: [for chromosome in population.Tail do
                            if (Random ()).NextDouble() < threshold then
                                let value = mutator chromosome.Value
                                yield Chromosome (value, target, fitness)
                            else yield chromosome]

/// Randomly selects one of two given members, preferring the first member because it is more fit.
let updateVals fitness target (c1 : Chromosome<'a>) (c2 : Chromosome<'a>) =
    let value = [| for i = 0 to c1.Value.Length - 1 do if (Random ()).NextDouble() > 0.3 then yield c1.Value.[i] else yield c2.Value.[i] |]
    Chromosome (value, target, fitness)

/// Splits a given population in half and mates all its members.
let mate (population : Population<'a>) fitness target =
    let partA, partB = List.splitAt (population.Length / 2) population
    List.map2 (fun a b -> updateVals fitness target a b) partA partB


/// Mutate the given population, then promote top members and add mated members to the end.
let evolveStep (population : Population<'a>) mutator threshold fitness target =
    let size = population.Length
    let mutated = mutate population mutator threshold fitness target |> rank
    let promoteSize = size / 5
    let keepSize = (size / 2) - promoteSize
    let xs, ys = List.splitAt keepSize mutated
    xs @ (List.take promoteSize ys) @ (mate mutated fitness target)
    
/// Generates a new member for the population
let genMember mutator fitness target =
    let value = mutator target
    Chromosome (value, target, fitness)

/// Creates a new population
let initPopulation size initGen fitness target =
    List.init size (fun _ -> genMember initGen fitness target) |> rank

/// Evolves a population until at least one member meets the criteria set by the given fitness function.
let evolve size threshold initGen mutator fitness target =
    let rec loop (population : Population<'a>) lastBestMember generation=
        match population.Head.Fitness with
        | 0 -> 
            printf "%i\n" generation
            population
        | _ -> 
            if population.Head.Value <> lastBestMember then
                printf "Generation: %i\n" generation
                printf "Best: %A (%i)\n" (population.Head.Value) (population.Head.Fitness)
                printf "Worst: %A (%i)\n\n" ((population.[population.Length - 1]).Value) ((population.[population.Length - 1]).Fitness)
            loop (evolveStep population mutator threshold fitness target) population.Head.Value (generation + 1)
    loop (initPopulation size initGen fitness target) [||] 0    
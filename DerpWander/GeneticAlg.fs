/// Relevant types and functions for genetic algorithm. Adapted from a previous project.
module GeneticAlg

open System

open Util

/// Represents the genetic information of a member of a population
type DNA = { Actionsome : double []; Statesome : double []; mutable Fitness : float; }

type Population = DNA list

/// Sorts DNAs by their fitness values (worst fitness is 0).
let rank (population : Population) = population |> List.sortBy (fun dna -> -dna.Fitness)

/// Mutates the DNAs in a population using given mutator function.    
let mutate (population : Population) mutator threshold =
    List.take 5 population @ [for dna in List.drop 5 population do
                                if (Random ()).NextDouble() < threshold then
                                    yield mutator dna
                                else yield dna]

/// Randomly selects each gene from either the first or second DNA, preferring the genes of the first because it is more fit.
let updateVals (d1 : DNA) (d2 : DNA) =
    let threshold = 0.90
    let actionsome = 
        [| for i = 0 to d1.Actionsome.Length - 1 do 
            if (Random ()).NextDouble() < threshold then yield d1.Actionsome.[i] 
            else yield d2.Actionsome.[i] |]
    let statesome = 
        [| for i = 0 to d1.Statesome.Length - 1 do 
            if (Random ()).NextDouble() < threshold then yield d1.Statesome.[i] 
            else yield d2.Statesome.[i] |]
    { Actionsome = actionsome; Statesome = statesome; Fitness = 0.0; }

/// Splits a given population in half and mates all its members.
let mate (population : Population) =
    let partA, partB = List.splitAt (population.Length / 2) population
    List.map2 (fun a b -> updateVals a b) partA partB

/// Mutate the given population, then promote top members and add mated members to the end.
let evolveStep (population : Population) mutator threshold =
    let size = population.Length
    let mutated = mutate population mutator threshold |> rank
    let promoteSize = size / 5
    let keepSize = (size / 2) - promoteSize
    let xs, ys = List.splitAt keepSize mutated
    let retVal = xs @ (List.take promoteSize ys) @ (mate mutated)
    retVal
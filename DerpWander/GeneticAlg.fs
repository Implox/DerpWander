/// Relevant types and functions for genetic algorithm. Adapted from a previous project.
module GeneticAlg

open System

open Util

type Fitness = int
type Codon = double
type Gene = Codon array

/// Represents the genetic information of a member of a population
module Genome =
    type T = { ActionGene : Gene; StateGene : Gene }
    let create (actionGene, stateGene) = { ActionGene = actionGene; StateGene = stateGene }
    let actionGene (gene : T) = gene.ActionGene
    let stateGene (gene : T) = gene.StateGene

type Population = (Genome.T * Fitness) list

/// Sorts DNAs by their fitness values (worst fitness is 0).
let rank : Population -> Population = List.sortBy (snd >> ((*) -1))

/// Mutates the DNAs in a population using given mutator function.    
let mutateBy mutator codonMutationThreshold genomeMutationThreshold =
    let mutate = mutator codonMutationThreshold
    List.map (fun (genome : Genome.T) -> 
        if rand.NextDouble () < genomeMutationThreshold then
            mutate genome
        else genome)

/// Randomly selects each codon from either the first or second gene, preferring the codons of the first because they are more fit.
let mateGenePair dominantPreference =
    Array.map2 (fun aCodon bCodon -> 
                    if rand.NextDouble () < dominantPreference then aCodon
                    else bCodon)

/// Splits a given population in half and mates all its members.
let mate dominanceThreshold (genomes : Genome.T list) =
    let matePair = mateGenePair dominanceThreshold
    genomes
    |> List.splitAt (genomes.Length / 2)
    ||> List.map2 (fun sup sub -> (matePair sup.ActionGene sub.ActionGene, 
                                   matePair sup.StateGene sub.StateGene))
    |> List.map Genome.create

/// Mutate the given population, then promote top members and add mated members to the end.
let evolve mutator codonMutationThreshold genomeMutationThreshold dominanceThreshold (population : Population) =
    let population = rank population
    let size = population.Length
    let genomes = population |> List.map fst
    let mutated = mutateBy mutator codonMutationThreshold genomeMutationThreshold genomes
    let promoteSize = size / 5
    let keepSize = (size / 2) - promoteSize
    let xs, ys = List.splitAt keepSize mutated
    let retVal = xs @ (List.take promoteSize ys) @ (mate dominanceThreshold mutated)
    retVal
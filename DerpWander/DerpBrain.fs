module DerpBrain

open System
open System.Collections.Generic

open Microsoft.FSharp.Reflection

open Util
open VoseAlg
open GeneticAlg

type State = int

/// Represents each of the potential actions a Derp can perform.
type Action = MoveForward | TurnBack | TurnLeft | TurnRight

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Action =
    let cases = 
        FSharpType.GetUnionCases typedefof<Action>
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))
    let count = cases.Length
    let getIndex (action : Action) = Array.IndexOf (cases, action)
    let matchIndex (index : int) = cases.[index] :?> Action


/// Represents each of the things a Derp can see in front of itself.
type Sight = Derp | Food | Empty

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sight =
    let cases = 
        FSharpType.GetUnionCases typedefof<Sight> 
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))
    let count = cases.Length
    let getIndex (sight : Sight) = Array.IndexOf (cases, sight)


/// The brain of a Derp. Performs all calculations relevant to decision-making.
type DerpBrain (stateCount : int, actionMatrix : double [,], stateMatrix : double [,]) =
    let actionAliases = 
        actionMatrix
        |> Array2D.flatten
        |> Array.breakBy Action.count
        |> Array.map (Array.normalize >> alias)

    let stateAliases =
        stateMatrix
        |> Array2D.flatten
        |> Array.breakBy stateCount
        |> Array.map (Array.normalize >> alias)

    /// Represents a matrix of functions which choose the next action of the
    /// derp given its current state and what it's seeing
    let actionAliasMatrix = 
        match stateCount with
        | 1 -> Array2D.init Sight.count stateCount (fun i _ -> actionAliases.[i])
        | _ -> Array2D.init Sight.count stateCount (fun i j -> actionAliases.[i + Sight.count * j])

    /// Represents a matrix of functions which choose the next state of the
    /// derp given its current state and what it's seeing
    let stateAliasMatrix  = 
        match stateCount with
        | 1 -> Array2D.init Sight.count stateCount (fun i _ -> stateAliases.[i])
        | _ -> Array2D.init Sight.count stateCount (fun i j -> stateAliases.[i + Sight.count * j])

    /// Samples the proper action matrix to get the next action performed by the derp
    let sampleActionMatrix (state : State) (sight : Sight) =
        let index = Sight.getIndex sight
        let chooseAction = actionAliasMatrix.[index, state]
        chooseAction () |> Action.matchIndex

    /// Samples the proper state matrix to get the next state for the derp
    let sampleStateMatrix (state : State) (sight : Sight) =
        let index = Sight.getIndex sight
        let chooseState = stateAliasMatrix.[index, state] 
        chooseState ()

    new (stateCount : int) =
        DerpBrain (stateCount, 
                   Array2D.init (stateCount * Sight.count) Action.count (fun _ _ -> rand.NextDouble ()), 
                   Array2D.init (stateCount * Sight.count) stateCount   (fun _ _ -> rand.NextDouble ()))

    new (stateCount : int, genome : Genome.T) = 
        DerpBrain (stateCount,
                   Array.elevate genome.ActionGene (stateCount * Sight.count) Action.count,
                   Array.elevate genome.StateGene (stateCount * Sight.count) stateCount)

    /// The number of states in this brain
    member this.StateCount = stateCount

    /// The matrix of probability distributions for selecting the next action
    /// based on the current state and what is being seen by the Derp
    member this.ActionMatrix = actionMatrix

    /// The matrix of probability distributions for selecting the next state
    /// based on the current state and what is being seen by the derp
    member this.StateMatrix = stateMatrix

    /// Gets the next action and the next state for the brain based on what
    /// it sees and what state it already is in
    member this.Sample (state : State) (sight : Sight) =
        (sampleActionMatrix state sight, sampleStateMatrix state sight)

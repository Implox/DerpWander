module DerpBrain

open System
open System.Collections.Generic

open Microsoft.FSharp.Reflection

open Util
open VoseAlg
open GeneticAlg

type State = int

/// Represents each of the potential actions a Derp can perform.
type Action =
    | MoveForward
    | TurnBack
    | TurnLeft
    | TurnRight

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Action =
    let cases = 
        FSharpType.GetUnionCases typedefof<Action>
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))
    let count = cases.Length
    let getIndex (action : Action) = Array.IndexOf (cases, action)
    let matchIndex (index : int) = cases.[index] :?> Action


/// Represents each of the things a Derp can see in front of itself.
type Sight =
    | Derp
    | Food
    | Empty

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
        |> Array.map Array.normalize
        |> Array.map (fun col -> alias col)

    let stateAliases =
        stateMatrix
        |> Array2D.flatten
        |> Array.breakBy stateCount
        |> Array.map Array.normalize
        |> Array.map (fun col -> alias col)

    /// Represents a matrix of functions which choose the next action of the
    /// derp given its current state and what it's seeing
    let actionAliasMatrix = 
        match stateCount with
        | 1 -> Array2D.init Sight.count stateCount (fun i j -> actionAliases.[i])
        | _ -> Array2D.init Sight.count stateCount (fun i j -> actionAliases.[i + Sight.count * j])

    /// Represents a matrix of functions which choose the next state of the
    /// derp given its current state and what it's seeing
    let stateAliasMatrix  = 
        match stateCount with
        | 1 -> Array2D.init Sight.count stateCount (fun i j -> stateAliases.[i])
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

    new (stateCount : int) = DerpBrain (stateCount, 
                                        Array2D.init (stateCount * Sight.count) Action.count (fun _ _ -> rand.NextDouble ()), 
                                        Array2D.init (stateCount * Sight.count) stateCount   (fun _ _ -> rand.NextDouble ()))

    new (stateCount : int, dna : DNA) = DerpBrain (stateCount,
                                                   Array.elevate dna.Actionsome (stateCount * Sight.count) Action.count,
                                                   Array.elevate dna.Statesome (stateCount * Sight.count) stateCount)

    member this.StateCount = stateCount

    member this.ActionMatrix = actionMatrix
    member this.StateMatrix = stateMatrix

    member this.Sample (state : State) (sight : Sight) =
        (sampleActionMatrix state sight, sampleStateMatrix state sight)

﻿module DerpBrain

open System
open System.Collections.Generic

open Microsoft.FSharp.Reflection

open Util
open VoseAlg

type State = int

/// Represents each of the potential actions a Derp can perform.
type Action =
    | MoveForward
    | MoveBackward
    | TurnLeft
    | TurnRight
with
    static member Cases = 
        FSharpType.GetUnionCases typedefof<Action>
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))
    static member Count = Action.Cases.Length
    static member GetIndex (action : Action) = Array.IndexOf (Action.Cases, action)
    static member MatchIndex (index : int) = Action.Cases.[index] :?> Action

/// Represents each of the things a Derp can see in front of itself.
type Sight =
    | Derp
    | Wall
    | Food
    | Empty
with
    static member Cases = 
        FSharpType.GetUnionCases typedefof<Sight> 
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))
    static member Count = Sight.Cases.Length
    static member GetIndex (sight : Sight) = Array.IndexOf (Sight.Cases, sight)

/// The brain of a Derp. Performs all calculations relevant to decision-making.
type DerpBrain (stateCount : int, actionMatrix : double [,], stateMatrix : double [,]) =
    let actionAliases = 
        [|for i = 0 to (Array2D.length1 (actionMatrix) - 1) do
            for j = 0 to (Array2D.length2 (actionMatrix) - 1) do
                yield actionMatrix.[i, j]|]
        |> Array.breakBy Action.Count
        |> Array.map (fun col -> alias col)

    let stateAliases =
        [|for i = 0 to (Array2D.length1 (stateMatrix) - 1) do
            for j = 0 to (Array2D.length2 (stateMatrix) - 1) do
                yield stateMatrix.[i, j]|]
        |> Array.breakBy stateCount
        |> Array.map (fun col -> alias col)

    let actionMatrix = Array2D.init Sight.Count stateCount (fun i j -> actionAliases.[i * Sight.Count + j])
    let stateMatrix  = Array2D.init Sight.Count stateCount (fun i j ->  stateAliases.[i * Sight.Count + j])

    let sampleActionMatrix (state : State) (sight : Sight) =
        let index = Sight.GetIndex sight
        actionMatrix.[state, index].Choose ()
        |> Action.MatchIndex

    let sampleStateMatrix (state : State) (sight : Sight) =
        let index = Sight.GetIndex sight
        stateMatrix.[state, index].Choose ()

    new (stateCount) = DerpBrain (stateCount, 
                                  Array2D.create (stateCount * Sight.Count) Action.Count 0.25, 
                                  Array2D.create (stateCount * Sight.Count) stateCount   0.25)

    member this.StateCount = stateCount
    member this.Sample (state : State) (sight : Sight) =
        (sampleActionMatrix state sight, sampleStateMatrix state sight)
module World

open System
open System.Collections.Generic

open Microsoft.FSharp.Reflection

open Util
open DerpBrain
open WorldOptions

/// Represents each of the possible orientations of a Derp.
type Orientation =
    | North
    | South
    | East
    | West
with
    /// Returns an orientation based on an action taken by a Derp.
    static member ResolveAction (action : Action) (orientation : Orientation) =
        match action with
        | MoveForward -> orientation
        | MoveBackward -> orientation
        | TurnLeft ->
            match orientation with
            | North -> West
            | South -> East
            | East -> North
            | West -> South
        | TurnRight ->
            match orientation with
            | North -> East
            | South -> West
            | East -> South
            | West -> North
    
    static member RandomCase () =
        let cases = 
            FSharpType.GetUnionCases typedefof<Action>
            |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))
        cases.[rand.Next cases.Length] :?> Orientation

/// Represents a Derp, a creature that walks around and tries to consume food.
type Derp (brain : DerpBrain, orientation : Orientation) =
    new (brain : DerpBrain) = new Derp (brain, Orientation.RandomCase ())
    let mutable orientation = orientation
    let mutable state = 0

    member this.Brain = brain
    member this.Orientation = orientation
    member this.State = state
    member this.Update (sight : Sight) =
        let action, nextState = this.Brain.Sample state sight
        orientation <- Orientation.ResolveAction action orientation
        action

/// Represents a cell in the world.
type Cell =
    | Derp of Derp
    | Food
    | Empty

type World (optionSet : OptionSet) =
    let size = 64
    let derpCount = 50
    let derps = 

    let map =
        let temp = Array2D.create size size Empty
        for p in (optionSet.PlantGrowthFunc size) do
            let x, y = p
            temp.[x, y] <- Cell.Food
        for p in (optionSet.DerpRespawnOp  size) do
            let x, y = p
            temp.[x, y] <- Cell.Derp 
        temp

    member this.Size = size
    member this.DerpCount = derpCount
    member this.Map = map
    member this.UpdateSpeed = optionSet.Speed
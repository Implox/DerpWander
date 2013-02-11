module Derp

open Microsoft.FSharp.Reflection

open Util
open DerpBrain

/// Represents each of the possible orientations of a Derp.
type Orientation =
    | North
    | South
    | East
    | West

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Orientation =
    /// Returns an orientation based on an action taken by a Derp.
    let ResolveAction (action : Action) (orientation : Orientation) =
        match action with
        | TurnLeft ->
            match orientation with
            | North -> West
            | South -> East
            | East  -> North
            | West  -> South
        | TurnRight ->
            match orientation with
            | North -> East
            | South -> West
            | East  -> South
            | West  -> North

    /// Returns the opposite of a given orientation.
    let Invert (orientation : Orientation) =
        match orientation with
        | North -> South
        | South -> North
        | East  -> West
        | West  -> East
    
    /// All the possible orientations.
    let Cases = 
        FSharpType.GetUnionCases typedefof<Orientation> 
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))

    let Count = Array.length Cases

    /// Returns a random orientation.
    let RandomCase () = Cases.[rand.Next Count] :?> Orientation

/// Represents a Derp, a creature that walks around and tries to consume food.
type Derp (brain : DerpBrain, orientation : Orientation) =
    let mutable orientation = orientation
    let mutable state = 0
    let mutable plantsEaten = 0

    new (brain : DerpBrain) = new Derp (brain, Orientation.RandomCase ())

    member this.Brain = brain
    member this.Orientation = orientation
    member this.State = state
    member this.PlantsEaten = plantsEaten

    member this.AddPlant () = plantsEaten <- plantsEaten + 1

    member this.Update (sight : Sight) =
        let action, nextState = this.Brain.Sample state sight
        state <- nextState
        if action = MoveBackward || action = MoveForward then Some action
        else 
            orientation <- Orientation.ResolveAction action orientation
            None

module Derp

open Microsoft.FSharp.Reflection

open Util
open GeneticAlg
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
    let resolveAction (action : Action) (orientation : Orientation) =
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
    let invert (orientation : Orientation) =
        match orientation with
        | North -> South
        | South -> North
        | East  -> West
        | West  -> East
    
    /// All the possible orientations.
    let cases = 
        FSharpType.GetUnionCases typedefof<Orientation> 
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))

    let count = Array.length cases

    /// Returns a random orientation.
    let randomCase () = cases.[rand.Next count] :?> Orientation

/// Represents a Derp, a creature that walks around and tries to consume food.
type Derp (brain : DerpBrain, orientation : Orientation) =
    let mutable orientation = orientation
    let mutable state = 0
    let mutable plantsEaten = 0

    new (brain : DerpBrain) = new Derp (brain, Orientation.randomCase ())

    member this.Brain = brain
    member this.Orientation = orientation
    member this.State = state
    member this.PlantsEaten = plantsEaten

    member this.Actionsome = this.Brain.ActionMatrix |> Array2D.flatten
    member this.Statesome = this.Brain.StateMatrix |> Array2D.flatten

    member this.AddPlant () = plantsEaten <- plantsEaten + 1

    member this.Update (sight : Sight) =
        let action, nextState = this.Brain.Sample state sight
        state <- nextState
        match action with
        | MoveBackward | MoveForward -> Some action
        | _ ->
            orientation <- Orientation.resolveAction action orientation
            None
    
    static member Mutator (dna : DNA) =
        if rand.NextDouble () < 0.60 then
            let i = rand.Next dna.Actionsome.Length
            dna.Actionsome.[i] <- dna.Actionsome.[i] + rand.NextDouble ()
        else
            let i = rand.Next dna.Statesome.Length
            dna.Statesome.[i] <- dna.Statesome.[i] + rand.NextDouble ()
        { Actionsome = Array.normalize dna.Actionsome; Statesome = Array.normalize dna.Statesome; Fitness = dna.Fitness}

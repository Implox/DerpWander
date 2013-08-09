module Derp

open System.Collections.Generic

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
        | MoveForward -> orientation
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
        | TurnBack ->
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

/// Stores information about a Derp for the purpose of computing its fitness 
/// at the end of a generation.
type Tracker () =
    let mutable timesMoved = 0
    let mutable plantsEaten = 0
    let visitedCells = HashSet<int * int> ()

    /// The number of times a Derp has successfully moved to a new location in
    /// the world. This value is used to differentiate between the number of 
    /// times a Derp deliberately moved to a location as opposed to when it 
    /// simply changed its orientation.
    member this.TimesMoved with get () = timesMoved

    /// The number of plants a Derp has eaten during a generation.
    member this.PlantsEaten with get () = plantsEaten

    /// Set of all the unique locations a Derp has visited during a generation.
    member this.VisitedCells with get () = visitedCells

    member this.IncMoves () =
        timesMoved <- timesMoved + 1

    member this.IncPlants () = 
        plantsEaten <- plantsEaten + 1

    member this.AddCell pos = visitedCells.Add pos |> ignore

    member this.GetFitness () = 
        float plantsEaten + (float visitedCells.Count / float timesMoved)


/// Represents a Derp, a creature that walks around and tries to consume food.
type Derp (brain : DerpBrain, orientation : Orientation) =
    let mutable orientation = orientation
    let mutable state = 0

    let tracker = Tracker ()

    new (brain : DerpBrain) = new Derp (brain, Orientation.randomCase ())

    member this.Brain = brain

    member this.Orientation
        with get () = orientation
        and set value = orientation <- value

    member this.State = state

    member this.Actionsome = this.Brain.ActionMatrix |> Array2D.flatten
    member this.Statesome = this.Brain.StateMatrix |> Array2D.flatten

    member this.Tracker = tracker

    member this.Update (sight : Sight) =
        let action, nextState = this.Brain.Sample state sight
        state <- nextState
        orientation <- Orientation.resolveAction action orientation
        if action = MoveForward then tracker.IncMoves ()
        action

    static member Mutator (dna : DNA) =
        if rand.NextDouble () < 0.60 then
            let i = rand.Next dna.Actionsome.Length
            dna.Actionsome.[i] <- dna.Actionsome.[i] + rand.NextDouble ()
        else
            let i = rand.Next dna.Statesome.Length
            dna.Statesome.[i] <- dna.Statesome.[i] + rand.NextDouble ()
        { Actionsome = Array.normalize dna.Actionsome; 
          Statesome = Array.normalize dna.Statesome; 
          Fitness = dna.Fitness }

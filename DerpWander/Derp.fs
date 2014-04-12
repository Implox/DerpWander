module Derp

open System.Collections.Generic

open Microsoft.FSharp.Reflection

open Util
open GeneticAlg
open DerpBrain

type Status = Alive | Dead

/// Represents each of the possible orientations of a Derp
type Orientation = North | South | East | West

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Orientation =
    /// Returns an orientation based on an action taken by a Derp
    let resolveAction (action : Action) (orientation : Orientation) =
        match action, orientation with
        | MoveForward, _ -> orientation
        | TurnLeft,    North -> West
        | TurnLeft,    South -> East
        | TurnLeft,    East  -> North
        | TurnLeft,    West  -> South
        | TurnRight,   North -> East
        | TurnRight,   South -> West
        | TurnRight,   East  -> South
        | TurnRight,   West  -> North
        | TurnBack,    North -> South
        | TurnBack,    South -> North
        | TurnBack,    East  -> West
        | TurnBack,    West  -> East
        
    /// All the possible orientations
    let cases = 
        FSharpType.GetUnionCases typedefof<Orientation> 
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]))

    let count = Array.length cases

    /// Returns a random orientation
    let randomCase () = cases.[rand.Next count] :?> Orientation

/// Stores information about a Derp for the purpose of computing its fitness 
/// at the end of a generation
type Tracker () =
    let mutable plantsEaten = 0
    let mutable age = 0

    /// The number of plants a Derp has eaten during a generation
    member this.PlantsEaten with get () = plantsEaten

    /// The age of a derp at the time of its death
    member this.Age with get () = age

    /// Incremenets the age counter by one
    member this.SuccAge () = age <- age + 1

    /// Increments the plant counter by one
    member this.SuccPlants () = plantsEaten <- plantsEaten + 1

    member this.Fitness = float age


/// Represents a Derp, a creature that walks around and tries to consume food
type Derp (brain : DerpBrain, orientation : Orientation) =
    let mutable status = Alive
    let mutable orientation = orientation
    let mutable state = 0
    let mutable energy = 10.0

    /// The basal cost of a single step
    /// This is the energy consumed in a single step if the derp remains stationary.
    let baseCost = -0.10

    /// The factor by which the basal energy cost is multiplied
    /// when a derp moves to a new cell in the world
    let movementCost =
        let moveFactor = 1.5
        (baseCost * moveFactor)

    let tracker = Tracker ()

    new (brain : DerpBrain) = new Derp (brain, Orientation.randomCase ())

    member this.Brain = brain

    member this.Status with get () = status

    member this.Orientation
        with get () = orientation
        and set value = orientation <- value

    member this.State with get () = state

    member this.Energy with get () = energy

    member this.Actionsome = this.Brain.ActionMatrix |> Array2D.flatten
    member this.Statesome = this.Brain.StateMatrix |> Array2D.flatten

    member this.Tracker = tracker

    member this.Eat () =
        let foodEnergy = 1.0
        energy <- energy + foodEnergy

    member this.Die () = status <- Dead

    member this.Update (sight : Sight) =
        let action, nextState = this.Brain.Sample state sight
        state <- nextState
        orientation <- Orientation.resolveAction action orientation
        this.Tracker.SuccAge ()
        if action = MoveForward then energy <- energy + movementCost
        else energy <- energy + baseCost
        action

    static member IsAlive (derp : Derp) = derp.Status = Alive

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

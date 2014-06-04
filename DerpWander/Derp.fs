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
    let resolveAction action orientation =
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

    member this.Fitness = age

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

    /// The brain of this Derp
    member this.Brain = brain

    /// The current alive-dead status of this Derp
    member this.Status with get () = status

    /// The current orientation of this Derp
    member this.Orientation
        with get () = orientation
        and set value = orientation <- value

    /// The current state of this Derp
    member this.State with get () = state

    /// The total amount of energy that this Derp has available
    member this.Energy with get () = energy

    /// The flattened version of this Derp's action matrix
    member this.ActionGene = Array2D.flatten this.Brain.ActionMatrix

    /// The flattened version of this Derp's state matrix
    member this.StateGene = Array2D.flatten this.Brain.StateMatrix

    /// The tracker for this Derp
    member this.Tracker = tracker

    /// Eats a plant, which gives the Derp more energy
    member this.Eat () =
        let foodEnergy = 1.0
        energy <- energy + foodEnergy

    /// Sets this Derp's alive-dead status to dead
    member this.Die () = status <- Dead

    /// Updates this derp based on what it sees (and what state it's in)
    member this.Update (sight : Sight) =
        let action, nextState = this.Brain.Sample state sight
        state <- nextState
        orientation <- Orientation.resolveAction action orientation
        this.Tracker.SuccAge ()
        if action = MoveForward then energy <- energy + movementCost
        else energy <- energy + baseCost
        action

    /// Checks if a given Derp is alive
    static member IsAlive (derp : Derp) = derp.Status = Alive

    /// Mutates a given Derp geneome
    static member Mutator (mutationThreshold  : double) : Genome.T -> Genome.T =
        let mutateVal () =
            if rand.NextDouble () < mutationThreshold then
                rand.NextDouble ()
            else 0.0

        let mutate : Gene -> Gene = 
            Array.map (fun codon -> codon + mutateVal ())
            << Array.normalize

        (fun genome ->
            let actionGene = genome.ActionGene |> mutate
            let stateGene = genome.StateGene |> mutate
            Genome.create (actionGene, stateGene))
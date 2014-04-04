module World

open System
open System.Collections.Generic
open System.Threading.Tasks

open Microsoft.FSharp.Reflection

open Util
open DerpBrain
open WorldOptions
open Derp

/// Represents a cell in the world.
type Cell = Empty | Food | Derp of Derp

type World (optionSet : OptionSet, derpBrains : DerpBrain list, generation : int) =
    let size = optionSet.WorldSize
    let width, height = size
    let derpCount = optionSet.DerpCount
    let wrap = wrap size
    let derps = 
        optionSet.DerpRespawnOp derpBrains optionSet.WorldSize 
            |> Seq.map (fun brainPosPair -> (brainPosPair.Pos, Derp.Derp brainPosPair.Brain))
            |> Seq.toArray

    let map =
        let temp = Array2D.create width height Cell.Empty
        let grow = optionSet.PlantGrowthFunc
        let write value p = 
            let x, y = wrap p
            temp.[x, y] <- value
        let placeFood = write Cell.Food
        let placeDerp derp = write (Cell.Derp derp)

        grow size placeFood
        for (p, derp) in derps do placeDerp derp p
        temp

    /// Respawns a plant using the chosen respawn function.
    let respawnPlant p =
        let spawn = optionSet.PlantRespawnFunc
        let write cell (x, y) = map.[x, y] <- cell
        let writeFood = write Cell.Food
        let isEligible p =
            let x, y = wrap p
            match map.[x, y] with
            | Cell.Empty -> true
            | _ -> false
        spawn isEligible writeFood size p

    /// Gets the coordinate that is in front of a given Derp.
    /// This represents what the Derp can see.
    let coordSeen orientation pos =
        match orientation with
        | North -> tupleAdd pos (0, -1)
        | South -> tupleAdd pos (0,  1)
        | East  -> tupleAdd pos (1,  0)
        | West  -> tupleAdd pos (-1, 0)
        
    /// Resolves a world Cell to a Sight recognizable by a DerpBrain.
    let matchSight pos =
        let x, y = pos
        match map.[x, y] with
        | Derp _ -> Sight.Derp
        | Food   -> Sight.Food
        | Empty  -> Sight.Empty

    new (optionSet) = new World (optionSet, 
                                 [for i = 0 to optionSet.DerpCount - 1 do 
                                    yield DerpBrain (optionSet.StateCount)], 
                                 0)

    member this.Derps =
        derps
        |> Seq.map snd
        |> Seq.toList

    /// Updates the world a single generation.
    member this.Update () =
        let moveDerp (x, y) (nX, nY) (derp : Derp) =
            map.[x, y] <- Cell.Empty
            match map.[nX, nY] with
            | Cell.Food -> 
                derp.Tracker.SuccPlants ()
                derp.Eat ()
                if rand.NextDouble () < optionSet.PlantRespawnThreshold
                    then respawnPlant (nX, nY)
            | _ -> ()
            map.[nX, nY] <- Cell.Derp derp
            ()

        let canMoveTo (nX, nY) =
            match map.[nX, nY] with
            | Cell.Derp _ -> false
            | _ -> true

        for i = 0 to derps.Length - 1 do
            let pos, derp = derps.[i]
            if derp.IsAlive () then
                if derp.Energy <= 0.0 then derp.Die ()
                else
                    let foreCoord = wrap <| coordSeen derp.Orientation pos
                    let sight = matchSight foreCoord
                    let action = derp.Update sight
                    if action = MoveForward then
                        let nPos = foreCoord
                        if canMoveTo nPos then
                            moveDerp pos nPos derp
                            derp.Tracker.AddCell nPos
                            derps.[i] <- (nPos, derp)
                    else moveDerp pos pos derp
            
    /// The OptionSet for this world.
    member this.Options = optionSet

    /// The horizontal and vertical dimensions of the world.
    member this.Size = size

    /// The horizontal width of the world.
    member this.Width = width

    /// The vertical height of the world.
    member this.Height = height

    /// The number of Derps in this world.
    member this.DerpCount = derpCount

    /// The Cell grid of this world.
    member this.Map = map

    member this.Generation = generation

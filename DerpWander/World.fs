module World

open System
open System.Collections.Generic
open System.Threading.Tasks

open Microsoft.FSharp.Reflection

open Util
open DerpBrain
open WorldOptions
open Derp

type Generation = int

/// Represents a cell in the world.
type Cell =
    | Derp of Derp
    | Food
    | Empty

type World (optionSet : OptionSet, derpBrains : DerpBrain list, generation : Generation) =
    let size = optionSet.WorldSize
    let width, height = size
    let derpCount = optionSet.DerpCount
    let derps = 
        optionSet.DerpRespawnOp derpBrains optionSet.WorldSize 
            |> Seq.map (fun brainPosPair -> (brainPosPair.Pos, Derp.Derp brainPosPair.Brain))
            |> Seq.toArray

    let map =
        let temp = Array2D.create width height Cell.Empty
        let grow = optionSet.PlantGrowthFunc
        let place cell (x, y) = temp.[x %% width, y %% height] <- cell
        let placeFood = place Cell.Food
        let placeDerp derp = place (Cell.Derp derp)

        grow size placeFood
        for ((x, y), derp) in derps do placeDerp derp (x, y)
        temp

    /// Gets the coordinate that is in front of a given Derp.
    /// This represents what the Derp can see.
    let coordSeen orientation pos =
        match orientation with
        | North -> tupleAdd pos (0,  1)
        | South -> tupleAdd pos (0, -1)
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
        |> Seq.map (fun p ->
            let _, derp = p
            derp)
        |> Seq.toList

    member this.Update () =
        let moveDerp (x, y) (nX, nY) (derp : Derp) =
            map.[x, y] <- Cell.Empty
            match map.[nX, nY] with
            | Cell.Food -> derp.Tracker.SuccPlants ()
            | _ -> ()
            map.[nX, nY] <- Cell.Derp derp
            ()

        let canMoveTo (nX, nY) =
            match map.[nX, nY] with
            | Cell.Empty -> true
            | Cell.Food -> true
            | Cell.Derp _ -> false

        for i = 0 to derps.Length - 1 do
            let pos, derp = derps.[i]
            let foreCoord = 
                let x, y = coordSeen derp.Orientation pos
                (x %% width, y %% height)
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

module World

open System
open System.Collections.Generic

open Microsoft.FSharp.Reflection

open Util
open DerpBrain
open WorldOptions
open Derp

/// Represents a cell in the world.
type Cell =
    | Derp of Derp
    | Food
    | Empty

type World (optionSet : OptionSet, derpBrains : DerpBrain list) =
    let size = optionSet.WorldSize
    let derpCount = optionSet.DerpCount

    let map =
        let temp = Array2D.create size size Empty
        for p in (optionSet.PlantGrowthFunc size) do
            let x, y = p
            temp.[x, y] <- Cell.Food
        for brainPosPair in (optionSet.DerpRespawnOp derpBrains optionSet.WorldSize) do
            let x, y = brainPosPair.pos
            temp.[x, y] <- (Cell.Derp (new Derp (brainPosPair.brain)))
        temp

    /// Gets all the derps in the world, along with their locations.
    let getDerps () =
        [for x in 0 .. (size - 1) do
            for y in 0 .. (size - 1) do
                match map.[x, y] with
                | Derp derp -> yield (derp, (x, y))
                | _ -> ()]

    new (optionSet) = new World (optionSet, [for i = 0 to optionSet.DerpCount - 1 do yield DerpBrain (optionSet.StateCount)])

    member this.Update () =
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
            if not (isInBounds x y size size) then Sight.Wall
            else
                match map.[x, y] with
                | Derp _ -> Sight.Derp
                | Food   -> Sight.Food
                | Empty  -> Sight.Empty

        for derp, pos in getDerps () do
            let frontCoord = coordSeen derp.Orientation pos
            let sight = matchSight frontCoord
            let actionOp = derp.Update sight
            match actionOp with
            | Some action ->
                match action with
                | MoveForward  ->
                    if sight = Sight.Wall || sight = Sight.Derp then ()
                    else
                        let x, y = pos
                        let newX, newY = frontCoord
                        map.[x, y] <- Empty
                        if map.[newX, newY] = Cell.Food then derp.AddPlant ()
                        map.[newX, newY] <- Cell.Derp derp
                | MoveBackward ->
                    let backCoord = coordSeen (Orientation.Invert derp.Orientation) pos
                    let invertSight = matchSight backCoord
                    if invertSight = Sight.Wall || invertSight = Sight.Derp then ()
                    else
                        let x, y = pos
                        let newX, newY = backCoord
                        map.[x, y] <- Empty
                        if map.[newX, newY] = Cell.Food then derp.AddPlant ()
                        map.[newX, newY] <- Cell.Derp derp
            | None -> ()

             
    /// The OptionSet for this world.
    member this.Options = optionSet

    /// The square size of this world.
    member this.Size = size

    /// The number of Derps in this world.
    member this.DerpCount = derpCount

    /// The Cell grid of this world.
    member this.Map = map

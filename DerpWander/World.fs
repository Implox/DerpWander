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
    let derpCount = optionSet.DerpCount
    let derps = 
        optionSet.DerpRespawnOp derpBrains optionSet.WorldSize 
            |> Seq.map (fun brainPosPair -> (brainPosPair.Pos, Derp.Derp brainPosPair.Brain))
            |> Seq.toArray

    let map =            
        let temp = Array2D.create size size Cell.Empty
        let func = optionSet.PlantGrowthFunc
        func size (fun (x : int, y : int) -> temp.[x, y] <- Cell.Food)
        for ((x, y), derp) in derps do temp.[x, y] <- Cell.Derp derp
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
        if not (isInBounds x y size size) then Sight.Wall
        else
            match map.[x, y] with
            | Derp _ -> Sight.Derp
            | Food   -> Sight.Food
            | Empty  -> Sight.Empty

    new (optionSet) = new World (optionSet, [for i = 0 to optionSet.DerpCount - 1 do yield DerpBrain (optionSet.StateCount)], 0)

    member this.Derps =
        derps
        |> Seq.map (fun p ->
            let _, derp = p
            derp)
        |> Seq.toList

    member this.Update () =
        let inline moveDerp (x, y) (nX, nY) (derp : Derp) =
            map.[x, y] <- Cell.Empty
            match map.[nX, nY] with
            | Cell.Food -> derp.AddPlant ()
            | _ -> ()
            map.[nX, nY] <- Cell.Derp derp
            ()

        let canMove (nX, nY) =
            if not (isInBounds nX nY size size) then false
            else
                match map.[nX, nY] with
                | Cell.Empty -> true
                | Cell.Food -> true
                | Cell.Derp _ -> false

        for i = 0 to derps.Length - 1 do
            let pos, derp = derps.[i]
            let mutable pos = pos
            let foreCoord = coordSeen derp.Orientation pos
            let backCoord = coordSeen (Orientation.invert derp.Orientation) pos
            let sight = matchSight foreCoord
            let actionOp = derp.Update sight
            let nPos =
                match actionOp with
                | Some MoveForward -> foreCoord
                | Some MoveBackward -> backCoord
                | _ -> pos
            if pos <> nPos && canMove nPos then 
                moveDerp pos nPos derp
                derps.[i] <- (nPos, derp)
                         
    /// The OptionSet for this world.
    member this.Options = optionSet

    /// The square size of this world.
    member this.Size = size

    /// The number of Derps in this world.
    member this.DerpCount = derpCount

    /// The Cell grid of this world.
    member this.Map = map

    member this.Generation = generation

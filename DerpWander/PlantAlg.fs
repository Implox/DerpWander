module PlantAlg

open System
open System.Collections.Generic

open Util

//The following functions generate lists of coordinates in the world where food will grow, based on a specified growth pattern.

/// Generates a "clumped" growth pattern.
let clump worldSize =
    let plants = List<(int * int)> ()

    let phi samplePoint basePoint =
        let x, y = samplePoint
        let bx, by = basePoint
        let dx = float (bx - x)
        let dy = float (by - y)
        let r = 1.5
        let factor = 1.0
        let arg = sqrt (dx * dx + dy * dy) / r
        if arg > Math.PI then false
        else
            let threshold = (cos (arg) + 1.0) / 2.0
            rand.NextDouble () < (threshold * factor)

    let seeds =
        let threshold = 0.9
        let div = 3
        [for i in [1 .. (worldSize / div) - 1] do
            for j in [1 .. (worldSize / div) - 1] do
                let x = i * (worldSize / div)
                let y = j * (worldSize / div)
                if rand.NextDouble () < threshold then
                    yield (x + rand.Next (-(worldSize / (div * 2)), worldSize / (div * 2)),
                           y + rand.Next (-(worldSize / (div * 2)), worldSize / (div * 2)))]

    for seed in seeds do
        for i = 0 to (worldSize - 1) do
            for j = 0 to (worldSize - 1) do
                if phi (i, j) seed && not (plants.Contains (i, j)) then plants.Add (i, j)
    plants |> List.ofSeq

/// Generates a random growth pattern.
let random worldSize =
    let plantCount = int (float (worldSize * worldSize) * 0.45)
    [for i = 0 to (plantCount - 1) do yield (rand.Next worldSize, rand.Next worldSize)]

/// Generates a growth pattern where the plants group toward the bottom.
let nearBottom worldSize =
    let factor = 1.0
    
    let phi (samplePoint : int * int) =
        let x, _ = samplePoint
        let dx = abs (float ((worldSize - (worldSize / 8)) - x))
        rand.NextDouble () < (factor * (1.0 / dx))

    [for i = 0 to worldSize - 1 do
        for j = 0 to worldSize - 1 do
            if phi (i, j) then yield (i, j)]

// The following functions handle plant respawn behavior

let neverRespawn  = None 

let anywhereRespawn worldSize = Some (rand.Next worldSize, rand.Next worldSize)
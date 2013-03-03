/// Contains functions relevant to the plant-related WorldOptions (eg. spawning/respawning of plant)
module PlantOptionAlg

open System
open System.Collections.Generic

open Util

module PlantGrowth = 

    /// Generates a "clumped" growth pattern.
    let clump worldSize write =
        let seeds =
            let threshold = 0.95
            let div = 6
            [for i in [0 .. (worldSize / div) - 1] do
                for j in [0 .. (worldSize / div) - 1] do
                    let x = i * (worldSize / div)
                    let y = j * (worldSize / div)
                    if rand.NextDouble () < threshold then
                        yield (x + rand.Next (-(worldSize / (div * 2)), worldSize / (div * 2)),
                               y + rand.Next (-(worldSize / (div * 2)), worldSize / (div * 2)))]

        let solidRadius = 0.5
        let falloffRadius = 5.0
        let phi samplePoint basePoint =
            let x, y = samplePoint
            let bx, by = basePoint
            let dx = float (bx - x)
            let dy = float (by - y)
            let d = sqrt (dx * dx + dy * dy)
            if d <= solidRadius then true
            elif d <= falloffRadius then rand.NextDouble () > (d - solidRadius) / (falloffRadius - solidRadius)
            else false
        
        let iRadius = int falloffRadius
        for seed in seeds do
            let bx, by = seed
            for x = max 0 (bx - iRadius) to min (worldSize - 1) (bx + iRadius) do
                for y = max 0 (by - iRadius) to min (worldSize - 1) (by + iRadius) do
                    if phi (x, y) seed then write (x, y)

    /// Generates a random growth pattern.
    let random worldSize write =
        let plantCount = int (float (worldSize * worldSize) * 0.25)
        for i = 0 to (plantCount - 1) do write (rand.Next worldSize, rand.Next worldSize)

    /// Generates a growth pattern where the plants group toward the bottom.
    let nearBottom worldSize write =
        let factor = 1.0
    
        let phi (samplePoint : int * int) =
            let x, _ = samplePoint
            let dx = abs (float ((worldSize - (worldSize / 8)) - x))
            rand.NextDouble () < (factor * (1.0 / dx))

        for i = 0 to worldSize - 1 do
            for j = 0 to worldSize - 1 do
                if phi (i, j) then write (i, j)


module PlantRespawn =
    let neverRespawn  = None 

    let anywhereRespawn worldSize = Some (rand.Next worldSize, rand.Next worldSize)

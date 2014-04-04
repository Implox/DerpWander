/// Contains functions relevant to the plant-related WorldOptions (eg. spawning/respawning of plant)
module PlantOptionAlg

open System
open System.Collections.Generic

open Util

module PlantGrowth = 
    /// Generates a "clumped" growth pattern
    let clumped worldSize write =
        let width, height = worldSize
        let seeds =
            let threshold = 0.40
            let div = 6
            [for i in [0 .. (width / div) - 1] do
                for j in [0 .. (height / div) - 1] do
                    let x = i * (width / div)
                    let y = j * (height / div)
                    if rand.NextDouble () < threshold then
                        yield (x + rand.Next (-(width / (div * 2)), width / (div * 2)),
                               y + rand.Next (-(height / (div * 2)), height / (div * 2)))]
        let solidRadius = 0.5
        let falloffRadius = 5.0

        let phi samplePoint basePoint =
            let x, y = samplePoint
            let bx, by = basePoint
            let dx = float (bx - x)
            let dy = float (by - y)
            let d = sqrt (dx * dx + dy * dy)
            if d <= solidRadius then true
            elif d <= falloffRadius then 
                rand.NextDouble () > (d - solidRadius) / (falloffRadius - solidRadius)
            else false
        
        let iRadius = int falloffRadius
        for seed in seeds do
            let bx, by = seed
            for x = (bx - iRadius) to (bx + iRadius) do
                for y = (by - iRadius) to (by + iRadius) do
                    if phi (x, y) seed then write (x, y)

    /// Generates a random growth pattern
    let random worldSize write =
        let width, height = worldSize
        let plantCount = int (float (width * height) * 0.40)
        for i = 0 to (plantCount - 1) do write (rand.Next width, rand.Next height)

    /// Generates a growth pattern where the plants group toward the bottom
    let nearBottom worldSize write =
        let width, height = worldSize
        let factor = 1.0
        let phi (samplePoint : Point2) =
            let _, y = samplePoint
            let dy = abs (float ((height - (height / 8)) - y))
            rand.NextDouble () < (factor * (1.0 / dy))
        for x = 0 to width - 1 do
            for y = 0 to height - 1 do
                if phi (x, y) then write (x, y)

module PlantRespawn =
    /// Spawns a new plant in the general vicinity of where a given plant was eaten
    let nearby isEligible write worldSize plantPos =
        let rec makePointNear plantPos =
            let div = 8
            let width, height = worldSize
            let avg = (width + height) / 2
            let r = avg / div
            let candidate = tupleAdd plantPos (rand.Next (-r + 1, r), rand.Next (-r + 1, r))
            if candidate |> isEligible then candidate
            else makePointNear plantPos
        write <| makePointNear plantPos

    /// Spawns a new plant anywhere in the world
    let anywhere isEligible write worldSize _ =
        let width, height = worldSize
        let rec makePoint () =
            let candidate = (rand.Next width, rand.Next height)
            if candidate |> isEligible then candidate
            else makePoint ()
        write <| makePoint ()

    /// Spawns no new plants.
    let never _ _ _ _ = ()
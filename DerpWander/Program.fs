module Program

open System
open System.Windows.Forms

open Util
open GeneticAlg
open DerpBrain
open WorldOptions
open World
open Window

[<STAThread; EntryPoint>]
let main args =
    let options = new OptionSet (64, 
                                 20, 
                                 1,
                                 GrowthPatternOption.Clumps, 
                                 PlantRespawnOption.Never, 
                                 DerpRespawnOption.Random, 
                                 GenSpeed.FastestNoDisp,
                                 0.01)

    let world = new World (options)
    let timeChunk = 50
    let averages = Array.create timeChunk 0.0


    Application.EnableVisualStyles ()
    let window = new GraphicsWindow (world)
    window.Show ()

    /// Updates a population for the next generation.
    let nextGeneration () = 
        let derps = window.World.Derps
        
        if (window.World.Options.Speed = GenSpeed.FastestNoDisp && window.World.Generation % timeChunk = 0) || (window.World.Options.Speed <> GenSpeed.FastestNoDisp) then 
            printfn "GenStep %i" (window.World.Generation)
            printfn "Best: %i" (derps |> List.maxBy (fun derp -> derp.PlantsEaten)).PlantsEaten
            printfn "Avg: %.2f"  (derps |> List.averageBy (fun derp -> float derp.PlantsEaten))
            printfn "%i-Generation Average: %.2f\n" timeChunk (Array.average averages)

        averages.[window.World.Generation % timeChunk] <- derps |> List.averageBy (fun derp -> float derp.PlantsEaten)

        let population : Population =
            [for derp in derps do
                yield { Actionsome = derp.Actionsome; Statesome = derp.Statesome; Fitness = derp.PlantsEaten }]
        let nextStep = evolveStep population Derp.Derp.Mutator options.Threshold
        nextStep |> List.map (fun dna -> DerpBrain (options.StateCount, dna))

    /// Simulates the world for a single generation.
    let rec simGeneration (last : DateTime) (days : int) (maxDays : int) (world : World) =
        if window.Visible then
            let current = DateTime.Now
            let dTime = int (current - last).TotalMilliseconds
            if dTime >= int world.Options.Speed then
                if days = maxDays then
                    window.World <- new World (options, nextGeneration (), world.Generation + 1)
                    simGeneration current 1 maxDays window.World
                else
                    Application.DoEvents ()
                    window.Update ()
                    simGeneration current (days + 1) maxDays world
            else
                Application.DoEvents ()
                simGeneration last days maxDays world
        else ()

    simGeneration DateTime.Now 1 300 window.World
    0

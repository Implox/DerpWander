module Program

open System
open System.Windows.Forms

open Util
open GeneticAlg
open DerpBrain
open WorldOptions
open World
open Window

[<EntryPoint>]
let main args =
    Console.BufferHeight <- int Int16.MaxValue-1
    let options = new OptionSet ((128, 64), 
                                 25,
                                 3,
                                 GrowthPatternOption.Clumped, 
                                 PlantRespawnOption.Nearby, 
                                 DerpRespawnOption.Random, 
                                 GenSpeed.Fastest,
                                 0.05,
                                 0.10)

    let world = new World (options)
    let timeChunk = 250
    let averages = Array.create timeChunk 0.0


    Application.EnableVisualStyles ()
    let window = new GraphicsWindow (world)
    printfn "Number of derps: %i\nStates per Derp brain: %i\nSkipSize: %i generations\nRight click on the window to modify options...\n" 
            options.DerpCount 
            options.StateCount 
            timeChunk

    window.Show ()

    /// Updates a population for the next generation.
    let nextGeneration (genLength : int) = 
        let world = window.World
        let derps = world.Derps
       
        if (world.Options.Speed = GenSpeed.FastestNoDisp && world.Generation % timeChunk = 0) || (world.Options.Speed <> GenSpeed.FastestNoDisp) then 
            printfn "Generation %i" (world.Generation)
            printfn "Best: %i" (derps |> List.maxBy (fun derp -> derp.Tracker.PlantsEaten)).Tracker.PlantsEaten
            printfn "Avg: %.2f"  (derps |> List.averageBy (fun derp -> float derp.Tracker.PlantsEaten))
            printfn "%i-Generation Average: %.2f\n" timeChunk (if world.Generation >= timeChunk then (Array.average averages) else Double.NaN)

        averages.[world.Generation % timeChunk] <- derps |> List.averageBy (fun derp -> float derp.Tracker.PlantsEaten)

        let population : Population =
            [for derp in derps do
                yield { Actionsome = derp.Actionsome; Statesome = derp.Statesome; Fitness = derp.Tracker.GetFitness ()}]
        let nextStep = evolveStep population Derp.Derp.Mutator options.MutationThreshold
        nextStep |> List.map (fun dna -> DerpBrain (options.StateCount, dna))

    /// Simulates the world for a single generation.
    let rec simGeneration (last : DateTime) (day : int) (genLength : int) (world : World) =
        if window.Visible then
            let current = DateTime.Now
            let dTime = int (current - last).TotalMilliseconds
            if dTime >= int world.Options.Speed then
                if day = genLength then
                    window.World <- new World (options, nextGeneration (genLength), world.Generation + 1)
                    simGeneration current 1 genLength window.World
                else
                    Application.DoEvents ()
                    window.Update ()
                    simGeneration current (day + 1) genLength world
            else
                Application.DoEvents ()
                simGeneration last day genLength world
        else ()
    simGeneration DateTime.Now 1 300 window.World
    0

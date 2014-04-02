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
    Console.BufferHeight <- int Int16.MaxValue-1
    let options = new OptionSet ((128, 64), 
                                 15,
                                 2,
                                 GrowthPatternOption.Clumped, 
                                 PlantRespawnOption.Never, 
                                 DerpRespawnOption.Random, 
                                 GenSpeed.Fastest,
                                 0.05)

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
        let derps = window.World.Derps
        
        if (window.World.Options.Speed = GenSpeed.FastestNoDisp && window.World.Generation % timeChunk = 0) || (window.World.Options.Speed <> GenSpeed.FastestNoDisp) then 
            printfn "Generation %i" (window.World.Generation)
            printfn "Best: %i" (derps |> List.maxBy (fun derp -> derp.Tracker.PlantsEaten)).Tracker.PlantsEaten
            printfn "Avg: %.2f"  (derps |> List.averageBy (fun derp -> float derp.Tracker.PlantsEaten))
            printfn "%i-Generation Average: %.2f\n" timeChunk (if window.World.Generation >= timeChunk then (Array.average averages) else Double.NaN)

        averages.[window.World.Generation % timeChunk] <- derps |> List.averageBy (fun derp -> float derp.Tracker.PlantsEaten)

        let population : Population =
            [for derp in derps do
                yield { Actionsome = derp.Actionsome; Statesome = derp.Statesome; Fitness = derp.Tracker.GetFitness ()}]
        let nextStep = evolveStep population Derp.Derp.Mutator options.Threshold
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

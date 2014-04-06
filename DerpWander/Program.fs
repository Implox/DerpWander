module Program

open System
open System.Windows.Forms

open Util
open GeneticAlg
open DerpBrain
open Derp
open WorldOptions
open World
open Window

[<EntryPoint>]
let main args =
    Console.BufferHeight <- int Int16.MaxValue-1
    let options = new OptionSet ((128, 96), 
                                 25,
                                 3,
                                 "Clumped", 
                                 "Anywhere", 
                                 "Random", 
                                 GenSpeed.Fastest,
                                 0.05,
                                 0.50)

    let world = new World (options)
    let timeChunk = 250
    let longAverage = Array.create timeChunk 0.0
    let bestAverage = Array.create timeChunk 0.0


    Application.EnableVisualStyles ()
    let window = new GraphicsWindow (world)
    printfn "Number of derps: %i\nStates per Derp brain: %i\nTimeChunk: %i generations\nRight click on the window to modify options...\n" 
            options.DerpCount 
            options.StateCount 
            timeChunk

    window.Show ()

    /// Updates a population for the next generation.
    let nextGeneration () = 
        let world = window.World
        let derps = world.Derps
        let best = (derps |> List.maxBy (fun derp -> derp.Tracker.Fitness)).Tracker.Fitness
        let avg = (derps |> List.averageBy (fun derp -> float derp.Tracker.Age))
        bestAverage.[world.Generation % timeChunk] <- best
        longAverage.[world.Generation % timeChunk] <- avg

        if (world.Options.Speed = GenSpeed.FastestNoDisp && world.Generation % timeChunk = 0) || (world.Options.Speed <> GenSpeed.FastestNoDisp) then
            printfn "Generation %i" (world.Generation)
            printfn "Best (days): %i" <| int best
            printfn "Avg (days): %.2f" avg
            printfn "%i-Generation Average (days): %.2f" timeChunk (if world.Generation >= timeChunk then (Array.average longAverage) else Double.NaN)
            printfn "%i-Generation Best Average (days): %.2f" timeChunk (if world.Generation >= timeChunk then (Array.average bestAverage) else Double.NaN)
            printfn ""

        let population : Population =
            derps |> List.map (fun derp -> dna derp.Actionsome derp.Statesome derp.Tracker.Fitness)
        let nextStep = evolveStep population Derp.Derp.Mutator options.MutationThreshold
        nextStep |> List.map (fun dna -> DerpBrain (options.StateCount, dna))

    /// Simulates the world for a single generation.
    let rec simGeneration (last : DateTime) (day : int) (world : World) =
        if window.Visible then
            let current = DateTime.Now
            let dTime = int (current - last).TotalMilliseconds
            if dTime >= int world.Options.Speed then
                if world.Derps |> List.forall (not << Derp.IsAlive) then
                    window.World <- new World (options, nextGeneration (), world.Generation + 1)
                    simGeneration current 1 window.World
                else
                    Application.DoEvents ()
                    window.Update ()
                    simGeneration current (day + 1) world
            else
                Application.DoEvents ()
                simGeneration last day world
        else ()

    simGeneration DateTime.Now 1 window.World
    0

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

/// Updates a population for the next generation.
let nextGeneration (timeChunk : int) writeLong writeBest avgLong avgBest (world : World) = 
    let options = world.Options
    let derps = world.Derps
    let best = (derps |> List.maxBy (fun derp -> derp.Tracker.Fitness)).Tracker.Fitness
    let avg = (derps |> List.averageBy (fun derp -> float derp.Tracker.Age))
    writeBest (world.Generation % timeChunk) best
    writeLong (world.Generation % timeChunk) avg

    if (options.Speed = GenSpeed.FastestNoDisp && world.Generation % timeChunk = 0) || (options.Speed <> GenSpeed.FastestNoDisp) then
        printfn "Generation %i" (world.Generation)
        printfn "Best (days): %i" <| int best
        printfn "Avg (days): %.2f" avg
        printfn "%i-Generation Average (days): %.2f" timeChunk (if world.Generation >= timeChunk then avgLong () else Double.NaN)
        printfn "%i-Generation Best Average (days): %.2f\n" timeChunk (if world.Generation >= timeChunk then avgBest () else Double.NaN)

    let population =
        derps |> List.map (fun derp -> dna derp.Actionsome derp.Statesome derp.Tracker.Fitness)
    let nextStep = evolveStep population Derp.Mutator options.MutationThreshold
    nextStep |> List.map (fun dna -> DerpBrain (options.StateCount, dna))

/// Simulates the world for a single generation.
let simGeneration (timeChunk : int) longAvg bestAvg (window : GraphicsWindow) =
    let write (arr : _ array) index value = arr.[index] <- value
    let writeLong = write longAvg
    let writeBest = write bestAvg

    let avgLong () = Array.average longAvg
    let avgBest () = Array.average bestAvg
    let nextGen = nextGeneration timeChunk writeLong writeBest avgLong avgBest

    let rec iter (last : DateTime) (day : int) (window : GraphicsWindow) =
        let world = window.World
        let options = world.Options
        if window.Visible then
            let current = DateTime.Now
            let dTime = int (current - last).TotalMilliseconds
            if dTime >= int options.Speed then
                if world.Derps |> List.forall (not << Derp.IsAlive) then
                    window.World <- new World (options, nextGen world, world.Generation + 1)
                    iter current 1 window
                else
                    Application.DoEvents ()
                    window.Update ()
                    iter current (day + 1) window
            else
                Application.DoEvents ()
                iter last day window
        else ()
    iter DateTime.Now 1 window

[<EntryPoint>]
let main args =
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
    let window = new GraphicsWindow (world)
    let timeChunk = 250

    let longAverage = Array.create timeChunk 0.0
    let bestAverage = Array.create timeChunk 0.0

    Console.BufferHeight <- int Int16.MaxValue - 1
    Application.EnableVisualStyles ()

    printfn "Number of derps: %i" options.DerpCount
    printfn "States per Derp brain: %i" options.StateCount
    printfn "TimeChunk: %i generations" timeChunk
    printfn "Right click on the window to modify options...\n"

    window.Show ()
    simGeneration timeChunk longAverage bestAverage window
    0

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
let nextGeneration (timeChunk : int) writeLong writeBest avgLong avgBest (window : GraphicsWindow) = 
    let world = window.World
    let options = world.Options
    let derps = world.Derps
    let best = (derps |> List.maxBy (fun derp -> derp.Tracker.Fitness)).Tracker.Fitness
    let avg = (derps |> List.averageBy (fun derp -> float derp.Tracker.Age))
    writeBest (world.Generation % timeChunk) best
    writeLong (world.Generation % timeChunk) avg

    if (world.Options.Speed = GenSpeed.FastestNoDisp && world.Generation % timeChunk = 0) || (world.Options.Speed <> GenSpeed.FastestNoDisp) then
        printfn "Generation %i" (world.Generation)
        printfn "Best (days): %i" <| int best
        printfn "Avg (days): %.2f" avg
        printfn "%i-Generation Average (days): %.2f" timeChunk (if world.Generation >= timeChunk then avgLong () else Double.NaN)
        printfn "%i-Generation Best Average (days): %.2f\n" timeChunk (if world.Generation >= timeChunk then avgBest () else Double.NaN)

    let population =
        derps |> List.map (fun derp -> dna derp.Actionsome derp.Statesome derp.Tracker.Fitness)
    let nextStep = evolveStep population Derp.Derp.Mutator options.MutationThreshold
    nextStep |> List.map (fun dna -> DerpBrain (options.StateCount, dna))

/// Simulates the world for a single generation.
let rec simGeneration (timeChunk : int) (last : DateTime) (day : int) (window : GraphicsWindow) =
    let longAverage = Array.create timeChunk 0.0
    let bestAverage = Array.create timeChunk 0.0

    let write (arr : _ array) index value = arr.[index] <- value
    let writeLong = write longAverage
    let writeBest = write bestAverage

    let avgLong () = Array.average longAverage
    let avgBest () = Array.average bestAverage

    let world = window.World
    let options = world.Options

    let nextGen = nextGeneration timeChunk writeLong writeBest avgLong avgBest
    let simGen = simGeneration timeChunk

    if window.Visible then
        let current = DateTime.Now
        let dTime = int (current - last).TotalMilliseconds
        if dTime >= int options.Speed then
            if world.Derps |> List.forall (not << Derp.IsAlive) then
                window.World <- new World (options, nextGen window, world.Generation + 1)
                simGeneration timeChunk current 1 window
            else
                Application.DoEvents ()
                window.Update ()
                simGen current (day + 1) window
        else
            Application.DoEvents ()
            simGen last day window
    else ()

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

    Console.BufferHeight <- int Int16.MaxValue - 1
    Application.EnableVisualStyles ()
    printfn "Number of derps: %i\nStates per Derp brain: %i\nTimeChunk: %i generations\nRight click on the window to modify options...\n" 
            options.DerpCount
            options.StateCount
            timeChunk
    window.Show ()
    simGeneration timeChunk DateTime.Now 1 window
    0

module Program

open System
open System.IO
open System.Windows.Forms
open Util
open GeneticAlg
open DerpBrain
open Derp
open WorldOptions
open World
open GraphicsWindow

/// The StreamWriter that logs information to the specified log file
let mutable logging = StreamWriter.Null

/// The number of generations to skip before printing information to the console.
let timeChunk = 250

/// Stores the average fitness for the last [timeChunk] generations
let longAverage = Array.create timeChunk 0.0

/// Stores the best fitness for the last [timeChunk] generations
let bestAverage = Array.create timeChunk 0.0

/// Prints information of a generation and species history to the console
let printInfo gen avg best longAvg bestAvg =
    printfn "Generation %i" gen
    printfn "Avg (days): %.2f" avg
    printfn "Best (days): %i" best
    printfn "%i-Generation Average (days): %.2f" timeChunk longAvg
    printfn "%i-Generation Best Average (days): %.2f\n" timeChunk bestAvg

/// Logs relevant generation information
let logInfo gen avg best =
    logging.WriteLine ("Generation " + gen.ToString ())
    logging.WriteLine ("Avg (days): " + avg.ToString ())
    logging.WriteLine ("Best (days): " + best.ToString ())

/// Updates a population for the next generation
let nextGeneration (world : World) = 
    let options = world.Options
    let derps = world.Derps

    let evolve = 
        let mutator = Derp.Mutator
        let codonThreshold = options.derpOptions.codonMutationThreshold
        let genomeThreshold = options.derpOptions.genomeMutationThreshold
        let dominanceThreshold = options.derpOptions.dominanceThreshold
        evolve mutator codonThreshold genomeThreshold dominanceThreshold

    /// Stores historical information and prints it
    do
        let speedVal = GeneralOptions.speedValue options.generalOptions
        let gen = world.Generation
        let avg = (derps |> List.averageBy (fun derp -> float derp.Tracker.Age))
        let best = (derps |> List.maxBy (fun derp -> derp.Tracker.Fitness)).Tracker.Fitness
        longAverage.[gen % timeChunk] <- avg
        bestAverage.[gen % timeChunk] <- float best

        let longAvg = if world.Generation >= timeChunk then longAverage |> Array.average else Double.NaN
        let bestAvg = if world.Generation >= timeChunk then bestAverage |> Array.average else Double.NaN

        if logging <> null then logInfo gen avg best
        if (speedVal = 0 && world.Generation % timeChunk = 0) || (speedVal <> 0)
            then printInfo gen avg best longAvg bestAvg

    derps
    |> List.map (fun derp -> ((Genome.create (derp.ActionGene, derp.StateGene)), derp.Tracker.Fitness))
    |> evolve 
    |> List.map (fun genome -> DerpBrain (options.derpOptions.stateCount, genome))

/// Simulates the world for a single generation.
let simGeneration (window : GraphicsWindow) =
    let rec iter (last : DateTime) (day : int) (window : GraphicsWindow) =
        let world = window.World
        let options = world.Options
        let genSpeed = GeneralOptions.speedValue options.generalOptions
        if window.Visible then
            let current = DateTime.Now
            let dTime = int (current - last).TotalMilliseconds
            if dTime >= genSpeed then
                if world.Derps |> List.forall (not << Derp.IsAlive) then
                    let nextGen = nextGeneration world
                    let nextWorld = World (options, nextGen, world.Generation + 1)
                    window.World <- nextWorld
                    iter current 1 window
                else
                    Application.DoEvents ()
                    window.Update ()
                    iter current (day + 1) window
            else
                Application.DoEvents ()
                iter last day window
    iter DateTime.Now 1 window

[<EntryPoint>]
let main args =
    if args.Length < 12 || args.Length > 13 then failwith "Invalid number of arguments"
    let genOps = GeneralOptions.create (Int32.Parse args.[0], Int32.Parse args.[1]) (Int32.Parse args.[2]) args.[3]
    let derpOps = DerpOptions.create (Double.Parse args.[4]) (Double.Parse args.[5]) (Double.Parse args.[6]) (Int32.Parse args.[7]) args.[8]
    let plantOps = PlantOptions.create (Double.Parse args.[9]) args.[10] args.[11]
    let options = CompleteOptionSet.create genOps derpOps plantOps
    if args.Length = 13 then
        logging <- File.CreateText (args.[12])
        logging.WriteLine ("parameters:" + (Array.fold (fun acc x -> acc + " " + x) "" args))

    let world = new World (options)
    let window = new GraphicsWindow (world)

    Console.BufferHeight <- int Int16.MaxValue - 1
    Application.EnableVisualStyles ()

    printfn "Number of derps: %i" options.generalOptions.derpCount
    printfn "States per Derp brain: %i" options.derpOptions.stateCount
    printfn "TimeChunk: %i generations" timeChunk
    printfn "Right click on the window to modify options...\n"

    window.Show ()
    simGeneration window
    0

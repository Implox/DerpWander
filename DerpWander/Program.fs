module Program

open System
open System.Windows.Forms

open Util
open WorldOptions
open World
open Window

[<STAThread; EntryPoint>]
let main args =
    let world = new World (new OptionSet (64,
                                          50,
                                          4,
                                          GrowthPatternOption.Clumps, 
                                          PlantRespawnOption.Never, 
                                          DerpRespawnOption.Random, 
                                          GenSpeedOption.Slowest))
    let updateTime = world.Options.Speed

    Application.EnableVisualStyles ()
    let window = new GraphicsWindow (world)
    window.Show ()

    let rec loop (last : DateTime) =
        if window.Visible then
            let current = DateTime.Now
            let dTime = (floor >> int) (current - last).TotalMilliseconds
            if dTime >= updateTime then
                Application.DoEvents ()
                printf "UPDATE%i\n" (rand.Next ())
                window.Update ()
                loop current
            else
                Application.DoEvents ()
                loop last
        else ()
    loop DateTime.Now
    0

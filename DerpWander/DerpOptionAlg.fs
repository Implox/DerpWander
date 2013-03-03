/// Contains functions relevant to the derp-related WorldOptions (eg. spawning/respawning of derps)
module DerpOptionAlg

open System

open Util
open DerpBrain

/// Represents the pairing of a DerpBrain and a physical location in the world.
type BrainPosPair = { Brain : DerpBrain; Pos : (int * int); }

let random (derpList : DerpBrain list) (size : int) =
    let rec loop derps accum =
        match derps with
        | hd :: tl ->
            let positions = accum |> List.map (fun x -> x.Pos)
            let newCoord = (rand.Next size, rand.Next size)
            if List.exists (fun p -> p = newCoord) positions then loop derps accum
            else loop tl ({ Brain = hd; Pos = newCoord } :: accum)
        | _ -> accum
    loop derpList []

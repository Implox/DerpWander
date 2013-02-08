/// Contains functions relevant to the derp-related WorldOptions (eg. spawning/respawning of derps)
module DerpOptionAlg

open System

open Util
open DerpBrain

/// Represents the pairing of a DerpBrain and a physical location in the world.
/// Necessary because some respawn options require that a Derp be born in the same location as its parent.
type BrainPosPair = {Brain : DerpBrain; Pos : (int * int)}

let random (derpList : DerpBrain list) (size : int) =
    let rec loop derps accum =
        match derps with
        | hd :: tl -> loop tl ({Brain = hd; Pos = (rand.Next size, rand.Next size)} :: accum)
        | _ -> accum
    loop derpList []
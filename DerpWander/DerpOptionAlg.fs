/// Contains functions relevant to the derp-related WorldOptions (eg. spawning/respawning of derps)
module DerpOptionAlg

open System

open Util
open DerpBrain

let random (derpList : DerpBrain list) worldSize =
    let rec loop derps accum =
        match derps with
        | hd :: tl ->
            let width, height = worldSize
            let positions = accum |> List.map fst
            let newCoord = (rand.Next width, rand.Next height)
            if List.exists (fun p -> p = newCoord) positions then loop derps accum
            else loop tl ((newCoord, hd):: accum)
        | _ -> accum
    loop derpList []

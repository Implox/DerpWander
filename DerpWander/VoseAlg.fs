/// Contains implementation of Vose's Alias Method for sampling discrete, non-uniform probability distributions. 
module VoseAlg

open System

open Util

type Alias = { n : int; prob : double []; alias : int [] }
with
    member this.Choose () =
        let p, e = modf (rand.NextDouble this.n - 1.0)
        let j = int e
        if p <= this.prob.[j] then j else this.alias.[j]

/// Takes a float array representation of a probability distribution and creates an Alias from it.
let alias pa =
    let rec split (pa : double []) n j (small, large as part) =
        if j = n then part
        elif pa.[j] > 1.0 then split pa n (j + 1) (small, j :: large)
        else split pa n (j + 1) (j :: small, large)

    let rec init r (pa : double []) part =
        match part with
        | j :: small, k :: large ->
            r.prob.[j] <- pa.[j]
            r.alias.[j] <- k
            pa.[k] <- (pa.[k] + pa.[j] - 1.0)
            if pa.[k] > 1.0 then init r pa (small, k :: large)
            else init r pa (k :: small, large)
         | j :: small, [] ->
            r.prob.[j] <- 1.0
            init r pa (small, [])
        | [], k :: large ->
            r.prob.[k] <- 1.0
            init r pa ([], large)
        | [], [] -> r.Choose
    
    let n = Array.length pa
    if n = 0 then failwith "Invalid arg: \"alias\""
    else 
        let sc = float n / Array.foldBack (fun s p -> if p < 0.0 then failwith "Invalid arg: \"p\"" else s + p) pa 0.0
        let sa = Array.map ((*) sc) pa
        let r  = { n = n; prob = Array.zeroCreate n; alias = Array.create n -1 }
        init r sa (split sa n 0 ([], []))

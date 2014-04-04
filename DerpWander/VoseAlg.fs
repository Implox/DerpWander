/// Contains implementation of Vose's Alias Method for sampling discrete, non-uniform probability distributions
module VoseAlg

open System

open Util

/// Samples a discrete, non-uniform probability distribution in constant time
let sample (n : int, prob : double [], alias : int []) () =
    let p, e = modf (rand.NextDouble n - 1.0)
    let j = int e
    if p <= prob.[j] then j else alias.[j]

/// Takes a float array representation of a probability distribution and
/// creates a fast sampling function of the distrubution using Vose's Alias method
let alias pa =
    let rec split (pa : double []) n j (small, large as part) =
        if j = n then part
        elif pa.[j] > 1.0 then split pa n (j + 1) (small, j :: large)
        else split pa n (j + 1) (j :: small, large)

    let rec init (n : int, prob : double [], alias : int []) (pa : double []) part =
        match part with
        | j :: small, k :: large ->
            prob.[j] <- pa.[j]
            alias.[j] <- k
            pa.[k] <- (pa.[k] + pa.[j] - 1.0)
            if pa.[k] > 1.0 then init (n, prob, alias) pa (small, k :: large)
            else init (n, prob, alias) pa (k :: small, large)
         | j :: small, [] ->
            prob.[j] <- 1.0
            init (n, prob, alias) pa (small, [])
        | [], k :: large ->
            prob.[k] <- 1.0
            init (n, prob, alias) pa ([], large)
        | [], [] -> sample (n, prob, alias)
    
    let n = Array.length pa
    if n = 0 then failwith "Invalid arg: \"alias\""
    else 
        let sc = float n / Array.foldBack (fun s p -> if p < 0.0 then failwith "Invalid arg: \"p\"" else s + p) pa 0.0
        let sa = Array.map ((*) sc) pa
        init (n, Array.zeroCreate n, Array.create n -1) sa (split sa n 0 ([], []))

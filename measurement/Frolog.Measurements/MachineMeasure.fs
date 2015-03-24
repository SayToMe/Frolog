namespace Frolog.Tests

open Frolog
open Frolog.SearchMachines

[<RequireQualifiedAccess>]
module Distributions =
    let random min max = 
        let r = new System.Random()
        fun (x) -> r.Next(min, max)

module MachinePrepare =
    let private prepare m facts =
        let addrule (s: #ISearchMachine) r = s.AddRule r
        facts |> List.iter(fun r -> addrule m r)

    let simpleGet facts = 
        let machine = SearchMachines.Simple.Create()
        prepare machine facts
        machine :> ISearchMachine
    let lrucacheGet facts prms = 
        let machine = SearchMachines.Custom.LRUMachine prms
        prepare machine facts
        machine :> ISearchMachine
    let lifocacheGet facts prms = 
        let machine = SearchMachines.Custom.LIFOMachine prms
        prepare machine facts
        machine :> ISearchMachine

open MachinePrepare

module MachineMeasure =
    type measureResult = { time: int64; hits: int; result: Signature array array }

    let measureMachine machine queries =
        let execute (m: ISearchMachine) (s: Signature list) hitsGet =
            let res = Array.create s.Length [||]
            let sw = System.Diagnostics.Stopwatch.StartNew()
            for i in 0..s.Length-1 do
                let sign = s.[i]
                let contexts = m.Execute sign |> Seq.toArray
                res.[i] <- contexts
            { time = sw.ElapsedMilliseconds; result = res; hits = hitsGet()}
        let getMeasurement (machine: ISearchMachine) qs =
            execute machine queries (fun () -> 
                match machine with
                | :? SearchMachines.Simple as simpl -> 0
                | :? SearchMachines.Custom as custm -> custm.CacheHits
                | _ -> failwith "Unknown type of search machine.")
        getMeasurement machine queries
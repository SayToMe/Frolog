namespace Frolog.Tests

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.CustomRules

module Main =
    let defFact name x =
        tryDefFact (sprintf "%s(%i)" name x) |> Option.get
        
    let defCall name x =
        sprintf "%s(%i)" name x |> termf |> sign |> Option.get

    let defCall2 name x y =
        sprintf "%s(%s, %s)" name x y |> termf |> sign |> Option.get

    let createFacts predName (distrib: int -> int) size: Rule list =
        [1..size] |> List.map(fun x -> defFact predName (distrib x))
        
    let createCustomFacts (distrib: int -> 'a) factG size: Rule list =
        [1..size] |> List.map(fun x -> factG (distrib x))

    let createSimpleQueries predName (distrib: int -> int) size: Signature list =
        [1..size] |> List.map(fun x -> defCall predName (distrib x))
    
    let createCustomQueries (distrib: int -> 'a) callDef size: Signature list =
        [1..size] |> List.map(fun x -> callDef (distrib x))
        
    let createFibsQueries (distrib: int -> int) size =
        createCustomQueries distrib (fun x -> defCall2 "factorial" (string x) "F") size

    type measureGGParameters = { factsN: int; factsD: int; queriesN: int; queriesD: int; precedencesN: int }
    let measureGGP (mp: measureGGParameters) resCallback =
        let prms = { maxPrecedences = mp.precedencesN }
        let rf = Distributions.random 1 mp.factsD >> (fun o -> o.ToString())
        let rq = Distributions.random 1 mp.queriesD >> (fun o -> o.ToString())
        
        let facts = [1..mp.factsN] |> List.map(fun _ -> GGrandparent.parent (rf()) (rf()))
        let queries = [1..mp.queriesN] |> List.map(fun _ -> defCall2 "ggrandparent" (rq()) (rq()))

        let simple = MachinePrepare.lrucacheGet facts prms
        let fifo = MachinePrepare.lrucacheGet facts prms
        let lifo = MachinePrepare.lifocacheGet facts prms

        let sr = MachineMeasure.measureMachine simple queries
        resCallback "Simple" sr
        let fr = MachineMeasure.measureMachine fifo queries
        resCallback "Fifo" fr
        let lr = MachineMeasure.measureMachine lifo queries
        resCallback "Lifo" lr
        
    // With size get nth item
    type QueryDistribution = int -> int -> int
    type measureParameters = { d: int; factsN: int; queriesN: int; queriesDistrib: QueryDistribution; precedencesN: int }
    let measureDeepRules (mp: measureParameters) resCallback =
        let r = new System.Random()
        let prms = { maxPrecedences = mp.precedencesN }
        let r() = sprintf "%i" (r.Next())

        let facts = [1..mp.factsN] |> List.map(fun i -> DeepRule.getFact (string i) (r()))
        let queries = [1..mp.queriesN] |> List.map(fun i -> signf <| DeepRule.call mp.d (string (mp.queriesDistrib mp.factsN i)) "X")

        let simple = MachinePrepare.simpleGet facts
        let lru = MachinePrepare.lrucacheGet facts prms
        let lifo = MachinePrepare.lifocacheGet facts prms

        let sr = MachineMeasure.measureMachine simple queries
        resCallback "Simple" sr
        let fr = MachineMeasure.measureMachine lru queries
        resCallback "LRU" fr
        let lr = MachineMeasure.measureMachine lifo queries
        resCallback "LIFO" lr

    [<EntryPoint>]
    let main args =
        let sb = new System.Text.StringBuilder()
        let wrf (s: string) = sb.AppendLine(s) |> ignore
        let watch = System.Diagnostics.Stopwatch.StartNew()

        let wrs d fn qn pn name time hits = sprintf "%i\t%i\t%i\t%i\t%s\t%i\t%i" d fn qn pn name time hits |> wrf

        // manual count
        let tenpercent = 96 / 10
        let mutable cnt = 0
        let mutable pcnt = 0

        let r = new System.Random()
        let qd size n =
            // n % size
            r.Next(size)

        wrf "Deep\tFacts\tQueries\tPrcd\tName\tTime\tHits"
        for d in [1; 2] do
            for f in [25; 100; 500; 1000] do
                for q in [100; 500; 1000; 2500] do
                    for p in [q / 100; q / 10; q / 3] do
                        let prms = { d = d; factsN = f; queriesN = q; queriesDistrib = qd; precedencesN = p }
                        measureDeepRules prms (fun name res ->
                            let p = prms
                            wrs p.d p.factsN p.queriesN p.precedencesN name res.time res.hits)
                        if (tenpercent = cnt) then
                            pcnt <- pcnt + 1
                            printfn "%i0 percent completed" pcnt
                            cnt <- 0
                        else
                            cnt <- cnt + 1
        printfn "Wait for result. Executed in %i ms." watch.ElapsedMilliseconds
        let rec getUniqueFileName name suffix d =
            let filename = name + (if suffix = 0 then "" else " (" + string suffix + ")") + "." + d
            if System.IO.File.Exists filename then
                getUniqueFileName name (suffix+1) d
            else
                filename

        let name = System.DateTime.Now.ToShortDateString()

        System.IO.File.WriteAllText(getUniqueFileName name 0 "txt", sb.ToString())
        System.Console.ReadKey() |> ignore
        0
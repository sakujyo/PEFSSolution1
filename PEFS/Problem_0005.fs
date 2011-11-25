module PEFS.Problem_0005

// 解があり現実的な時間内に停止すると仮定する

open PEFS.Library
open System
let solve (problem : decimal) =

// 最小公倍数でfoldするバージョン(< 200ms, Core 2 Duo 1.86GHz
//let solve (problem : int) =
    let lcm (a : decimal) (b : decimal) =
        let max = if a > b then a else b
        let min = if a > b then b else a
        if max % min = 0M then max else
        let limit = max |> float |> Math.Sqrt |> Math.Ceiling |> int

//        printfn "%M %M limit = %d" a b limit

//        System.Console.ReadLine() |> ignore
        
        let maxPFs = primeFactors max 2M ((primes limit).GetEnumerator())
        let minPFs = primeFactors min 2M ((primes limit).GetEnumerator())
//        printf  "maxPFs = %A " maxPFs
//        printfn "minPFs = %A"  minPFs
        let maxFactorAll =
            match Seq.max(maxPFs) > Seq.max(minPFs) with
                | true  -> Seq.max(maxPFs)
                | false -> Seq.max(minPFs)
        seq {
            for i in primes (maxFactorAll |> int) do
//                let aFactorI = aPFs |> List.partition(fun n -> n = i) |> fst
                let maxFactorI = maxPFs |> List.filter(fun n -> n = decimal(i))
//                let bFactorI = bPFs |> List.partition(fun n -> n = i) |> fst
                let minFactorI = minPFs |> List.filter(fun n -> n = decimal(i))
                match maxFactorI.Length > minFactorI.Length with
                    | true -> yield! maxFactorI
                    | false -> yield! minFactorI
        } |> Seq.fold (*) 1M

//    [2M..problem]
    seq { 2M .. problem }
//    |> Seq.fold (lcm) 1M
    |> Seq.fold (lcm) 1M

//    // state 最小公倍数の素因数のリスト
//    let folder (state : decimal list) (nlist : decimal list) =
//        seq {
//            for i in 2m..(problem |> decimal) do
//                let sFactorI = state |> List.filter(fun n -> n = i)
//                let nFactorI = nlist |> List.filter(fun n -> n = i)
//                match sFactorI.Length > nFactorI.Length with
//                    | true  -> yield! sFactorI
//                    | false -> yield! nFactorI
//        } |> Seq.toList
//
//    [2..problem]
//    |> Seq.map(fun n ->
//        primeFactors (n |> decimal) 2m ((primes n).GetEnumerator())
//    )
//    |> Seq.fold(folder) []
//    |> Seq.fold (*) 1M

let run () =
    solve 20M

let run2 () =
//    for i in [1..20] do
//        primes i
//        |> Seq.iter(fun n -> printf "%d " n)
//        printfn "";;
    // 解があり現実的な時間内に停止すると仮定する

//    naturals
//    |> Seq.find(fun n ->
//        //printf "%d " n
//        [2..20]
//        |> Seq.forall(fun m ->
//            n % m = 0
//        )
//    )

    // state 最小公倍数
    let folder (state : decimal list) (nlist : decimal list) =
//        let ds = state |> decimal
//        let nInt = n |> int
//        let sPFs = primeFactors ds 2m ((primes state
//        let nPFs = primeFactors n 2m ((primes nInt).GetEnumerator())
//        let newState = []
        seq {
            for i in 2m..20m do
                let sFactorI = state |> List.partition(fun n -> n = i) |> fst
                let nFactorI = nlist |> List.partition(fun n -> n = i) |> fst
//                newState |> List.append
                match sFactorI.Length < nFactorI.Length with
                    | true -> yield! nFactorI
//                    | true -> yield (i, nFactorI.Length)
//                    | false -> yield (i, sFactorI.Length)
                    | false -> yield! sFactorI
        } |> Seq.toList

//    primeFactors 36m 2m ((primes 36).GetEnumerator()) 
    [2..20]
    |> Seq.map(fun n ->
        let d = n |> decimal
        primeFactors d 2m ((primes n).GetEnumerator())
    )
//    |> Seq.iter(fun a -> printfn "%A" a)
    |> Seq.fold(folder) []
    |> Seq.reduce (*)
    //232792560

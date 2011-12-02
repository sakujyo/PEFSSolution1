module PEFS.Library

//let upperLimit = float problem |> System.Math.Sqrt |> System.Math.Ceiling |> int
let mutable pfaLimit = 1
let primeFlagsArray = ref (Array.create (pfaLimit + 1) false)

// primes n は n までの素数のシーケンスを返す
let primes (upperLimit : int) =
    if upperLimit > pfaLimit then
        primeFlagsArray := Array.create (upperLimit + 1) true
        pfaLimit <- upperLimit
    //let primeFlagsArray : bool array = Array.create (upperLimit + 1) true
        for i in 2 .. (!primeFlagsArray).Length - 1 do
        if (!primeFlagsArray).[i] then
//            1 |> ignore // for BREAK Point
            for j in i * 2 .. i .. (!primeFlagsArray).Length - 1 do
                Array.set (!primeFlagsArray) j false
//            for j in [i * 2 .. i .. (!primeFlagsArray).Length - 1] do
            done
        else
            ()
        done

//    seq { for i in [2..upperLimit] do if (!primeFlagsArray).[i] then yield i}
    seq { for i in 2..upperLimit do if (!primeFlagsArray).[i] then yield i}

let naturals =
    Seq.initInfinite(fun n -> n + 1)

/// primeFactors (n : decimal) 2m (primes.GetEnumerator()) は n の素因数の decimal list を返す
let rec primeFactors n p (e : System.Collections.Generic.IEnumerator<int>) =
    match n with
        | n when (n = 1m) -> []
//        | n when p * p > n -> [n]
        | n when n % p = 0m -> (p |> decimal) :: primeFactors (n / p) p e
        | n ->
            if e.MoveNext() then
                (primeFactors n (e.Current |> decimal) e)
            else [n]
        | _ -> []

// RemoveNthElement alist [] index
let rec RemoveNthElement listn listx n =
    match listn with
        | head :: tail ->
            match n with
                | 0 -> listx@tail
                | _ -> RemoveNthElement tail (listx@[head]) (n - 1)
        | [] -> listx       //n がリストの長さ以上だった場合      

  
open System
let pf (n : decimal) =
    let x = n |> float |> Math.Sqrt |> Math.Ceiling |> int
    primeFactors n 2m ((primes x).GetEnumerator())

/// pFactorsInt (n : int) 2 (primes.GetEnumerator()) は n の素因数の decimal list を返す
let rec pFactorsInt n p (e : System.Collections.Generic.IEnumerator<int>) =
    match n with
        | n when (n |> int = 1) -> []
//        | n when p * p > n -> [n]
        | n when (n % p) |> int = 0 -> p :: pFactorsInt (n / p) p e
        | n ->
            if e.MoveNext() then
                (pFactorsInt n e.Current e)
            else [n]
        | _ -> []

let rec primeFactorsDecimal n p (e : System.Collections.Generic.IEnumerator<int>) =
    match n with
        | n when (n = 1m) -> []
//        | n when p * p > n -> [n]   // 素因数分解の結果が int におさまる保証はない
        | n when n % p = 0m -> (p |> decimal) :: primeFactorsDecimal (n / p) p e
        | n ->
            if e.MoveNext() then
                (primeFactorsDecimal n (e.Current |> decimal) e)
            else [n]
        | _ -> []

// LCM (a : decimal) (b : decimal) は a と b の最小公倍数を返す(DP非対応版)
open System
let LCM (a : decimal) (b : decimal) =
    let max = if a > b then a else b
    let min = if a > b then b else a
    if max % min = 0M then max else
    let limit = max |> float |> Math.Sqrt |> Math.Ceiling |> int
    let maxPFs = primeFactors max 2M ((primes limit).GetEnumerator())
    let minPFs = primeFactors min 2M ((primes limit).GetEnumerator())
    let maxFactorAll =
        match Seq.max(maxPFs) > Seq.max(minPFs) with
            | true  -> Seq.max(maxPFs)
            | false -> Seq.max(minPFs)
    seq {
        for i in primes (maxFactorAll |> int) do
            let maxFactorI = maxPFs |> List.filter(fun n -> n = decimal(i))
            let minFactorI = minPFs |> List.filter(fun n -> n = decimal(i))
            match maxFactorI.Length > minFactorI.Length with
                | true -> yield! maxFactorI
                | false -> yield! minFactorI
    } |> Seq.fold (*) 1M

// intLCM (a : int) (b : int) は a と b の最小公倍数を返す(DP非対応版)
// ただし素因数だらけの数同士のLCMだとintなんてすぐあふれちゃいます
open System
let intLCM (a : int) (b : int) =
    let max = if a > b then a else b
    let min = if a > b then b else a
    if max % min = 0 then max else
    let limit = max |> float |> Math.Sqrt |> Math.Ceiling |> int
    let maxPFs = pFactorsInt max 2 ((primes limit).GetEnumerator())
    let minPFs = pFactorsInt min 2 ((primes limit).GetEnumerator())
    let maxFactorAll =
        match Seq.max(maxPFs) > Seq.max(minPFs) with
            | true  -> Seq.max(maxPFs)
            | false -> Seq.max(minPFs)
    seq {
        for i in primes maxFactorAll do
            let maxFactorI = maxPFs |> List.filter(fun n -> n = i)
            let minFactorI = minPFs |> List.filter(fun n -> n = i)
            match maxFactorI.Length > minFactorI.Length with
                | true -> yield! maxFactorI
                | false -> yield! minFactorI
    } |> Seq.fold (*) 1

// int n = 12 程度までしか正しい値は返らない
// decimal n = 27M 程度までしか正しい値は返らない
let rec factorial n =
    match n with
        | 0M -> 1M
        | 1M -> 1M
        | _ -> n * factorial (n - 1M)



//let lcm (a : int) (b : int) =
//    let max = if a > b then a else b
//    let min = if a > b then b else a
//    if max % min = 0 then max else
//    let aPFs = pFactorsInt a 2 ((primes a).GetEnumerator())
//    let bPFs = pFactorsInt b 2 ((primes b).GetEnumerator())
//    seq {
//        for i in primes max do
//            let aFactorI = aPFs |> List.filter(fun n -> n = i)
//            let bFactorI = bPFs |> List.filter(fun n -> n = i)
//            match aFactorI.Length > bFactorI.Length with
//                | true -> yield! aFactorI
//                | false -> yield! bFactorI
//    } |> Seq.fold (*) 1

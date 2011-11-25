// プログラム実行の仕組みは整理しなくていいか？
// あと実時間ではなくCPUTIMEで計測したい

module PEFS.Problem_0003
//    let problem = 600851475143m
//    let problem = 37366090917157m   // CPUTIME 14s (Core 2 Duo 1.86GHz)

//     割り算の対象自体は、2 から Sqrt(n) までの素数でよい、
//     ただし素因数がSqrt(n)より小さいものだけであるという意味ではない



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
//            for j in [i * 2 .. i .. (!primeFlagsArray).Length - 1] do
            for j in i * 2 .. i .. (!primeFlagsArray).Length - 1 do
                Array.set (!primeFlagsArray) j false
            done
        else
            ()
        done

//    seq { for i in [2..upperLimit] do if (!primeFlagsArray).[i] then yield i}
    seq { for i in 2..upperLimit do if (!primeFlagsArray).[i] then yield i}



//    let upperLimit = float problem |> System.Math.Sqrt |> System.Math.Ceiling |> int

//    let primeFlagsArray : bool array = Array.create (upperLimit + 1) true
//    let primes = seq { for i in [2..upperLimit] do if primeFlagsArray.[i] then yield i}
//

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

//open PEFS.Library
open System
let test problem =
    let upperLimit = problem |> float |> Math.Sqrt |> Math.Ceiling |> int
    let result = primeFactors problem 2m ((primes upperLimit).GetEnumerator())
    result

let run () =
//        for i in 2 .. primeFlagsArray.Length - 1 do
//        if primeFlagsArray.[i] then
//            for j in [i * 2 .. i .. primeFlagsArray.Length - 1] do
//                Array.set primeFlagsArray j false
//            done
//        else
//            ()
//        done
//
    let problem = 600851475143m
    test problem
//        primes |>





        //primeFactors problem 2m (primes.GetEnumerator())

        //maxPrimeFactor problem
//        let psum = primes |> Seq.filter(fun x -> x <= 200 * 10000)|> Seq.sum
//        psum
// プログラム実行の仕組みは整理しなくていいか？
// あと実時間ではなくCPUTIMEで計測したい
namespace PEFS
module Problem_0003 =
//    let problem = 81m
//    let problem = 13195m
//    let problem = 30971726M
//    let problem = 435290354M

//    let problem = 452902805417m
//    let problem = 600851475143m
//    let problem = 47018449m
    let problem = 37366090917157m   // CPUTIME 17s (Core 2 Duo 1.86GHz)


//     割り算の対象自体は、2 から Sqrt(n) までの素数でよい、
//     ただし素因数がSqrt(n)より小さいものだけであるという意味ではない

    let upperLimit = float problem |> System.Math.Sqrt |> System.Math.Ceiling |> int

    let primeFlagsArray : bool array = Array.create (upperLimit + 1) true
    let primes = seq { for i in [2..upperLimit] do if primeFlagsArray.[i] then yield i}

    let rec primeFactors n p (e : System.Collections.Generic.IEnumerator<int>) =
        match n with
            | n when (n = 1m) -> []
            | n when p * p > n -> [n]
            | n when n % p = 0m -> (p |> decimal) :: primeFactors (n / p) p e
            | n ->
                if e.MoveNext() then
                    (primeFactors n (e.Current |> decimal) e)
                else []
            | _ -> []

    let test problem_n =
        let result = primeFactors problem_n 2m (primes.GetEnumerator())
        printfn "%A" result
        result

    let run () =
        for i in 2 .. primeFlagsArray.Length - 1 do
            if primeFlagsArray.[i] then
                for j in [i * 2 .. i .. primeFlagsArray.Length - 1] do
                    Array.set primeFlagsArray j false
                done
            else
                ()
        done

//        primes |>





        //primeFactors problem 2m (primes.GetEnumerator())

        //maxPrimeFactor problem
//        let psum = primes |> Seq.filter(fun x -> x <= 200 * 10000)|> Seq.sum
//        psum
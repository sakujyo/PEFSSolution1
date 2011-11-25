// Learn more about F# at http://fsharp.net


module PEFS.Main

//open PEFSTest.PEFSTestModule
//循環依存となるためNG


// モジュール名を変える事で別の問題の解答を取得出来る
let getAnswer () =

    let cp = System.Diagnostics.Process.GetCurrentProcess()
    let t1 = cp.TotalProcessorTime

//    let answer = PEFS.Problem_0000.run(1234)
//    let answer = PEFS.Problem_0011.run()
//    let answer = PEFS.Problem_0011rege.run()
//    let answer = PEFS.Problem_0012.run()
//    let answer = PEFS.Problem_0013.run()
//    let answer = PEFS.Problem_0067.run()
//    let answer = PEFS.Problem_0001.run()
//    let answer = PEFS.Problem_0002.run()
//    let answer = PEFS.Problem_0002rege.run()
//    let answer = PEFS.Problem_0003.run()
//    let answer = PEFS.Problem_0005.run()
//    let answer = PEFS.Problem_0004.run()
//    let answer = PEFS.Problem_0004graphical.run()
    let answer = PEFS.Problem_0010.run()

//    let answer = PEFS.Problem_0016.run()
//    let answer = Problem_0067.run()

    let t2 = cp.TotalProcessorTime
    printfn "CPU Time: %A" (t2 - t1)

    printfn "%A" answer

open System.Diagnostics
let main() =
    let stopWatch = new Stopwatch()
    stopWatch.Start()

    // 実際の処理
    getAnswer()

    stopWatch.Stop();

    printfn "%d ms" stopWatch.ElapsedMilliseconds
    ()

main()


(* ブロックコメント。ネストできる。
    (*
    　printfだと改行無しになります
    *)
*)
(* ←このコメントをON/OFFする
	printfn "hello world";;
// *)
//not true;;		//論理値の否定。bool値のみ

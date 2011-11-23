// Learn more about F# at http://fsharp.net

module PEFSTestTest

open PEFSTest.PEFSTestModule

let p01 a =
    printfn "%d" a

let p02 = fun a -> a
    //x
    
let p03 (transform : int -> unit) y = 
    printfn "a: "
    transform y
    printfn "b: "

let p04 = p01
let m01 =
//    PEFSTest.PEFSTestModule.MyTest.measureCPUT p01 1
    p03 p01 1
//    p03 p04 1
//    PEFSTest.PEFSTestModule.MyTest.measureCPUT(p01 1)
    ()

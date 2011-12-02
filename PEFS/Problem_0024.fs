module PEFS.Problem_0024

(*
Problem 24 †
順列とはモノの順番付きの並びのことである. たとえば, 3124は数1, 2, 3, 4の一つの順列である. 
すべての順列を数の大小でまたは辞書式に並べたものを辞書順と呼ぶ. 
0と1と2の順列を辞書順に並べると

012 021 102 120 201 210
になる.

0,1,2,3,4,5,6,7,8,9からなる順列を辞書式に並べたときの100万番目を答えよ
*)

//f 1000000 桁数(mPm) n:階乗 i:次にかける値(1から始める)
//      f 6 4 1 1 = 1
let rec f p m n i =
    match i = m with
        | true  -> [(p - 1) / n], (p - 1) % n
        | false ->  let a = f p m (n * i) (i + 1)
                    snd(a) / n::fst(a), snd(a) % n
                    
// RemoveElement alist [] element
let rec RemoveElement listn listx e =
    match listn with
        | head :: tail ->
            match head = e with
                | true -> RemoveElement tail listx e
                | _ -> RemoveElement tail (listx@[head]) e
        | [] -> listx   

let rec decode clist olist num =
    match olist with
        | head::tail -> decode (RemoveElement clist [] (List.nth clist head)) tail (num + string(List.nth clist head))
        | [] -> num
        
let solve problem =
    decode [0..9] (List.rev (fst(f problem 10 1 1))) ""

let run () =
    solve (100 * 10000)   //100万番目を求める

























//let rec f p m n i =
//    match i = m with
////        | true  -> [m * (p - 1) / n]                //((n - 1) / p) + 1
//        | true  -> [(p - 1) / n], (p - 1) % n                //((n - 1) / p) + 1
//        | false ->  let a = f p m (n * i) (i + 1)
//                    snd(a) / n::fst(a), snd(a) % n //let m = (f p (n * i) (i + 1))
////        | false -> 1::f p m (n * (i + 1)) (i + 1)  //let m = (f p (n * i) (i + 1))

//// RemoveElement alist [] index
//let rec RemoveElement listn listx n =
//    match listn with
//        | head :: tail ->
//            match n with
//                | 0 -> listx@tail
//                | _ -> RemoveElement tail (listx@[head]) (n - 1)
//        | [] -> listx

//let rec decode (clist : int list) (olist : int list) (num : string) =
//    match clist.Length = 0 with
//        | true -> num
//        | _    -> decode (RemoveElement clist [] olist.Head) olist.Tail (num + string(List.nth clist olist.Head))

//let rec RemoveElement listn listx n =
//    match listn with
//        | head :: tail ->
//            match n with
//                | 0 -> listx@tail
//                | _ -> RemoveElement tail (listx@[head]) (n - 1)
//        | [] -> listx       //n がリストの長さ以上だった場合      
//
//let rec decode clist olist num =
//    match olist with
//        | head::tail -> decode (RemoveElement clist [] head) tail (num + string(List.nth clist head))
//        | [] -> num
 
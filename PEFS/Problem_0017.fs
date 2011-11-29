module PEFS.Problem_0017

//Problem 17 †
//1 から 5 までの数字を英単語で書けば one, two, three, four, five であり、全部で 3 + 3 + 5 + 4 + 4 = 19 の文字が使われている。
//
//では 1 から 1000 (one thousand) までの数字をすべて英単語で書けば、全部で何文字になるか。
//
//注: 空白文字やハイフンを数えないこと。
//例えば、342 (three hundred and forty-two) は 23 文字、
//115 (one hundred and fifteen) は20文字と数える。なお、"and" を使用するのは英国の慣習。

let dic (n : int) =
    match n with
        | 0 -> "zero"
        | 1 -> "one"
        | 2 -> "two"
        | 3 -> "three"
        | 4 -> "four"
        | 5 -> "five"
        | 6 -> "six"
        | 7 -> "seven"
        | 8 -> "eight"
        | 9 -> "nine"
        | 10 -> "ten"
        | 11 -> "eleven"
        | 12 -> "twelve"
        | 13 -> "thirteen"
        | 14 -> "fourteen"
        | 15 -> "fifteen"
        | 16 -> "sixteen"
        | 17 -> "seventeen"
        | 18 -> "eighteen"
        | 19 -> "nineteen"
        | _ -> ""

let dic2 (n : int) =
    match n with
        | 2 -> "twenty"
        | 3 -> "thirty"
        | 4 -> "forty"
        | 5 -> "fifty"
        | 6 -> "sixty"
        | 7 -> "seventy"
        | 8 -> "eighty"
        | 9 -> "ninety"
        | _ -> ""

let rec write (n : int) =
    match n >= 1000 with
        | true  -> "one thousand"    // あの1000を超える数をまだ知らない
        | false ->
            match n >= 100 with
                | true  ->
                    match n % 100 = 0 with
                        | true -> dic (n / 100) + " " + "hundred"
                        | false -> write (n / 100) + " hundred and " + write (n % 100)
                | false ->
                    match n > 19 with
                        | true -> 
                            match n % 10 = 0 with
                                | true -> dic2 (n / 10)
                                | false -> dic2 (n / 10) + "-" + dic (n % 10)
                        | false -> dic n

let trim (s : string) =
    s.Length - 
        (s |> Seq.map (fun c -> 
            match c with
                | ' ' | '-' -> 1
                | _ -> 0
            ) |> Seq.sum
        )

let solve (problem : int) =
    seq { 1..problem }
    |> Seq.map (fun n -> trim(write n))
    |> Seq.sum

let run () =
    solve 1000

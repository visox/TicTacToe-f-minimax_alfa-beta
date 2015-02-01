// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
//module Array2D

type FieldStatus = 
    | untaken = 0
    | player1 = 1
    | player2 = 2

type Board = 
    { fields : seq<int * int * FieldStatus> }

let boardSize = 3
let winnerSize = 3

let initBoard = 
    { fields = 
          seq { 
              for x in 1..boardSize do
                  for y in 1..boardSize do
                      yield (x, y, FieldStatus.untaken)
          } }

let oneMove (board : Board, xx: int, yy: int, status: FieldStatus) = 
    let fields_ = seq{yield! (board.fields |> Seq.filter(fun (x,y,_) -> x <> xx || y <> yy)); yield(xx,yy, status)  }
    let res : Board = { fields = fields_  }
    res

let moveOptions board =
    board.fields |> Seq.filter (fun (x, y, stat) -> stat.Equals(FieldStatus.untaken))
    |> Seq.map (fun (x, y, _) -> (x, y))

let fieldByXY (x,y, board) : FieldStatus =
    let (_, _, result) = board.fields |> Seq.filter (fun (xx, yy,_) -> x = xx && y = yy) |> Seq.exactlyOne
    result

let winningSeq (seq, player) =
    let result = seq |> Seq.exists(fun (stat) -> stat <> player)
    not result

let rec diagonale1 (board, x, y) =
    match (x,y) with
        | (xx, _) when xx > boardSize -> Seq.empty
        | (_, yy) when yy > boardSize -> Seq.empty
        | (xx,yy) -> Seq.append (diagonale1(board, xx+1, yy+1)) [fieldByXY(xx, yy, board)]

let rec diagonale2 (board, x, y) =
    match (x,y) with
        | (xx, _) when xx = 0 -> Seq.empty
        | (_, yy) when yy = 0 -> Seq.empty
        | (xx,yy) -> Seq.append (diagonale2(board, xx-1, yy-1)) [fieldByXY(xx, yy, board)]

let rec horizontal (board, x, y) =
    match (x,y) with
        | (xx, _) when xx > boardSize -> Seq.empty
        | (xx,yy) -> Seq.append (horizontal(board, xx+1, yy)) [fieldByXY(xx, yy, board)]

let rec vertical (board, x, y) =
    match (x,y) with
        | (_, yy) when yy > boardSize -> Seq.empty
        | (xx,yy) -> Seq.append (vertical(board, xx, yy+1)) [fieldByXY(xx, yy, board)]

let rec winningStrike (fields, player, need, now) =
    match fields with
        |empt when (Seq.length empt) < now -> false
        |_ ->
            match Seq.head fields with
                | player_ when player_ = player -> 
                    match now with
                        | more when more > 1 -> winningStrike ((Seq.skip 1 fields), player, need, now-1)
                        | 1 -> true
                | _ -> winningStrike ((Seq.skip 1 fields), player, need, need)
         
let hasWonDiag1_1 (board, player) = 
    seq{for x in 1..boardSize do yield x}
    |> Seq.exists (fun(x) ->
        let set = diagonale1(board, x, 1)
        winningStrike(set, player, winnerSize, winnerSize))

let hasWonDiag1_2 (board, player) = 
    seq{for y in 1..boardSize do yield y}
    |> Seq.exists (fun(y) ->
        let set = diagonale1(board, 1, y)
        winningStrike(set, player, winnerSize, winnerSize))

let hasWonDiag2_1 (board, player) = 
    seq{for x in 1..boardSize do yield x}
    |> Seq.exists (fun(x) ->
        let set = diagonale2(board, x, boardSize)
        winningStrike(set, player, winnerSize, winnerSize))

let hasWonDiag2_2 (board, player) = 
    seq{for y in 1..boardSize do yield y}
    |> Seq.exists (fun(y) ->
        let set = diagonale2(board, boardSize, y)
        winningStrike(set, player, winnerSize, winnerSize))

let hasWonHorizontal (board, player) = 
    seq{for y in 1..boardSize do yield y}
    |> Seq.exists (fun(y) ->
        let set = horizontal(board, 1, y)
        winningStrike(set, player, winnerSize, winnerSize))

let hasWonVertical (board, player) = 
    seq{for x in 1..boardSize do yield x}
    |> Seq.exists (fun(x) ->
        let set = vertical(board, x, 1)
        winningStrike(set, player, winnerSize, winnerSize))


let hasWon (board, player) =
    hasWonDiag1_1(board, player) ||
    hasWonDiag1_2(board, player) ||
    hasWonDiag2_1(board, player) ||
    hasWonDiag2_2(board, player) ||
    hasWonHorizontal(board, player) ||
    hasWonVertical(board, player) 

let evalBoard(board) =
    match hasWon(board, FieldStatus.player1) with
        | true -> 1
        | false ->  match hasWon(board, FieldStatus.player2) with
                    | true -> -1
                    | false -> 0

let rec maximum (board, prevMin, toEval, maximumCur) =
    match toEval |> Seq.map(fun(x,y) -> minMove(oneMove(board, x, y, FieldStatus.player1), maximumCur)) |> Seq.head  with
        | (eval, _, _) when eval >= prevMin || Seq.length toEval = 1 -> 
            let (xx,yy) = toEval|> Seq.head
            (max eval maximumCur, xx,yy)
        | (eval, _, _) -> 
            let (res, xx, yy) = maximum (board, prevMin, toEval |> Seq.skip 1, max eval maximumCur) 
            match res with
                | bigger when bigger > eval -> (res, xx, yy)
                | smaller -> 
                    let (xx,yy) = toEval|> Seq.head
                    (eval, xx, yy)

and minimum (board, prevMax, toEval, minimumCur) =
    match toEval |> Seq.map(fun(x,y) -> maxMove(oneMove(board, x, y, FieldStatus.player2), minimumCur)) |> Seq.head  with
        | (eval, _, _) when eval <= prevMax || Seq.length toEval = 1 -> 
            let (xx,yy) = toEval|> Seq.head
            (min eval minimumCur, xx,yy)
        | (eval, _, _) -> 
            let (res, xx, yy) = minimum (board, prevMax, toEval |> Seq.skip 1, min eval minimumCur)         
            match res with
                | smaller when smaller < eval -> (res, xx, yy)
                | bigger -> 
                    let (xx,yy) = toEval|> Seq.head
                    (eval, xx, yy)


and maxMove (board, prevMin) : (int* int* int) = 
    let moves = moveOptions(board)
    match moves with
        | mov when (Seq.length mov) > 0 ->
             maximum(board,prevMin, mov, -1)         
        | _ -> (evalBoard(board), -1,-1)
and minMove (board, prevMax) : (int * int * int) = 
    let moves = moveOptions(board)
    match moves with
        | mov when (Seq.length mov) > 0 ->
            minimum(board,prevMax, mov, 1)    
        | _ -> (evalBoard(board), -1, -1)



[<EntryPoint>]
let main argv = 
    let board = initBoard
    //let board = oneMove(board, 1,1, FieldStatus.player1)
   // let board = oneMove(board, 2,2, FieldStatus.player1)
    //let board = oneMove(board, 3,3, FieldStatus.player1)
   // let x = hasWonDiag1_1 (board, FieldStatus.player1) 

    let (won, x, y) = maxMove(board, 1)
    printfn "%A" argv
    0 // return an integer exit code

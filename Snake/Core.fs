namespace Snake

type Pos = { X: int; Y: int; }

type Dir = 
    | N
    | S
    | E
    | W

type Move =
    | Left
    | Right

type Snake = Dir * Pos list

type State = { Snake1: Snake; Snake2: Snake; }

type Board = { Xmin: int; Xmax: int; Ymin: int; Ymax: int }

module Game =

    let rev (d:Dir) =
        match d with
        | N -> S
        | W -> E
        | S -> N
        | E -> W

    let left (d:Dir) =
        match d with
        | N -> W
        | W -> S
        | S -> E
        | E -> N

    let right (d:Dir) = d |> rev |> left

    let move (x:Pos) (d:Dir) =
        match d with
        | N -> { x with Y = x.Y - 1 }
        | S -> { x with Y = x.Y + 1 }
        | E -> { x with X = x.X + 1 }
        | W -> { x with X = x.X - 1 }
                
    let turn (d: Dir) (m: Move option) =
        match m with
        | None -> d
        | Some(x) ->
            match x with
            | Left -> left d
            | Right -> right d

    let update (s: Snake) (m: Move option) =
        let (d,(x::xs)) = s // ignore empty snake
        let d' = turn d m
        d',(move x d')
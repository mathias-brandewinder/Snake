open System
open Snake
open Snake.Game

type MoveResult =
    | Exit
    | Collision
    | OK

[<EntryPoint>]
let main argv = 

    printfn "%A" argv

    let rng = Random ()

    let move () =
        let r = rng.NextDouble ()
        if r < 0.3 then Some(Left)
        elif r < 0.6 then Some(Right)
        else None

    let result (b:Board) (s:Snake) (p:Pos) =
        if p.X > b.Xmax || p.X < b.Xmin || p.Y > b.Ymax || p.Y < b.Ymin
        then Exit
        elif (snd s) |> List.exists (fun x -> x = p)
        then Collision
        else OK

    let render (p:Pos) =
        Console.SetCursorPosition(p.X,p.Y)
        Console.ForegroundColor <- System.ConsoleColor.Red
        Console.Write "*"

    let exit () =
        Console.Write "EXIT!"
        Console.ReadLine () |> ignore

    let collision () =
        Console.Write "COLLISION!"
        Console.ReadLine () |> ignore

    let rec loop (b:Board) (s:Snake) =
        let m = move ()
        let (d,hd) = update s m
        match (result b s hd) with
        | Exit -> exit ()
        | Collision -> collision ()
        | OK ->  
            System.Threading.Thread.Sleep 20          
            hd |> render
            let s' = d,hd::(snd s)
            loop b s'

    let snake = N, [{X=20;Y=20}]
    let board = { Xmin = 0; Xmax = 50; Ymin = 0; Ymax = 50 }
    loop board snake

    0 // return an integer exit code

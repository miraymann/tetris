module Tetris =
    open System 
    
    type Direction = Left = -1 | Right = 1 | Down = 0
    type Command   = Move of Direction | Transform | Nil
    
    let rec main () =    
        let go (glass:int list list) next score command =
            let createTetrimino () = 
                let i = (new Random()).Next 19       
               //[x1,y1;x2,y2;x3,y3;x4,y4]      
                [[ 4,18; 5,18; 4,19; 5,19] //O1
                 [ 4,19; 3,19; 5,19; 6,19] //I1
                 [ 4,18; 4,19; 4,17; 4,16] //I2
                 [ 4,18; 4,17; 5,18; 5,19] //Z1
                 [ 5,19; 4,19; 5,18; 6,18] //Z2
                 [ 4,18; 5,17; 5,18; 4,19] //S1 
                 [ 5,19; 4,18; 5,18; 6,19] //S2 
                 [ 5,18; 5,17; 5,19; 4,19] //L1 
                 [ 5,19; 6,19; 4,19; 4,18] //L2 
                 [ 4,18; 4,19; 4,17; 5,17] //L3 
                 [ 5,18; 4,18; 6,18; 6,19] //L4
                 [ 4,18; 4,17; 4,19; 5,19] //J1
                 [ 5,19; 4,19; 6,19; 6,18] //J2
                 [ 5,18; 5,19; 5,17; 4,17] //J3
                 [ 5,18; 6,18; 4,18; 4,19] //J4
                 [ 5,18; 5,17; 5,19; 4,18] //T1
                 [ 5,18; 4,18; 6,18; 5,19] //T2
                 [ 4,18; 4,19; 4,17; 5,18] //T3
                 [ 5,19; 6,19; 4,19; 5,18] //T4
                ].[i] |> List.mapi (fun j (x,y) -> (x,y,i*4+j+1))                                               
            let isPossible (glass:int list list) tetrimino = 
                tetrimino |> List.fold (fun p (x,y,_) -> p && x>=0 && x<=9 && y>=0 && y<=19 && glass.[y].[x]<> -1) true 
            let change_glass glass glass_changing =
                let rec goRows y =
                    let rec goCells x = function 
                        | h::t ->  (fun p -> if List.exists p glass_changing
                                             then let (_,_,v) = glass_changing |> List.find p in v::goCells (x+1) t   
                                             else h::goCells (x+1) t) (fun (x',y',_) -> x=x' && y=y')
                        | []   -> []
                    function | h::t -> (goCells 0 h)::(goRows (y+1) t)
                             | []   -> []
                goRows 0 glass
            let _if p v = if p then Some v else None               
            let do_command () =
                let transform tetrimino =  
                    let ((_,_,v)::_ as tetrimino') = tetrimino |> List.sortBy (fun (_,_,v) -> v) 
                   //[  2  ;  3  ;  4  ]
                   //[dx,dy;dx,dy;dx,dy], v // r
                    [[ 0, 0; 0, 0; 0, 0], 0 // O1 -> O1
                     [ 1, 1;-1,-1;-2,-2], 1 // I1 -> I2
                     [-1,-1; 1, 1; 2, 2],-1 // I2 -> I1
                     [-1, 1;-1,-1; 0,-2], 1 // Z1 -> Z2
                     [ 1,-1; 1, 1; 0, 2],-1 // Z2 -> Z1
                     [-2, 0;-1,-1; 1,-1], 1 // S1 -> S2
                     [ 2, 0; 1, 1;-1, 1],-1 // S2 -> S1
                     [ 1, 1;-1,-1; 0,-2], 1 // L1 -> L2
                     [-1, 1; 1,-1; 2, 0], 1 // L2 -> L3
                     [-1,-1; 1, 1; 0, 2], 1 // L3 -> L4
                     [ 1,-1;-1, 1;-2, 0],-3 // L4 -> L1
                     [-1, 1; 1,-1; 0,-2], 1 // J1 -> J2
                     [ 1, 1;-1,-1;-2, 0], 1 // J2 -> J3
                     [ 1,-1;-1, 1; 0, 2], 1 // J3 -> J4
                     [-1,-1; 1, 1; 2, 0],-3 // J4 -> J1
                     [-1, 1; 1,-1; 1, 1], 1 // T1 -> T2 
                     [ 1, 1;-1,-1; 1,-1], 1 // T2 -> T3
                     [ 1,-1;-1, 1;-1,-1], 1 // T3 -> T4
                     [-1,-1; 1, 1;-1, 1],-3 // T4 -> T1
                    ].[v/4] |> (fun (l,i) -> [for (x,y,v),(dx,dy) in (0,0)::l |> List.zip tetrimino' -> x+dx,y+dy,v+i*4])
                let move d tetrimino = [for x,y,v in tetrimino -> (x+(int d), y-(if d = Direction.Down then 1 else 0),v)]
                let getTetrimino () =
                    let rec parse_rows y = function
                        | []    -> []
                        | h::t  -> let rec parse_cells x = function
                                       |[]            -> []
                                       |h::t when h>0 -> (x,y,h) :: parse_cells (x+1) t 
                                       |_::t          -> parse_cells (x+1) t 
                                   (parse_cells 0 h) @ (parse_rows (y+1) t) 
                    parse_rows 0 glass 
                let (|IsPosible|_|) tetrimino = tetrimino |> _if (isPossible glass tetrimino)
                let (|IsLand|_|) tetrimino = 
                    tetrimino |> _if (tetrimino |> List.fold (fun p (x,y,_) -> p || y<0 || glass.[y].[x]= -1) false)
                let (<+>) dglass dglass' = //not commutativity
                    let rec add = function
                        | l,[] | [],l                                                     -> l
                        | (x,y,_ as h)::t,((x',y',_)::_ as t') when y<y'|| (y=y' && x<x') -> h ::add (t, t')
                        | (x,y,_)::t,(x',y',_ as h')::t'       when y=y' && x=x'          -> h'::add (t, t')
                        | t,h'::t'                                                        -> h'::add (t, t')
                        | _                                                               -> []
                    let sort = List.sortWith (fun (x,y,_) (x',y',_) -> if y>y' || (y=y' && x>x') then 1 else -1)
                    add (sort dglass, sort dglass')
                let seter v = fun (x,y,_) -> x,y,v
                (fun t -> match command, (match command with | Transform -> transform | Move d -> move d) t with
                          | _,IsPosible t'                 -> (t |> List.map (seter  0)) <+> t'
                          | Move Direction.Down, IsLand t' -> (t |> List.map (seter -1))
                          | _                              -> []) (getTetrimino ()) 
            if command = Nil
            then Some (change_glass glass (createTetrimino ()), createTetrimino (), score)
            else let dglass = do_command ()
                 let glass' = change_glass glass dglass
                 let update_glass () =
                     let (|IsFullRow|_|) row = row |> _if (row |> List.fold (fun p i -> p && (i= -1)) true)
                     let rec update_glass' rows y ys killed_count =
                         match rows, ys with
                         | _, []                  -> rows @ ((0 |> List.replicate 10) |> List.replicate killed_count)
                         | h::t, y'::ys when y'=y -> update_glass' t (y+1) ys (killed_count+1)
                         | h::t, ys               -> h::(update_glass' t (y+1) ys killed_count)
                     let rec killed_rows y = function 
                         | (IsFullRow _)::t -> y::(killed_rows (y+1) t)
                         | _::t             -> killed_rows (y+1) t 
                         | []               -> []
                     (fun k -> update_glass' glass' 0 k 0, k |> List.length) (killed_rows 0 glass')
                 (function | (_,_,-1)::_ -> let (glass', killed_rows_count) = update_glass ()
                                            if isPossible glass' next
                                            then Some (change_glass glass' next, createTetrimino (), score + [0;1;3;7;15].[killed_rows_count])
                                            else None
                           | _           -> Some (glass', next, score)) dglass
        let print_char_to_cell (v:char) x y = Console.SetCursorPosition(x+1,20-y) 
                                              Console.Write v
        let rec game_over () = Console.Clear ()
                               Console.ForegroundColor <- ConsoleColor.Red
                               Console.WriteLine "GAME OVER!!!"
                               Console.Write "please press R - for restart, E - for exit"
                               match (Console.ReadKey true).Key with
                               | ConsoleKey.R -> main()
                               | ConsoleKey.E -> ()
                               | _            -> game_over()
        let printAthom = print_char_to_cell '█'
        let clearAthom = print_char_to_cell ' '
        let rec go' (glass:int list list) next score command i = 
            let print_glass glass glass' = 
                (glass, glass') ||> List.mapi2  (fun y r r' -> (r,r') ||> List.mapi2 (fun x v v' -> (x,y,v'<>0,(v=0)<>(v'=0))))
                                 |> List.fold   (@) []  
                                 |> List.filter (fun (_,_,_,f) -> f)
                                 |> List.sortBy (fun (_,_,v,_) -> v)
                                 |> List.iter   (fun (x,y,v,_) -> (if v then printAthom else clearAthom) x y)
            let print_next next next' = if next <> next' 
                                        then for t,f in [next,clearAthom;next',printAthom] do for x,y,_ in t do f (x+11) (y-2)
            let print_score score (score':int) = if score <> score'
                                                 then for x,y in [for x in [15..18] -> (x,10)] do clearAthom x y
                                                 Console.SetCursorPosition(15,10)
                                                 Console.Write ("{0:0000}", score')
            let rec readCommand () = match (Console.ReadKey true).Key with
                                     | ConsoleKey.DownArrow  -> Move Direction.Down
                                     | ConsoleKey.LeftArrow  -> Move Direction.Left
                                     | ConsoleKey.RightArrow -> Move Direction.Right
                                     | ConsoleKey.UpArrow    -> Transform
                                     | _                     -> Nil

            System.Threading.Thread.Sleep(15)
            if command = Nil && i<>0
            then go' glass next score (if i%15=0 then (Move Direction.Down) else command) (i+1)
            else match go glass next score command with
                 | None                       -> game_over ()
                 | Some (glass',next',score') -> print_glass glass glass'
                                                 print_next  next  next'
                                                 print_score score score'
                                                 let command' = if Console.KeyAvailable then readCommand () else Nil
                                                 go' glass' next' score' command' (i+1)
        Console.Clear ()
        for f in  [Console.SetWindowSize;Console.SetBufferSize] do f(32,23)
        Console.CursorVisible <- false
        Console.ForegroundColor <- ConsoleColor.DarkGray
        for s in [ "╔══════════╗"
                   "║          ║ ╔══════╗"
                   "║          ║ ║      ║"
                   "║          ║ ║      ║  <- NEXT"
                   "║          ║ ║      ║"
                   "║          ║ ║      ║"
                   "║          ║ ║      ║"
                   "║          ║ ║      ║"
                   "║          ║ ╠══════╣"
                   "║          ║ ║      ║"
                   "║          ║ ║ 0000 ║  <- SCORE"
                   "║          ║ ║      ║"
                   "║          ║ ╚══════╝"
                   "║          ║"
                   "║          ║"
                   "║          ║"
                   "║          ║"
                   "║          ║"
                   "║          ║"
                   "║          ║"
                   "║          ║"
                   "╚══════════╝" ] do Console.WriteLine s
        go' ((0 |> List.replicate 10) |> List.replicate 20)  [] 0 Nil 0 
    main ()
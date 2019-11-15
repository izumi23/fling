module G = Graphics
module D = Draw

(*allows the player to go back to the menu by pressing Esc*)
exception Menu

(*allows the player to go back of one move by pressing Backspace*)
exception Back

(* max width of the grid printed *)
let max_x = 15

(* max height of the grid printed *)
let max_y = 15

(* game is a reference to the initial game. *)
let game = ref (Rules.new_game [])

(* return the ball that the player wants to move *)
let rec get_ball game =
  let status = G.wait_next_event [G.Button_down; G.Key_pressed] in
  if status.G.keypressed then begin
    let k = Char.chr (Char.code status.G.key) in
    if k = '\027' then raise Menu
    else if k = '\b' then raise Back
    else get_ball game end
  else begin
    let (x,y) = (status.G.mouse_x,status.G.mouse_y) in
    let p = D.position_of_coord x y in
    if Rules.is_ball game p then
      begin
        let ball = Rules.ball_of_position game p in
        D.draw_ball ~select:true ball; (* to show which ball has been selected *)
        ball
      end
  else
    get_ball game (* the player has selected an empty cell *)
  end

(* convert the key pressed into a char and call the continuation k on it *)
let get_key_pressed k =
  let status = G.wait_next_event [G.Key_pressed] in
  let key = Char.code status.G.key in
  k (Char.chr key)

(* return the direction choosen by the player *)
let rec get_ball_direction () =
  let dir_of_char c =
    Rules.(
      match c with
      | '\027' -> raise Menu
      | 'z' -> Some Up
      | 's' -> Some Down
      | 'd' -> Some Right
      | 'q' -> Some Left
      | _ -> None
    )
  in
  get_key_pressed (fun c -> match dir_of_char c with
      | Some (x) -> x

      | None -> get_ball_direction () (* wrong key pressed by the player *)
    )

(* get the next move of the player *)
let get_next_move game =
  let p = get_ball game in
  let d = get_ball_direction () in
  Rules.make_move p d


(* create_game allows the player to create its own game by putting balls over the grid *)
let create_game () =
  D.ready false;
  D.draw_game max_x max_y (Rules.new_game []);
  let rec add_balls l =
    let status = G.wait_next_event [G.Button_down; G.Key_pressed] in
    if status.G.button = false then
      let k = Char.chr (Char.code status.G.key) in
      if k = 'e' then begin Draw.ready true; l end
      else if k = '\027' then raise Menu
      else add_balls l
    else
      let (x,y) = (status.G.mouse_x, status.G.mouse_y) in
      let p = D.position_of_coord x y in
      let (x',y') = Position.proj_x p, Position.proj_y p in
      (* balls can not be outside the grid *)
      if 0 <= x' && x' < max_x && 0 <= y' && y' < max_y then
        let ball = Rules.make_ball p in
        D.draw_ball ball;
        add_balls (ball::l)
      else
        add_balls l
  in
  let balls = add_balls [] in
  Rules.new_game balls


(*the files to load/save the games*)
let get_file c = match c with
    | '1' -> "Save1"
    | '2' -> "Save2"
    | '3' -> "Save3"
    | _ -> "None"

let game_of_matrix m =
  let balls = ref [] in
  for i = 0 to max_y-1 do
    for j = 0 to max_x-1 do
      if m.(i).(j) then
        balls := (Rules.make_ball (Position.from_int j i)) :: !balls
    done
  done;
  Rules.new_game !balls

(*reads a file to make a game*)
let game_of_config file =
  let m = Array.make_matrix max_y max_x false in
  let ic = open_in file in
  for i = max_y-1 downto 0 do
    let line = input_line ic in
    for j = 0 to max_x-1 do
      if line.[j] = 'O' then m.(i).(j) <- true
    done
  done;
  close_in ic;
  game_of_matrix m


let matrix_of_game game =
  let m = Array.make_matrix max_y max_x false in
  let mark_ball b =
    let x, y = Rules.coord b in m.(max_y-1-y).(x) <- true
  in
  List.iter mark_ball (Rules.get_balls game);
  m

(*saves the game in file*)
let config_of_game game file =
  let m = matrix_of_game game in
  let oc = open_out file in
  for i = 0 to max_y-1 do
    for j = 0 to max_x-1 do
      Printf.fprintf oc "%s" (if m.(i).(j) then "O" else ".")
    done;
    Printf.fprintf oc "%s" "\n"
  done;
  close_out oc


(* A menu is a pair of string * f where f is a function of type unit -> unit.
   If the player choose on the menu which function should be called *)
let rec menu = [("play new",play); ("solve new",solve); ("load",load); ("exit",leave)]

and menux = [("play new",play); ("solve new",solve); ("load",load); ("play loaded/last",replay); ("solve loaded/last",resolve); ("save",save); ("exit", leave)]

(* play allows the player to create a new game, and then try to solve it *)
and play () =
  game := create_game ();
  loop !game []

(* solve allows the player to create a new game and then see if the game can be solved *)
and solve () =
  game := create_game ();
  solver !game


(* loop game loops on the game while their is still moves possible for the player.
   previous keeps in store the previous states of the game*)
and loop game previous =
  try
    D.draw_game max_x max_y game;
    if Rules.moves game = [] then
      if List.length (Rules.get_balls game) = 1 then D.draw_string "You Won!"
      else D.draw_string "Game Over!";
    let move = get_next_move game in
    let game1 = Rules.apply_move game move in
    loop game1 (game :: previous)
  with Back -> begin
    match previous with
      | [] -> loop game []
      | g :: p -> loop g p
    end


(* solver game solve the game if it is possible *)
and solver game  =
  let moves = Solver.solve game in
  match moves with
  | None ->  D.draw_string "No solution!"; get_key_pressed (fun c -> main ())
  | Some moves ->

(* a loop to display the solution sequence move by move.
   Keeps the previous states of the game and the previous moves in prevg and prevm *)
    let rec loop_solve g moves prevg prevm =
      match moves with
      | [] -> g
      | m :: seq ->
        try
          D.draw_game max_x max_y g;
          let b, d = Rules.decompose m in
          D.draw_ball ~select:true ~dir:(Some d) b;
          D.draw_string "Solved!";
          get_key_pressed (fun c -> if c = '\027' then raise Menu; if c = '\b' then raise Back);
          let g1 = Rules.apply_move g m in
          loop_solve g1 seq (g::prevg) (m::prevm)
        with Back -> begin
          match prevg, prevm with
            | [], _ | _, [] -> loop_solve g moves [] []
            | g1 :: p, m1 :: q -> loop_solve g1 (m1::moves) p q
          end
    in
    D.draw_game max_x max_y (loop_solve game moves [] []);
    D.draw_string "Press any key to quit";
    get_key_pressed (fun c -> main ())


(* replay the previous game *)
and replay () =
  D.draw_game max_x max_y !game;
  D.ready true;
  loop !game []

(* resolve the preivous game *)
and resolve () =
  D.draw_game max_x max_y !game;
  D.ready true;
  solver !game

(* leave the application *)
and leave () =
  D.close_window()

(*load a game from a file to select*)
and load () =
  D.draw_string "Load from file: (1/2/3/Esc)";
  let file = get_key_pressed (get_file) in
  if file = "None" then main ()
  else begin
    (try
      game := game_of_config file; D.ready false
    with _ ->
      let l = if Rules.get_balls !game = [] then menu else menux in
      D.draw_menu l;
      D.draw_string "Reading error. Press any key";
      get_key_pressed (fun c -> ()));
    main ()
  end

(*save the current game to a file to select*)
and save () =
  D.draw_string "Save to file: (1/2/3/Esc)";
  let file = get_key_pressed (get_file) in
  if file = "None" then main ()
  else begin
    D.draw_menu menux;
    (try
      config_of_game !game file;
      D.draw_string (String.concat "" ["Game saved in '"; file; "'. Press any key"]);
      get_key_pressed (fun c -> ());
    with _ ->
      D.draw_string "Writing error. Press any key";
      get_key_pressed (fun c -> ()));
    main ()
  end

(* get the choice of the player *)
and main () =
  let l = if Rules.get_balls !game = [] then menu else menux in
  try
    let choice c =
      let i = (int_of_char c) - (int_of_char '0') in
      if 0 <= i && i < List.length l then
        snd (List.nth l i) ()
      else
        main ()
    in
    Random.self_init();
    D.init_window();
    D.draw_menu l;
    get_key_pressed choice
  with
    | Menu -> main ()
    | G.Graphic_failure("fatal I/O error") -> leave ()

let _ = main ()

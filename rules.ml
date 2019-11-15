(*Izumi Watanabe*)

type direction = Up | Right | Down | Left

type ball = Position.t * int

type move = ball * direction

type game = ball list

(*Keeps the index of the latest added ball in the game.
 *Stored separatedly in order to keep a clearer code, and
 *because its value is never used outside of the following function*)
let n = ref (-1)

let make_ball p = incr n; (p, !n)

let new_game ps = ps

let eq_ball b b' = snd b = snd b'

let make_move p d = (p, d)

let decompose m = m

let position_of_ball b = fst b

let get_balls g = g

let rec is_ball g p =
  match g with
  | [] -> false
  | b :: q when Position.eq p (position_of_ball b) -> true
  | b :: q -> is_ball q p

let rec ball_of_position game p =
  match game with
  | [] -> failwith "No ball at this position"
  | b :: q when Position.eq p (position_of_ball b) -> b
  | b :: q -> ball_of_position q p


let coord b =
   let p = position_of_ball b in
   Position.proj_x p, Position.proj_y p


(*possible_with (b,d) b' returns the distance between b and b' if b' is located
 *in the direction d from b, and None otherwise*)

let possible_with move b' =
  let b, d = move in
  let x, y = coord b in
  let x', y' = coord b' in
  match d with
    | Up when x = x' && y < y' -> Some (y'-y)
    | Down when x = x' && y' < y -> Some (y-y')
    | Left when x' < x && y = y' -> Some (x-x')
    | Right when x < x' && y = y' -> Some (x'-x)
    | _ -> None


(*possible (b,d) g returns a couple (poss,c) where:
 *c is the closest ball from b in the direction d if one exists, any ball otherwise
 *poss = 0 if c is undefined, 1 if c is at distance 1, 2 if c is at distance >= 2*)

let possible move g =
  let rec aux m c = function
    | [] -> (if m <> max_int then 2 else 0), c
    | b' :: q ->
      match possible_with move b' with
        | Some 1 -> 1, b'
        | Some i when i < m -> aux i b' q
        | _ -> aux m c q
  in
  aux max_int (Position.from_int 0 0, 0) (get_balls g)

let rec remove b = function
  | [] -> []
  | b' :: q when eq_ball b b' -> q
  | b' :: q -> b' :: (remove b q)

let rec replace b b' = function
  | [] -> []
  | c :: q when eq_ball b c -> b' :: q
  | c :: q -> c :: (replace b b' q)


(*applies move, and recursively the produced chain reaction if there is several balls in the row.
The moves that are not initial are possible even if the next ball is at distance 1*)

let apply_move g move =
  let game = get_balls g in
  let rec aux move b' game =
    let b, d = move in
    let p, k = b in
    let x, y = coord b and x', y' = coord b' in
    let nx, ny = match d with
      | Up -> x, y'-1
      | Down -> x, y'+1
      | Right -> x'-1, y
      | Left -> x'+1, y
    in
    let game = replace b ((Position.from_int nx ny), k) game in
    let poss, c = possible (b', d) game in
    if poss = 0 then remove b' game
    else aux (b', d) c game
  in
  let poss, b' = possible move game in
  if poss = 2 then new_game (aux move b') game else g


let moves g =
  let l = ref [] in
  let add b d =
    if fst (possible (b, d) g) = 2 then l := (b, d) :: !l
  in
  let rec aux = function
    | [] -> ()
    | b :: q -> List.iter (add b) [Up; Down; Left; Right] ; aux q
  in
  aux (get_balls g);
  !l

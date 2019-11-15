module G = Graphics

let width = 1024

let height = 1024

let line_height = 25

let padding_left = 50

let padding_right = 50

let padding_up = 50

let padding_down = 50

let margin = 5

let cell_size = ref 0

let colors_generated = ref false

let colors = ref []

let generate_new_color color =
  let from_rgb c =
    let r = c / (256 * 256) in
    let g = (c / 256) mod 256 in
    let b = c mod 256 in
    (r,g,b)
  in
  let mix i i' = (i + i') / 2 in
  let red = Random.int 256 in
  let green = Random.int 256 in
  let blue = Random.int 256 in
  let old_red, old_green, old_blue = from_rgb color in
  G.rgb (mix red old_red) (mix green old_green) (mix blue old_blue)

let init_window () =
  G.open_graph "";
  G.set_window_title "Fling";
  G.resize_window width height;
  G.clear_graph()

let close_window () =
  G.close_graph()


let draw_grid cols rows =
  G.set_color G.black;
  let cell_width = (width - padding_left - padding_right) / cols in
  let cell_height = (height - padding_up - padding_down) / rows in
  cell_size := min cell_width cell_height;
  let start_x, start_y = padding_left, padding_down in
  let end_x, end_y = start_x + cols * !cell_size, start_y + rows * !cell_size in
  G.moveto start_x start_y;
  for i = 0 to cols do
    G.lineto (G.current_x ()) end_y;
    G.moveto ((G.current_x ()) + !cell_size) start_y
  done;
  G.moveto padding_left padding_down;
  for i = 0 to rows do
    G.lineto end_x (G.current_y ());
    G.moveto start_x ((G.current_y ()) + !cell_size)
  done


(*if dir is some direction, draws an arrow starting from ball in the given direction*)
let draw_ball ?select:(select=false) ?dir:(dir=None) ball =
  let p = Rules.position_of_ball ball in
  let size = !cell_size in
  let x = padding_left + Position.proj_x p * size + (size / 2) in
  let y = padding_left + Position.proj_y p * size + (size / 2) in
  let radius = (size -margin) / 2 in
  begin
    if select then
      G.set_color G.red
    else
    if !colors_generated then
      begin
        let color = fst (List.find (fun cb -> Rules.eq_ball (snd cb) ball) !colors) in
        G.set_color color
      end
    else
      let color = generate_new_color G.white in
      colors := (color,ball)::!colors;
      G.set_color color
  end;
  if select then
    begin
      G.draw_circle x y radius;
      G.draw_circle x y (radius+1);
      G.draw_circle x y (radius+2)
    end
  else
    G.fill_circle x y radius;
  (*the coordinates of the arrow, given if it is horizontal (h) and/or
   *points towards the positive (e)*)
  let arrow x y h e =
    let coord = [| x, x, x-size/8, x+size/8 ;
    y+e*radius, y+e*(radius+size/2), y+e*(radius+3*size/8), y+e*(radius+3*size/8) |] in
    coord.(h), coord.(1-h)
  in
  match dir with None -> () | Some dr ->
    let (xa, xb, xc, xd), (ya, yb, yc, yd) = match dr with
      | Rules.Up -> arrow x y 0 1
      | Rules.Down -> arrow x y 0 (-1)
      | Rules.Right -> arrow y x 1 1
      | Rules.Left -> arrow y x 1 (-1)
    in
    let draw_line (x1, y1, x2, y2) = G.moveto x1 y1; G.lineto x2 y2 in
    G.set_line_width 3;
    List.iter draw_line [(xa,ya,xb,yb); (xb,yb,xc,yc); (xb,yb,xd,yd)];
    G.set_line_width 1

let draw_balls balls =
  List.iter draw_ball balls

let draw_string s =
  G.moveto (width/2) (height-padding_up);
  G.set_color G.red;
  G.draw_string s

let draw_game cols rows game =
  G.clear_graph ();
  draw_grid cols rows;
  draw_balls (Rules.get_balls game)

let position_of_coord x y =
  let size = !cell_size in
  let x', y' = x - padding_left, y - padding_down in
  Position.from_int (x'/size) (y'/size)

let draw_menu l =
  G.clear_graph();
  G.set_line_width 1;
  G.set_color G.black;
  G.moveto (width/2) (5*height/8);
  G.draw_string "Fling";
  G.set_text_size 20;
  let (x,y) = (width/2, height/2) in
  G.moveto x y;
  ignore @@ List.fold_left (fun (i,y) (name,_) -> G.draw_string (Printf.sprintf "%d : %s" i name);
                             let y' = y - line_height in
                             G.moveto x y'; (i+1,y')) (0,y) l

let ready b = colors_generated := b

(*Procedes with a depth-first search in a "graph", where the vertices are the game states and
 *the edges are the possible moves. The purpose is to find a game of cardinality 1. *)


let solve game =
  let rec search_moves game seq = function
    | [] -> None
    | m :: q ->
      let game1 = Rules.apply_move game m in
      match aux game1 seq with
      | None ->  search_moves game seq q
      | Some seq ->  Some (m :: seq)
  and aux game seq =
    let balls = Rules.get_balls game in
    if List.length balls = 1 then Some seq
    else let mov = Rules.moves game in search_moves game seq mov
  in
  aux game []

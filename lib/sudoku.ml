open Yojson.Basic.Util
module IntSet = Set.Make (Int)

type tile = Empty of IntSet.t | Filled of int
type t = { tiles : tile array }

let dim = 9
let block_size = dim / 3
let grid_size = dim * dim

let parse filename =
  let parse_fixed_size_array json key expected_size =
    let to_tile x =
      let v = to_int x in
      match v with
      | 0 -> Empty (List.init dim (fun i -> i + 1) |> IntSet.of_list)
      | y ->
          if 1 <= y && y <= 9 then Filled y
          else
            failwith
              (Printf.sprintf "Expected value in range [1,9], but got %d" y)
    in
    let json_array = json |> member key |> to_list |> List.map to_tile in
    if List.length json_array <> expected_size then
      failwith
        (Printf.sprintf "Expected array of size %d, but got %d" expected_size
           (List.length json_array))
    else Array.of_list json_array
  in
  let ic = open_in filename in
  let json = Yojson.Basic.from_channel ic in
  close_in ic;
  { tiles = parse_fixed_size_array json "tiles" grid_size }

let copy_tile tile =
  match tile with
  | Empty set -> Empty (IntSet.to_list set |> IntSet.of_list)
  | Filled value -> Filled value

let copy_sudoku sudoku = { tiles = Array.map copy_tile sudoku.tiles }

let get_column sudoku x =
  Array.init dim (fun i -> sudoku.tiles.((i * dim) + x)) |> Array.to_list

let get_row sudoku y = Array.sub sudoku.tiles (y * dim) dim |> Array.to_list

let get_block sudoku x y =
  (* nx and ny are the top left most tile in the block *)
  let nx = x / block_size * block_size in
  let ny = y / block_size * block_size in
  let r1 =
    Array.sub sudoku.tiles ((ny * dim) + nx) block_size |> Array.to_list
  in
  let r2 =
    Array.sub sudoku.tiles (((ny + 1) * dim) + nx) block_size |> Array.to_list
  in
  let r3 =
    Array.sub sudoku.tiles (((ny + 2) * dim) + nx) block_size |> Array.to_list
  in
  List.concat [ r1; r2; r3 ]

let pos_to_coord pos = (pos mod dim, pos / dim)

let find_option sudoku pos =
  let x, y = pos_to_coord pos in
  let t = Array.get sudoku.tiles ((y * dim) + x) in
  match t with
  | Empty options ->
      let col = get_column sudoku x in
      let row = get_row sudoku y in
      let block = get_block sudoku x y in
      let seen =
        List.concat [ col; row; block ]
        |> List.map (fun t -> match t with Empty _ -> 0 | Filled v -> v)
      in
      List.fold_left (fun acc x -> IntSet.remove x acc) options seen
  | Filled _ -> IntSet.empty

let find_all_options sudoku =
  Array.to_list sudoku.tiles
  |> List.mapi (fun i _ ->
         let opt = find_option sudoku i |> IntSet.to_list in
         (i, opt))
  |> List.sort (fun a b ->
         compare (snd a |> List.length) (snd b |> List.length))
  |> List.filter (fun (_, l) -> List.length l > 0)

let build_sudoku_str sudoku =
  let rec repeat_str s n = if n <= 0 then "" else s ^ repeat_str s (n - 1) in
  let n = Array.length sudoku.tiles in
  let sep = "\n" ^ repeat_str "-" ((2 * (dim + 2)) - 1) ^ "\n" in
  let buf = Buffer.create 256 in
  Array.iteri
    (fun i x ->
      Buffer.add_string buf
        (Printf.sprintf "%s "
           (match x with Empty _ -> "*" | Filled y -> string_of_int y));
      if (i + 1) mod (block_size * dim) = 0 && i + 1 <> n then
        Buffer.add_string buf sep
      else if (i + 1) mod dim = 0 then Buffer.add_char buf '\n'
      else if (i + 1) mod block_size = 0 then Buffer.add_string buf "| ")
    sudoku.tiles;
  Buffer.contents buf

let print sudoku =
  let s = build_sudoku_str sudoku in
  Printf.printf "%s" s

let build_all_options_str sudoku =
  let options = find_all_options sudoku in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "Options:\n";
  List.iter
    (fun (p, opts) ->
      let x, y = pos_to_coord p in
      Buffer.add_string buf (Printf.sprintf "(%d, %d): " x y);
      List.iter
        (fun opt -> Buffer.add_string buf (Printf.sprintf "%d " opt))
        opts;
      Buffer.add_char buf '\n')
    options;
  Buffer.contents buf

let print_all_options sudoku =
  let s = build_all_options_str sudoku in
  Printf.printf "%s\n" s

let fill_pos sudoku pos v = sudoku.tiles.(pos) <- Filled v

let equal s1 s2 =
  Array.length s1.tiles = Array.length s2.tiles
  && Array.for_all2 ( = ) s1.tiles s2.tiles

let filled_in sudoku =
  Array.for_all
    (fun t -> match t with Empty _ -> false | Filled _ -> true)
    sudoku.tiles

let rec solve sudoku =
  Logs.debug (fun m -> m "\n%s" (build_sudoku_str sudoku));
  Logs.debug (fun m -> m "\n%s" (build_all_options_str sudoku));
  let options = find_all_options sudoku in
  if List.length options = 0 && filled_in sudoku then (true, sudoku)
  else
    let singles, multiples =
      List.partition (fun (_, opts) -> List.length opts = 1) options
    in
    if List.length singles > 0 then (
      let p, o = List.hd singles in
      fill_pos sudoku p (List.hd o);
      solve sudoku)
    else if List.length multiples > 0 then
      let p, opts = List.hd multiples in
      solve_backtrack sudoku p opts
    else (false, sudoku)

and solve_backtrack sudoku pos options =
  match options with
  | [] -> (false, sudoku)
  | v :: rst ->
      let cpy = copy_sudoku sudoku in
      fill_pos cpy pos v;
      let res, filled_sudoku = solve cpy in
      if res then (res, filled_sudoku) else solve_backtrack sudoku pos rst

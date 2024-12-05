(* open Sudoku *)

let replace_substr ~old_substr ~new_substr str =
  let re = Str.regexp old_substr in
  Str.global_replace re new_substr str

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let sol_filename =
      replace_substr ~old_substr:"problem" ~new_substr:"solution" filename
    in
    let puzzle = Sudoku.parse_sudoku filename in
    let solution = Sudoku.parse_sudoku sol_filename in
    (* Sudoku.print_puzzle puzzle;
    Sudoku.print_all_options puzzle; *)
    let res, solved = Sudoku.solve puzzle in
    Printf.printf "Puzzle solved %b\n" res;
    (* Sudoku.print_puzzle solved;
    Sudoku.print_all_options solved; *)

    assert (Sudoku.equal solved solution)

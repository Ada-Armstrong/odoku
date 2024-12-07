(* A program that will solve a Sudoku using backtracking. *)

let setup_log () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info)

let () =
  setup_log ();
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let puzzle = Sudoku.parse filename in
    Sudoku.print puzzle;
    let res, solved = Sudoku.solve puzzle in
    Printf.printf "Puzzle solved %b\n" res;
    Sudoku.print solved

let tests = ref []
let add_test name f = tests := (name, f) :: !tests

let run_tests () =
  let passed = ref 0 in
  let failed = ref 0 in
  List.iter
    (fun (name, test) ->
      Printf.printf "Running test: %s ... " name;
      try
        test ();
        incr passed;
        Printf.printf "PASSED\n"
      with ex ->
        incr failed;
        Printf.printf "FAILED (%s)\n" (Printexc.to_string ex))
    (List.rev !tests);
  Printf.printf "\nTotal: %d, Passed: %d, Failed: %d\n" (List.length !tests)
    !passed !failed;
  !failed

let test_solve filename =
  let replace_substr ~old_substr ~new_substr str =
    let re = Str.regexp old_substr in
    Str.global_replace re new_substr str
  in
  let sol_filename =
    replace_substr ~old_substr:"problem" ~new_substr:"solution" filename
  in
  let puzzle = Sudoku.parse_sudoku filename in
  let solution = Sudoku.parse_sudoku sol_filename in
  let res, solved = Sudoku.solve puzzle in
  assert res;
  assert (Sudoku.equal solved solution)

let build_solve_test filename =
  let puzzle_dir = "../../../puzzles" in
  Filename.concat puzzle_dir filename |> test_solve

let () =
  add_test "Easy" (fun () -> build_solve_test "easy-problem.json");
  add_test "Medium" (fun () -> build_solve_test "medium-problem.json");
  add_test "Hard" (fun () -> build_solve_test "hard-problem.json");

  exit (run_tests ())

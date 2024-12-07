# ODoku
A Sudoku solver written in OCaml.

## Setup
After cloning the repo, run the following in a terminal in the root of the project directory.
```bash
opam switch create . --deps-only --locked
dune build
```

## Run
To run the executable use the following command.
See the [file format](#file-format) section for details on the input file.
```bash
dune exec odoku INPUT_FILEPATH
```

## File Format
The input file is expected to be a JSON object with a "tiles" key, the corresponding value should be a flat integer array of length 81 (9 rows X 9 columns).
The value 0 represents an empty tile while the digits 1-9 correspond to a filled tile of that value.
See the _puzzles_ directory for examples.

```json
{
    "tiles": [
        5, 3, 0, 0, 7, 0, 0, 0, 0,
        6, 0, 0, 1, 9, 5, 0, 0, 0,
        0, 9, 8, 0, 0, 0, 0, 6, 0,
        8, 0, 0, 0, 6, 0, 0, 0, 3,
        4, 0, 0, 8, 0, 3, 0, 0, 1,
        7, 0, 0, 0, 2, 0, 0, 0, 6,
        0, 6, 0, 0, 0, 0, 2, 8, 0,
        0, 0, 0, 4, 1, 9, 0, 0, 5,
        0, 0, 0, 0, 8, 0, 0, 7, 9
    ]
}
```

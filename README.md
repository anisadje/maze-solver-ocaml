# Maze

> Second-year project (L2 Computer Science), Université Paris-Saclay, 2025–2026.

## Members
DJEDJE Anîsa
CHEKROUN Massylia

## Description
An OCaml project to **load, randomly generate, display and solve mazes** stored as ASCII files. It uses graph traversal (BFS), recursion, functional programming and a modular structure.

```
$ ./maze.exe solve test/maze_6x6.laby
Path found:
+-+-+-+-+-+-+
|S..|    ...|
+ +.+-+-+.+.+
| |.......|.|
+ +-+-+-+-+.+
|   |     |.|
+ + + +-+-+.+
| | |.......|
+-+ +.+-+-+-+
|   |......E|
+ +-+-+-+-+-+
|           |
+-+-+-+-+-+-+
```

## Project structure
| File | Role |
|---|---|
| `grid.ml` / `grid.mli` | Reading, checking and printing a maze |
| `solver.ml` | Shortest path from `S` to `E` (BFS) |
| `generator.ml` | Random maze generation |
| `maze.ml` | Command line interface |
| `test/` | Example mazes and tests |

## Build
The project uses `dune`. From the root of the project:
```bash
dune build
```

## Usage
```bash
./maze.exe print  test/maze_6x6.laby    # check and print a maze
./maze.exe dump   test/maze_6x6.laby    # print the raw file
./maze.exe solve  test/maze_6x6.laby    # print the shortest path from S to E
./maze.exe stats  test/maze_6x6.laby    # size, number of walls and passages
./maze.exe random 5 8 0.7               # random maze: rows, columns, density
```

## Tests
```bash
dune test
```
The tests (`test/maze.t`) solve the example mazes, check that invalid mazes are rejected (open border, missing exit, wrong character, non rectangular grid) and that generated mazes are valid.

## File format
```
+-+-+
|  S|
+ + +
| | |
+ +-+
|E  |
+-+-+
```
`+`, `-` and `|` are walls, spaces are free cells, `S` is the start and `E` the exit. The maze must be rectangular and closed on its borders.
# maze-solver-ocaml

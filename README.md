
<!-- README.md is generated from README.Rmd. Please edit that file -->
puzzlr
======

The `puzzlr` package implements various puzzles. My hope is that this will come in handy for demonstrating or learning about different algorithms for solving the puzzles. So far, there are functions for creating and working with square tile-puzzles (see below) as well as instances of the knapsack problem (see `vignette("knapsack-branch-bound", package = "puzzlr")`, or this [lengthy writeup](https://tarakc02.github.io/branch-and-bound/))

Installation
------------

You can install puzzlr from github with:

``` r
# install.packages("devtools")
devtools::install_github("tarakc02/puzzlr")
```

Generating puzzles
------------------

``` r
library(puzzlr)
```

You can create a puzzle manually:

``` r
# manually specify:
puzzle(c(
    4,6,3,
    7,2,0,
    1,8,5
))
#> 4 . 6 . 3
#> 7 . 2 .  
#> 1 . 8 . 5
```

Or generate a random puzzle:

``` r
random_puzzle(size = 4)
#> 08 . 03 . 10 . 12
#> 14 . 15 . 01 . 11
#> 07 . 06 . 05 . 09
#> 04 . 02 .    . 13
```

Puzzle Methods
--------------

To visualize a puzzle:

``` r
set.seed(5476)
demo <- random_puzzle(size = 5)
plot(demo)
```

![](README/README-figunnamed-chunk-5-1.png)

To move a tile, specify the source and the destination:

``` r
# move the "13" down:
after_move <- move(demo, source = c(2, 4), dest = c(3, 4))
after_move
#> 15 . 23 . 18 . 17 . 09
#> 02 . 22 . 12 .    . 03
#> 16 . 21 . 08 . 13 . 11
#> 14 . 20 . 01 . 05 . 06
#> 24 . 19 . 10 . 07 . 04
```

Puzzles "remember" their move history by pointing to their "parent" -- that is, the puzzle state from which they came (the parent of a newly generated puzzle is `NULL`):

``` r
# number of moves made so far:
moves(after_move)
#> [1] 1

# the previous state:
parent(after_move)
#> 15 . 23 . 18 . 17 . 09
#> 02 . 22 . 12 . 13 . 03
#> 16 . 21 . 08 .    . 11
#> 14 . 20 . 01 . 05 . 06
#> 24 . 19 . 10 . 07 . 04
```

A state reachable from a given puzzle state is called a "neighbor" of that puzzle state, and the `neighbors` function generates all neighbors:

``` r
neighbors(demo)
#> [[1]]
#> 15 . 23 . 18 . 17 . 09
#> 02 . 22 . 12 .    . 03
#> 16 . 21 . 08 . 13 . 11
#> 14 . 20 . 01 . 05 . 06
#> 24 . 19 . 10 . 07 . 04
#> 
#> [[2]]
#> 15 . 23 . 18 . 17 . 09
#> 02 . 22 . 12 . 13 . 03
#> 16 . 21 . 08 . 05 . 11
#> 14 . 20 . 01 .    . 06
#> 24 . 19 . 10 . 07 . 04
#> 
#> [[3]]
#> 15 . 23 . 18 . 17 . 09
#> 02 . 22 . 12 . 13 . 03
#> 16 . 21 .    . 08 . 11
#> 14 . 20 . 01 . 05 . 06
#> 24 . 19 . 10 . 07 . 04
#> 
#> [[4]]
#> 15 . 23 . 18 . 17 . 09
#> 02 . 22 . 12 . 13 . 03
#> 16 . 21 . 08 . 11 .   
#> 14 . 20 . 01 . 05 . 06
#> 24 . 19 . 10 . 07 . 04
```

For convenience, two distance metrics have been implemented, the `hamming` and `manhattan` distance. The `manhattan` distance is the sum of each non-zero tile's manhattan distance to its goal position, and the `hamming` distance is the sum of each non-zero tile's hamming distance to its goal position. These can both be used as lower bounds on the number of moves required to solve the puzzle.

``` r
manhattan(demo)
#> [1] 85
hamming(demo)
#> [1] 24
```

Solving puzzles
---------------

``` r
set.seed(46654)
mypuzzle <- random_puzzle(3)
is_solvable(mypuzzle)
#> [1] TRUE
```

As stated above, the hope for this package is for it to serve as an easy-to-use framework against which to experiment with solver algorithms. As an example, I'll show how to solve a puzzle using [lazily evaluated lists](https://github.com/tarakc02/lazylist).

The idea is to generate every state that is reachable from the starting puzzle. Recall that every move keeps track of the history of moves that got from the original puzzle to the current state. So once I've generated every single reachable state, I just search through and find one that is in the goal state. Of course, the big problem is that there will be far too many such states. So I make sure that I evaluate them in order, where I order them by the number of moves needed to reach the state plus the manhattan distance from the goal state. Then I just scan one-by-one until I reach a solved state -- because of the way I'm ordering things, this will also represent the fastest solution.

``` r
library(lazylist)
reachable_states <- function(pz) {
    # to generate all reachable states, i first look at
    # all states reachable in 1 move
    possible_moves <- neighbors(pz)
    
    # then i generate a stream of further moves from each of those states
    stream_of_moves <- function(x) cons_stream(x, reachable_states(x))
    streams <- purrr::map(possible_moves, stream_of_moves)
    
    # finally, i interleave the streams of future moves, 
    # ordering in such a way that i'm guaranteed to stumble into
    # the shortest solution:
    purrr::reduce(streams, 
                  merge_weighted, 
                  weight = function(x) moves(x) + manhattan(x))
}

mypuzzle_moves <- reachable_states(mypuzzle)

# if a set of moves results in manhattan (or hamming) distance of 0 
# that means it represents a valid solution to the puzzle
valid_solutions <- stream_filter(mypuzzle_moves,
                                 function(x) manhattan(x) == 0)

# the first solution in the stream will be the one that required the 
# fewest moves:
mysolution <- valid_solutions[1]
```

To view how many moves were required by a given solution:

``` r
moves(mysolution)
#> [1] 19
```

You can animate the results:

``` r
animate_moves(mysolution)
```

![](README/README-fig-animated-solution.gif)

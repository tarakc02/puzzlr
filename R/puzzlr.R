#' @export
puzzle <- function(nums = sample(0:8), parent = NULL, moves = 0L) {
    n <- length(nums)
    force(parent)
    rank <- sqrt(length(nums))
    if (rank != as.integer(rank))
        stop("Puzzle must be a square")
    rank <- as.integer(rank)
    if (identical(dim(nums), c(rank, rank)))
        res <- nums
    else res <- matrix(nums, nrow = rank, ncol = rank, byrow = TRUE)

    actual <- vapply(
        seq_len(n - 1),
        function(x) which(res == x),
        FUN.VALUE = integer(1))

    actual <- ind_to_coord(res, actual)
    actual <- matrix(actual, ncol = 2)

    goal <- goal_index(res, seq_len(n - 1))
    goal <- matrix(goal, ncol = 2)
    manhattan <- sum(abs(actual - goal))
    hamming <- sum((actual[,1] != goal[,1]) + (actual[,2] != goal[,2]) > 0)

    structure(res,
              manhattan = manhattan,
              hamming = hamming,
              parent = function() parent,
              moves = moves,
              class = "puzzlr")
}

#' @export
manhattan <- function(pz) UseMethod("manhattan")

#' @export
hamming <- function(pz) UseMethod("hamming")

#' @export
moves <- function(pz) UseMethod("moves")

#' @export
parent <- function(pz) UseMethod("parent")

#' @export
move <- function(pz, source, dest) UseMethod("move")

#' @export
neighbors <- function(pz) UseMethod("neighbors")

#' @export
is_solvable <- function(pz) UseMethod("is_solvable")

#' @export
manhattan.puzzlr <- function(pz) attr(pz, "manhattan")

#' @export
hamming.puzzlr <- function(pz) attr(pz, "hamming")

#' @export
moves.puzzlr <- function(pz) attr(pz, "moves")

#' @export
parent.puzzlr <- function(pz) attr(pz, "parent")()

blank <- function(pz) {
    which(pz == 0)
}

#' @export
move.puzzlr <- function(pz, source, dest) {
    res <- pz
    res[dest[1], dest[2]] <- res[source[1], source[2]]
    res[source[1], source[2]] <- 0L
    puzzle(res, parent = pz, moves(pz) + 1L)
}

equivalent <- function(p1, p2) {
    if (is.null(p1) && is.null(p2)) stop("Comparing nothing")
    if (is.null(p1)) return(FALSE)
    if (is.null(p2)) return(FALSE)
    all(p1 == p2)
}

#' @export
neighbors.puzzlr <- function(pz) {
    blank_tile <- ind_to_coord(pz, blank(pz))
    row <- blank_tile[1]
    col <- blank_tile[2]
    dim <- nrow(pz)
    nbrs <- vector("list", 4L)

    if (row > 1) {
        candidate <- move(pz, c(row - 1L, col), blank_tile)
        if (!equivalent(candidate, parent(pz)))
            nbrs[[1]] <- candidate
    }

    if (row < dim) {
        candidate <- move(pz, c(row + 1L, col), blank_tile)
        if (!equivalent(candidate, parent(pz)))
            nbrs[[2]] <- candidate
    }

    if (col > 1) {
        candidate <- move(pz, c(row, col - 1L), blank_tile)
        if (!equivalent(candidate, parent(pz)))
            nbrs[[3]] <- candidate
    }

    if (col < dim) {
        candidate <- move(pz, c(row, col + 1L), blank_tile)
        if (!equivalent(candidate, parent(pz)))
            nbrs[[4]] <- candidate
    }
    purrr::compact(nbrs)
}

#' @export
is_solvable.puzzlr <- function(pz) {
    ## check solvability:
    tester <- c(t(pz))
    tester <- tester[tester > 0]
    l <- length(tester)
    inversions <- 0
    for (ind in seq_len(l - 1)) {
        comp <- tester[seq(ind + 1L, l)]
        comp <- comp[comp > 0]
        inversions <- inversions + sum(tester[ind] > comp)
    }
    rank <- nrow(pz)

    if (is_odd(rank)) {
        solvable <- is_even(inversions)
    } else {
        blank_tile <- ind_to_coord(pz, blank(pz))
        blank_row <- blank_tile[1]
        blank_from_bottom <- rank - blank_row + 1
        if (is_even(blank_from_bottom)) solvable <- is_odd(inversions)
        else solvable <- is_even(inversions)
    }
    solvable
}


#' @export
random_puzzle <- function(size) {
    n <- size**2
    pz <- puzzle(sample(seq(0, n - 1), replace = FALSE))
    if (is_solvable(pz)) return(pz)
    else return(random_puzzle(size))
}

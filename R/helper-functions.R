is_odd <- function(x) x %% 2 > 0
is_even <- function(x) !is_odd(x)

ind_to_coord <- function(pz, ind) {
    n <- dim(pz)[1]
    c(1, 1) + c((ind - 1) %% n, (ind - 1) %/% n)
}

coord_to_ind <- function(pz, coord) {
    n <- dim(pz)[1]
    ((coord[2] - 1) * n) + coord[1]
}

goal_index <- function(pz, num) {
    n <- dim(pz)[1]
    c(1, 1) +
        c((num - 1L) %/% n,
          (num - 1L) %% n)
}


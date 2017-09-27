#' @export
replay_moves <- function(sol) {

    # add 1 to display the original
    display_len <- moves(sol) + 1L

    res <- vector("list", length = display_len)

    ind <- display_len
    while (!is.null(sol)) {
        res[[ind]] <- sol
        ind <- ind - 1L
        sol <- parent(sol)
    }
    res
}


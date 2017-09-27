#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

darkcolor <- "#2d425f"
lightcolor <- "#f3a950"

#' @export
print.puzzlr <- function(x, ...) {
    dim <- nrow(x)
    blockwidth <- nchar(max(x))

    stringx <- stringr::str_pad(x, blockwidth, pad = "0")
    dim(stringx) <- c(dim, dim)

    blank_tile <- paste(rep("@", blockwidth), collapse = "")

    for (i in seq_len(dim)) {
        for (j in seq_len(dim)) {
            if (x[i,j] == 0) cat(blank_tile) else cat(stringx[i,j])
            if (j < dim) cat(" . ")
        }
        cat("\n")
    }
    invisible(x)
}

#' @export
as.data.frame.puzzlr <- function(pz) {
    columns <- apply(pz, 2, as.list) %>% lapply(as.integer)
    colnames <- letters[seq_len(length(columns))]
    columns <- structure(columns, names = colnames)
    res <- tibble::as_data_frame(columns)
    res <- dplyr::mutate(res, row = seq(nrow(res)))
    res <- tidyr::gather(res, col, value, -row)
    res <- dplyr::select(res, row, col, value)
    res <- dplyr::mutate(res, iszero = value == 0)
    res
}

#' @export
plot.puzzlr <- function(pz) {
    pzdf <- as.data.frame(pz)
    pzdf$value[pzdf$value == 0] <- ""
    tile_text_size <- 20
    if (nrow(pz) > 3) tile_text_size <- 15
    if (nrow(pz) > 4) tile_text_size <- 10
    if (nrow(pz) > 7) tile_text_size <- 8

    ggplot2::ggplot(pzdf, ggplot2::aes(x = col, y = -row)) +
        ggplot2::geom_tile(ggplot2::aes(fill = iszero), colour = lightcolor) +
        ggplot2::scale_fill_manual(values = c(`TRUE` = lightcolor,
                                              `FALSE` = darkcolor),
                                   guide = "none") +
        ggplot2::scale_x_discrete(expand = c(0,0)) +
        ggplot2::geom_text(ggplot2::aes(label = value), size = tile_text_size,
                           colour = lightcolor) +
        ggplot2::coord_equal() +
        ggplot2::theme(line = ggplot2::element_blank(),
                       rect = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       axis.title = ggplot2::element_blank())
}

#' @export
animate_moves <- function(solution, file = NULL) {
    all_moves <- purrr::map(replay_moves(solution), as.data.frame)
    movewidth <- nchar(moves(solution))

    tile_text_size <- 20
    if (nrow(solution) > 3) tile_text_size <- 15
    if (nrow(solution) > 4) tile_text_size <- 10
    if (nrow(solution) > 7) tile_text_size <- 8

    allframes <- purrr::imap(
        all_moves,
        ~dplyr::mutate(
            .x,
            frame = paste0("After ",
                           stringr::str_pad(
                               .y - 1,
                               width = movewidth,
                               side = "left",
                               pad = "0"),
                           " moves"))) %>%
        dplyr::bind_rows()

    toplot <- allframes %>%
        dplyr::mutate(value = ifelse(value == 0, "", value)) %>%
        ggplot2::ggplot(ggplot2::aes(x = col, y = -row, frame = frame)) +
        ggplot2::geom_tile(ggplot2::aes(fill = iszero), colour = lightcolor) +
        ggplot2::scale_fill_manual(
            values = c(`TRUE` = lightcolor, `FALSE` = darkcolor),
            guide = "none") +
        ggplot2::scale_x_discrete(expand = c(0,0)) +
        ggplot2::geom_text(ggplot2::aes(label = value), size = tile_text_size,
                           colour = lightcolor) +
        ggplot2::coord_equal() +
        ggplot2::theme(line = ggplot2::element_blank(),
                       rect = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       axis.title = ggplot2::element_blank(),
                       title = ggplot2::element_text(
                           hjust = 0,
                           size = 25,
                           margin = ggplot2::margin()),
                       plot.margin = ggplot2::margin(30, 30, 30, 30))

    res <- gganimate::gganimate(toplot, filename = file, saver = "gif")
    res
}

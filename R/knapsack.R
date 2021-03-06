#' @export
knapsack <- function(capacity, items, density_order = TRUE,
                     drop_heavy_items = TRUE) {
    if (!inherits(items, "data.frame"))
        stop("items must be a data frame")
    if (!setequal(names(items), c("id", "weight", "value")))
        stop("items must be a data frame with columns: id, weight, value")
    if (drop_heavy_items) items <- items[items$weight <= capacity, , drop = FALSE]
    structure(
        list(capacity = capacity,
             items = stackify(items, density_order = density_order),
             value = 0L,
             taken = stack()),
        class = "knapsack"
    )
}

#' @export
capacity <- function(ks) UseMethod("capacity")
#' @export
capacity.knapsack <- function(ks) ks$capacity

#' @export
items <- function(ks) UseMethod("items")
#' @export
items.knapsack <- function(ks) item_df(ks)

#' @export
next_item <- function(ks) UseMethod("next_item")
#' @export
next_item.knapsack <- function(ks) ks$items("top")

#' @export
n_items <- function(ks) UseMethod("n_items")
#' @export
n_items.knapsack <- function(ks) ks$items("size")

#' @export
total_value <- function(ks) UseMethod("total_value")
#' @export
total_value.knapsack <- function(ks) ks$value

#' @export
take_next <- function(ks) UseMethod("take_next")
#' @export
leave_next <- function(ks) UseMethod("leave_next")
#' @export
take_next.knapsack <- function(ks) next_knapsack(ks, take = TRUE)
#' @export
leave_next.knapsack <- function(ks) next_knapsack(ks, take = FALSE)

#' @export
taken_items <- function(ks) UseMethod("taken_items")
#' @export
taken_items.knapsack <- function(ks) {
    lst <- stack_to_list(ks$taken)
    lst <- purrr::map(lst, tibble::as_data_frame)
    dplyr::bind_rows(lst)
}

#' @export
take_items <- function(ks, ids) UseMethod("take_items")
#' @export
take_items.knapsack <- function(ks, ids) {
    while(ks$items("size") > 0)
        ks <- if (next_item(ks)$id %in% ids) take_next(ks) else leave_next(ks)
    ks
}

#' @export
print.knapsack <- function(x, ...) {
    its <- x$items
    printlen <- pmin(6L, its("size"))

    cat("knapsack\n")
    cat("capacity:", capacity(x), "\n")
    cat("   items:", its("size"), "\n")
    cat("..id..", "weight", "value", "\n")
    while(printlen > 0L) {
        item <- its("top")
        cat(format(item$id, scientific = FALSE, width = 6),
            format(item$weight, scientific = FALSE, width = 7),
            format(item$value, scientific = FALSE, width = 6), "\n", sep = "")
        printlen <- printlen - 1L
        its <- its("without_top")
    }
    if (its("size") > 0) cat("   ...")
    cat("\nTaken items:", x$taken("size"), "\n")
    cat("Total value:", x$value)
    invisible(x)
}

next_knapsack <- function(kss, take) {
    if (kss$items("size") <= 0L) return(NULL)
    current_item <- kss$items("top")

    if (take) return(structure(list(
        capacity = kss$capacity - current_item$weight,
        items = kss$items("without_top"),
        value = kss$value + current_item$value,
        taken = push(kss$taken, current_item)
    ), class = "knapsack"))

    structure(list(capacity = kss$capacity,
                   items = kss$items("without_top"),
                   value = kss$value,
                   taken = kss$taken),
              class = "knapsack")
}

#' @export
sub_problems <- function(ks) {
    remaining_items <- ks$items
    if (remaining_items("size") <= 0L) return(list())

    remaining_capacity <- ks$capacity
    next_item <- remaining_items("top")
    if (next_item$weight <= remaining_capacity) return(
        list(next_knapsack(ks, take = TRUE),
             next_knapsack(ks, take = FALSE))
    )
    list(next_knapsack(ks, take = FALSE))
}

item_df <- function(ks) {
    is <- ks$items
    id <- vector("integer", is("size"))
    weight <- vector("integer", is("size"))
    value <- vector("integer", is("size"))

    row <- 1L
    while (is("size") > 0) {
        item <- is("top")
        id[row] <- item$id
        weight[row] <- item$weight
        value[row] <- item$value
        is <- is("without_top")
        row <- row + 1L
    }

    structure(
        data.frame(id = id, weight = weight, value = value),
        class = c("tbl_df", "tbl", "data.frame")
    )
}

# random knapsack stuff from:
# https://pdfs.semanticscholar.org/ebd4/646ce0bd4185ea67b5f3dd265686284adb3a.pdf

#' @export
uncorrelated_instance <- function(n = 10, R = 1000) {
    weight <- sample(R, size = n, replace = TRUE)
    capacity <- round(runif(1) * sum(weight))
    value <- sample(R, size = n, replace = TRUE)
    knapsack(capacity = capacity,
             items = data.frame(
                 id = seq_len(n),
                 weight = weight,
                 value = value
             ))
}

#' @export
weakly_correlated_instance <- function(n = 10, R = 1000) {
    weight <- sample(R, size = n, replace = TRUE)
    capacity <- round(runif(1) * sum(weight))

    min_vals <- pmax(weight - (R/10), 1)
    max_vals <- weight + (R / 10)

    addon <- sample((2 * (R / 10)) - 1, size = n, replace = TRUE)
    value <- pmin(min_vals + addon, max_vals)

    knapsack(capacity = capacity,
             items = data.frame(
                 id = seq_len(n),
                 weight = weight,
                 value = value
             ))
}

#' @export
strongly_correlated_instance <- function(n = 10, R = 1000) {
    weight <- sample(R, size = n, replace = TRUE)
    capacity <- round(runif(1) * sum(weight))
    value <- weight + (R / 10)
    knapsack(capacity = capacity,
             items = data.frame(
                 id = seq_len(n),
                 weight = weight,
                 value = value
             ))
}

#' @export
inverse_strongly_correlated_instance <- function(n = 10, R = 1000) {
    value <- sample(R, size = n, replace = TRUE)
    weight <- value + (R / 10)

    capacity <- round(runif(1) * sum(weight))
    knapsack(capacity = capacity,
             items = data.frame(
                 id = seq_len(n),
                 weight = weight,
                 value = value
             ))
}

#' @export
almost_strongly_correlated_instance <- function(n = 10, R = 1000) {
    weight <- sample(R, size = n, replace = TRUE)
    capacity <- round(runif(1) * sum(weight))

    min_vals <- pmax(weight - (R / 10) - (R / 500), 1)
    max_vals <- weight + (R / 10) + (R / 500)

    addon <- sample((2 * ((R / 10) + (R / 500))) - 1, size = n,
                    replace = TRUE)
    value <- pmin(min_vals + addon, max_vals)

    knapsack(capacity = capacity,
             items = data.frame(
                 id = seq_len(n),
                 weight = weight,
                 value = value
             ))
}

#' @export
subset_sum_instance <- function(n = 10, R = 1000) {
    weight <- sample(R, size = n, replace = TRUE)
    value <- weight
    capacity <- round(runif(1) * sum(weight))

    knapsack(capacity = capacity,
             items = data.frame(
                 id = seq_len(n),
                 weight = weight,
                 value = value
             ))

}

#' @export
uncorrelated_instance_w_sim_wts <- function(n = 10, R = 1000) {
    weight_offset <- sample(101, size = n, replace = TRUE)
    weight <- 100000 + weight_offset - 1
    capacity <- round(runif(1) * sum(weight))
    value <- sample(1000, size = n, replace = TRUE)

    knapsack(capacity = capacity,
             items = data.frame(
                 id = seq_len(n),
                 weight = weight,
                 value = value
             ))
}

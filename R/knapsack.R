knapsack <- function(capacity, items, density_order = TRUE) {
    if (!inherits(items, "data.frame"))
        stop("items must be a data frame")
    if (!setequal(names(items), c("id", "weight", "value")))
        stop("items must be a data frame with columns: id, weight, value")
    structure(
        list(capacity = capacity,
             items = stackify(items, density_order = density_order),
             value = 0L,
             taken = stack()),
        class = "knapsack"
    )
}

capacity <- function(ks) UseMethod("capacity")
capacity.knapsack <- function(ks) ks$capacity

items <- function(ks) UseMethod("items")
items.knapsack <- function(ks) ks$items

next_item <- function(ks) UseMethod("next_item")
next_item.knapsack <- function(ks) ks$items("top")

print.knapsack <- function(x, ...) {
    its <- items(x)
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
    cat("\nTaken Items:", x$taken("size"), "\n")
    cat("Total value:", x$value)
    invisible(x)
}

# random knapsack stuff from:
# https://pdfs.semanticscholar.org/ebd4/646ce0bd4185ea67b5f3dd265686284adb3a.pdf

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

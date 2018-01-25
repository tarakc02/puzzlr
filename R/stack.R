stack <- function() {
    function(query)
        switch(query,
               top = NULL,
               without_top = stop("Stack is already empty", call. = FALSE),
               size = 0L)
}

push <- function(s, new_item) {
    sz <- s("size") + 1L
    force(new_item)
    function(query)
        switch(query,
               top = new_item,
               without_top = s,
               size = sz)
}

stackify <- function(items, density_order) {
    if (density_order) items <- dplyr::arrange(items, value / weight)
    res <- stack()
    for (i in seq(from = 1, to = nrow(items)))
        res <- push(res, as.list(items[i, , drop = FALSE]))
    res
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

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

stack_to_list <- function(s) {
    res <- vector("list", s("size"))
    i <- 1L
    while (s("size") > 0) {
        res[[i]] <- s("top")
        i <- i + 1L
        s <- s("without_top")
    }
    res
}

stackify <- function(items, density_order) {
    if (density_order) items <- dplyr::arrange(items, value / weight)
    res <- stack()
    for (i in seq(from = 1, to = nrow(items)))
        res <- push(res, as.list(items[i, , drop = FALSE]))
    res
}


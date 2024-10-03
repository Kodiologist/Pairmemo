# Test auxiliary functions for managing caches.
source("setup.R")


test("kvs",
   {pm(inc <- \(x)
        x + 1)

    is(inc(3), 4)
    l = pairmemo::kvs(inc)
    is(is.list(l), T)
    is(length(l), 1L)
    is(names(l[[1]]$k), c("file_format", "time", "args"))
    is(l[[1]]$v, 4)})


test("clear",
   {pm(inc <- \(x)
        x + 1)

    inc(1)
    inc(2)
    inc(3)
    inc(4)
    inc(5)
    is(
        sort(sapply(unname(meta(inc)), \(k) k$args$x)),
        c(1L, 2L, 3L, 4L, 5L))
    is(
        pairmemo::clear(inc, \(k) 1 < k$args$x && k$args$x < 5),
        c("Cache entries deleted" = 3L))
    is(
        sort(sapply(unname(meta(inc)), \(k) k$args$x)),
        c(1L, 5L))})

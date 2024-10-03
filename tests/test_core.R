# Test some core features of Pairmemo.
source("setup.R")


test("cached",
   {n.calls = 0
    pm(add <- \(x, y)
       {n.calls <<- n.calls + 1
        x + y})

    is(add(1, 2), 3)
    is(n.calls, 1)
    is(add(5, 5), 10)
    is(n.calls, 2)
    is(add(1, 2), 3)
    is(n.calls, 2)

    is(length(dir(file.path(TD, "add"))), 2L * 2L)})
      # There are 2 saved calls, and each call has a pair of files.


test("default arguments",
   {n.calls = 0
    pm(add <- \(x, y = 17)
       {n.calls <<- n.calls + 1
        x + y})

    is(add(10), 27)
    is(n.calls, 1)
    is(add(10, 17), 27)
    is(n.calls, 1)
    # The parameter with a default argument (and its default argument)
    # aren't saved in the metadata, so the user can add new parameters
    # without creating new cache misses.
    is(meta(add)[[1]]$args, list(x = 10L))
      # `10` in R gets serialized to `10` in JSON, which becomes
      # `10L` in R.

    is(add(10, 12), 22)
    is(n.calls, 2)
    # Once we set the parameter to something other than its default
    # argument, it appears in the metadata.
    is(meta(add, \(x) length(x$args) > 1)[[1]]$args,
        list(x = 10L, y = 12L))})


test("default arguments with ...",
  # In the presence of a `...` parameter, we don't try to compare any
  # arguments against default arguments.
   {pm(foo <- \(a, b = 17, ...)
       list(a, b, ...))

    is(foo(1), list(1, 17))
    is(meta(foo)[[1]]$args,
        list(a = 1L))
    is(foo(1, 17), list(1, 17))
    is(meta(foo, \(x) length(x$args) > 1)[[1]]$args,
        list(a = 1L, b = 17L))
    is(foo(1, 2, ccc = 3, ddd = 4), list(1, 2, ccc = 3, ddd = 4))
    is(meta(foo, \(x) "ccc" %in% names(x$args))[[1]]$args,
        list(a = 1L, b = 2L, ccc = 3L, ddd = 4L))})


test("reorder parameters",
   {n.calls = 0
    pm(add <- \(x, y)
       {n.calls <<- n.calls + 1
        x + y})
    is(add(3, 4), 7)
    is(n.calls, 1)
    # If the function definition is changed only to reorder the
    # parameters, caching is unaffected, provided that calls use the
    # same parameter names in the same way.
    pm(add <- \(y, x)
       {n.calls <<- n.calls + 1
        x + y})
    is(add(4, 3), 7)
    is(n.calls, 1)})


test("PAIRMEMO.KV",
   {pm(inc <- \(x)
        x + 1)

    l = inc(PAIRMEMO.KV = T, 5)
    is(is.list(l), T)
    is(names(l), c("k", "v"))
    is(names(l$k), c("file_format", "time", "args"))
    is(l$k$file_format, "rds")
    is(is.numeric(l$k$time), T)
    is(l$k$args, list(x = 5L))
      # Deserialization from JSON has converted `x` to an integer.
    is(l$v, 6)})


test("mem",
    for (mem in c(F, T))
       {n.calls = 0
        pm(mem = mem, inc <- \(x)
           {n.calls <<- n.calls + 1
            x + 1})
        pairmemo::clear(inc)

        is(inc(3), 4)
        is(n.calls, 1)
        # Delete the disk files manually, to check that with `mem = T`
        # we can still get the saved value from memory. (In general
        # use, you shouldn't desynchronize the caches like this.)
        unlink(dir(file.path(TD, "inc"), full = T))
        is(inc(3), 4)
        is(n.calls, (if (mem) 1 else 2))})


test("ap",
   {n.calls = 0
    pm(ap = base::list(always.int = as.integer),
        show.classes <- \(always.int, varies)
            {n.calls <<- n.calls + 1
             paste(class(always.int), class(varies))})

    is(show.classes(1L, 2L), "integer integer")
    is(n.calls, 1)
    is(show.classes(1, 2L), "integer integer")
    is(n.calls, 1)
    is(show.classes(1, 2), "integer numeric")
    is(n.calls, 2)
    is(show.classes("1", 2), "integer numeric")
    is(n.calls, 2)})

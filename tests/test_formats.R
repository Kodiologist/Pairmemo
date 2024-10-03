source("setup.R")


test("list built-in formats",
   {l = pairmemo::builtin.formats
    is(names(l), c("rds", "qs", "fst"))
    is(sort(names(l$rds)), c("name", "read", "write"))
    is(l$rds$name, "rds")
    is(is.function(l$rds$read), T)
    is(is.function(l$rds$write), T)})


test("format qs",
   {pm(format = "qs", f <- \(x)
        list(5, 5L, x, factor("spess")))
    is(f("hi"), list(5, 5L, "hi", factor("spess")))
    is(qs::qread(file.path(TD, "f", names(meta(f)))), f("hi"))})


test("format fst",
   {pm(format = "fst", f <- \(x)
        data.frame(a = x + 1:10, b = x + 11:20))
    is(data.table::is.data.table(f(8)), T)
      # We always use `as.data.table = T` with `read.fst`.
    is(as.data.frame(f(8)), data.frame(a = 8 + 1:10, b = 8 + 11:20))})


test("format custom",
   {myfmt = list(
        name = "myfmt",
        read = readLines,
        write = \(v, path)
            cat("I got: ", v, "\n", sep = "", file = path))
    pm(format = myfmt, inc <- \(x)
        x + 1)

    is(inc(3), "I got: 4")
    is(meta(inc)[[1]]$file_format, "myfmt")
    is(readLines(file.path(TD, "inc", names(meta(inc)))),
        "I got: 4")})

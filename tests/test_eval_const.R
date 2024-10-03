source("setup.R")


test("allowed",
   {is(eval.const(quote(NULL)), NULL)
    is(eval.const(quote(T)), T)
    is(eval.const(quote(TRUE)), T)
    is(
        eval.const(quote(
            paste(collapse = " ", 1[1], 2[[1]] + abs(-5)))),
        "1 7")})


test("forbidden",
   {foo = 1
    expect_error(eval.const(quote(foo)))})

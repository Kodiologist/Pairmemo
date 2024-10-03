is = testthat::expect_identical
expect_error = testthat::expect_error

TD = NULL
test = \(name, code)
# Create a temporary `pairmemo` directory at `TD`, run the test, and
# then delete the directory.
   {TD <<- tempfile()
    dir.create(TD)
    on.exit(unlink(TD, recursive = T), add = T)
    testthat::test_that(name, {code})}

pm = \(...)
    pairmemo::define(directory = TD, n.frame = 2, ...)
meta = pairmemo::metadata
eval.const = pairmemo::eval.const

\name{formats}
\alias{builtin.formats}
\alias{Formats}

\title{Pairmemo file formats}

\description{By default, Pairmemo saves the results of calling memoized functions as RDS objects, via \code{\link[base]{saveRDS}}. Using the \code{format} parameter to \code{\link{define}}, you can choose one of the other built-in formats, or create your own. I recommend \code{qs} for most purposes; I only picked RDS as the default to minimize dependencies.}

\details{

To use a built-in format, set \code{format} to an element of \code{names(pairmemo::builtin.formats)} (or an element of \code{pairmemo::builtin.formats} itself). To use a custom format, set \code{format} to a named list with these elements:

\itemize{
    \item \code{name}: A string. This is included in the JSON metadata paired with the saved call for the sake of human intelligibility, but is otherwise unused.
    \item \code{read}: A unary function that takes a file path and returns the object that was saved there.
    \item \code{write}: A binary function that takes an object to write followed by the path to write to.}

If you change the format assigned to a function that already has cached calls, be sure to clear the cache, or manually convert all the calls. Otherwise, Pairmemo will try to read them in the wrong format.}

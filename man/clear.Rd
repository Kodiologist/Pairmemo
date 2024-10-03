\name{clear}
\alias{clear}

\title{Delete saved calls}

\description{\code{pairmemo::clear} deletes a function's saved calls from disk (and from memory, if \code{\link{define}} recieved \code{mem = T}). For each call, both paired files are deleted. By default, all calls are cleared; set the \code{filter} parameter to clear a subset. The directory for the function is never deleted. If you want to get rid of the function entirely, you should do that manually.}

\arguments{
    \item{f}{The function to delete calls for.}
    \item{filter}{Per \code{\link{metadata}}. It's wise to use \code{metadata} to check that your filter works as intended before you actually delete stuff.}}

\value{An object of the form \code{c("Cache entries deleted" = n)}, where \code{n} is an integer.}

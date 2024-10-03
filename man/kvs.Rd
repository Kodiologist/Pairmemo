\name{kvs}
\alias{kvs}

\title{Read a function's saved calls en masse}

\description{\code{pairmemo::kvs} is similar to \code{\link{metadata}}, but it also reads and return function values. The return value of \code{kvs} is a list of named lists; the names are \code{k} ("key", for the metadata) and \code{v} ("value").}

\arguments{
    \item{f}{The function to inspect.}
    \item{filter}{Per \code{\link{metadata}}. Calls are filtered out before the call values are read from disk, so a well-chosen filter can save time and memory for a function with massive saved calls.}}

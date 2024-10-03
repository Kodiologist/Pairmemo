\name{metadata}
\alias{metadata}

\title{List a function's saved calls}

\description{\code{pairmemo::metadata} returns a list describing all currently memoized calls of a Pairmemoized function. The names of this list are the argument hashes, and the values are the deserialized JSON metadata files.}

\arguments{
    \item{f}{The function to inspect.}
    \item{filter}{An optional function to subset the calls that are returned.}}
    
\details{

Each JSON metadata file has these elements:

\itemize{
    \item \code{file_format}: A string naming the format in which the call is saved; see \link{Formats}.
    \item \code{time}: The time the call took, in seconds.
    \item \code{args}: The arguments to the function, as a named list. Since the  \code{jsonlite} package doesn't round-trip all R objects, these values won't always be equal to the real arguments. In particular, two lists of arguments can be non-equal in R terms (and have distinct hashes, hence be treated as separate calls by Pairmemo) but result in the same JSON.}

If a \code{filter} argument is given to \code{pairmemo::metadata}, it should take a named list representing a JSON metadata file and return a boolean value. (Returning a vector with more than one element is an error, but a zero-length vector is understood to mean "false".) The corresponding call is only returned by \code{pairmemo::metadata} if the filter returns true.}

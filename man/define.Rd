\name{define}
\alias{define}

\title{Set up memoization for a function}

\description{\code{define} is the main entry point for Pairmemo.}

\details{

\code{pairmemo::define} requires two arguments: \code{f}, the function definition, and \code{directory}, the path to a preexisting directory in which calls should be saved. Syntactically, \code{f} must be of the form \code{IDENTIFIER <- FUNCTION}. You can't use \code{=} instead of \code{<-}, or surround the whole thing in braces, or that sort of thing, because \code{pairmemo::define} needs to extract the function name from \code{f}.

Simple usage looks like this:

\preformatted{
    pairmemo::define(
        my.cool.function <- function(foo, bar)
           {message("Calling my cool function")
            foo + bar},
        directory = "/tmp/pairmemo")

    my.cool.function(1, 2)
    my.cool.function(1, 2) # Cached, so no message is printed.

    pairmemo::clear(my.cool.function) # Clear the cache.
    my.cool.function(1, 2)}

For legibility and namespace hygiene, I encourage calling Pairmemo's functions with \code{::} instead of using \code{library(pairmemo)}.

If you look in \code{"/tmp/pairmemo"}, you'll see a single directory named \code{"my.cool.function"}. Inside \code{"my.cool.function"} is a pair of files for each saved call (hence the name "Pairmemo"). One, storing the saved return value, is named with a hash of the arguments, with no file extension. The other file has the same name except that it ends with \code{".json"}: this is the \link{metadata} file. Inspecting the file (or calling \code{\link{metadata}}) shows you e.g. what arguments the function was called with.

It helps to understand how Pairmemo regularizes and hashes the arguments, so you can predict what changes to your function will invalidate your caches. The arguments are put in a named list, where the names are the parameter names (in full, whether or not the call used a partial match), in alphabetical order. If an argument is set to its default for that parameter, and the default can be evaluated with \code{\link{eval.const}}, and there is no \code{...} parameter for the function, that argument is excluded from the list: thus, you can add parameters, with the default value representing previous behavior, to an existing function while keeping your entire cache. The list is then hashed with \code{\link[digest]{digest}} and the character "h" is prepended. If you rename your function, just rename its directory to retain the cache.

In practice, a given project will probably have several different functions to be cached, which have different names but should all use the same Pairmemo directory, so it's convenient to use a wrapper function for \code{define} like so:

\preformatted{
    pm = function(...)
        pairmemo::define(
            directory = "/home/neumanae/myproject/pairmemo",
            n.frame = 2,
            ...)

    pm(my.cool.function <- function(foo, bar)
       {message("Calling my cool function")
        foo + bar})}

\code{define} has several optional arguments:

\itemize{
    \item \code{mem}: If true, save calls in memory, as well as on disk. Calls that are saved on disk but weren't made in this R process are only loaded into memory once they're read from disk for the first time.
    \item \code{format}: A string or function specifying the file format for saving calls (see \link{Formats}).
    \item \code{ap}: A named list of functions to use as argument preprocessors (see below).
    \item \code{n.frame}: Used as an argument to \code{\link[base]{parent.frame}} in the \code{\link[base]{assign}} call that \code{pairmemo::define} makes to name the newly defined function. The default, 1, is appropriate when calling \code{define} directly. Use 2 when writing a wrapper function for \code{define}.}}

\value{The return value of the \code{assign} call is passed through.}

\section{Argument preprocessors}{

It can be useful to transform arguments before Pairmemo hashes them. For example, if a function has a parameter that should be an integer, applying \code{as.integer} ensures that Pairmemo treats calling the function with \code{2.0} the same as calling it with \code{2L}.

The parameter \code{ap} ("argument preprocessors") to \code{pairmemo::define} names each argument to be preprocessed and provides a unary function to do the processing. Notice that \code{f} receives the preprocesssed result, not the original argument; this is construed as a feature.

\preformatted{
    pairmemo::define(directory = "…", ap = list(n = as.integer),
        f <- function(n) class(n))
    f(2.0)  # "integer"}}

\section{PAIRMEMO.KV}{

If the first argument in a call to a memoized function is named \code{PAIRMEMO.KV}, Pairmemo will specially recognize (and remove) this argument. Then, the function will return a named list of two objects \code{k} and \code{v}, per \code{\link{kvs}} (but unlike \code{kvs}, the result is the same regardless of whether the call has already been cached). This feature is mostly useful to get the execution time of the function.}

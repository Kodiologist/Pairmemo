\name{eval_const}
\alias{eval.const}

\title{Evaluate a class of constant expressions}

\description{\code{eval.const} evaluates a quoted expression in a limited environment that only allows things like arithmetic operators and \code{base::list}. The function exists mostly for internal use, but who knows, maybe you'll find it handy.}

\references{The implementation is based on \href{https://stackoverflow.com/a/18391779}{a Stack Overflow answer by Hadley Wickham}.}

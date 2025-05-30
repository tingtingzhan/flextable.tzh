% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_adjust_.R
\name{p_adjust_}
\alias{p_adjust_}
\alias{p_adjust_.numeric}
\alias{p_adjust_.pairwise.htest}
\title{Adjusted \eqn{p}-Values in All Correction Methods}
\usage{
p_adjust_(x)

\method{p_adjust_}{numeric}(x)

\method{p_adjust_}{pairwise.htest}(x)
}
\arguments{
\item{x}{see \strong{Usage}}
}
\value{
All method dispatches of generic function \link{p_adjust_} return a \link[base]{matrix} of adjusted \eqn{p}-values using all available \link[stats]{p.adjust.methods}.
}
\description{
Adjusted \eqn{p}-values in all correction methods.
}
\keyword{internal}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-adjs.R
\docType{methods}
\name{defadj}
\alias{defadj}
\alias{defadj,ANY-method}
\title{Define custom adjoint methods}
\usage{
defadj(f, ...)

\S4method{defadj}{ANY}(f, ...)
}
\arguments{
\item{f}{a custom function}

\item{...}{list of functions that define how gradients are computed w.r.t
the number of an argument}
}
\description{
Define methods to take analytic gradients for a custom function
}

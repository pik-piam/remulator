% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bisect.R
\name{bisect}
\alias{bisect}
\title{Use bisection to find x so that myform(param, x) is close to approx_this}
\usage{
bisect(param, myform, approx_this, lower, upper, eps)
}
\arguments{
\item{param}{Parameters applied in \code{myform}}

\item{myform}{User defined function used in approximation}

\item{approx_this}{y-value for which x-value should be approximated}

\item{lower}{Lower limit of interval to search in}

\item{upper}{Upper limit of interval to search in}

\item{eps}{Maximal distance of x to approx_this}
}
\value{
x-value that is within the eps distance to approx_this
}
\description{
Use bisection to find x so that myform(param, x) is close to approx_this
}
\author{
David Klein
}

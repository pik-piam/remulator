% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_flat_fits.R
\name{replace_flat_fits}
\alias{replace_flat_fits}
\title{Replace fits in years where they are flat with fits from other years}
\usage{
replace_flat_fits(
  path_to_postfit_Rdata,
  emu_path = "output/emulator",
  fitname = "replaced-flat"
)
}
\arguments{
\item{path_to_postfit_Rdata}{Path to the Rdata file that contains raw data, fitted data and fitcoefficients saved by the emulator function}

\item{emu_path}{Path the emulator results have been saved to by the emulator function.}

\item{fitname}{Name that describes the fit (default: linear) and will be used for naming the output folders.}
}
\description{
This function considers fits with a slope less than 0.01 flat and tries to replace them with non-flat fits from other years.
}
\seealso{
\code{\link{emulator}} \code{\link{fill_missing_years}}
}
\author{
David Klein
}
# R emulator

R package **remulator**, version **1.18.5**

[![CRAN status](https://www.r-pkg.org/badges/version/remulator)](https://cran.r-project.org/package=remulator) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2546517.svg)](https://doi.org/10.5281/zenodo.2546517) [![R build status](https://github.com/pik-piam/remulator/workflows/check/badge.svg)](https://github.com/pik-piam/remulator/actions) [![codecov](https://codecov.io/gh/pik-piam/remulator/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/remulator) [![r-universe](https://pik-piam.r-universe.dev/badges/remulator)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

A collection of R tools for fitting model results. 


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("remulator")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("magpie_emulator_for_remind") # How to produce MAgPIE 4.0 emulators and use them in REMIND 2.0
vignette("remulator")                  # Fitting curves to data (in magclass format) and plotting the curves to nice graphs and pdf
```

## Questions / Problems

In case of questions / problems please contact David Klein <dklein@pik-potsdam.de>.

## Citation

To cite package **remulator** in publications use:

Klein D (2021). _remulator: R emulator_. doi: 10.5281/zenodo.2546517 (URL: https://doi.org/10.5281/zenodo.2546517), R package version 1.18.5, <URL: https://github.com/pik-piam/remulator>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {remulator: R emulator},
  author = {David Klein},
  year = {2021},
  note = {R package version 1.18.5},
  doi = {10.5281/zenodo.2546517},
  url = {https://github.com/pik-piam/remulator},
}
```


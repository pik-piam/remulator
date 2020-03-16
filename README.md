# R emulator package

## Purpose and Functionality

The remulator R package is a collection of R tools to fit curves using an optimization algorithm.

## Installation

For installation an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the package can be installed using `install.packages`:

```r 
install.packages("remulator")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r 
vignette("remulator")
```

## Questions / Problems

In case of questions / problems please contact David Klein <dklein@pik-potsdam.de> or take a look at the short FAQ below

## FAQ
Q: Pdflatex reports an error if I want to generate my output PDF. Why? 
A: Please check if the automatic installation of styles is allowed for pdflatex.
If not, the pdf creation process may have been stopped due to un-installed style packages (e.g. packageXY.sty). 

Q: I can't build my vignette. Calling 'vignette("remulator")' gives me nothing. Why?
A: You can try a workaround in RStudio: open remulator/vignettes/remulator.Rmd . 
Then you can 'knit' the vignette locally by hitting the 'knit' button in the local menu of the opened document. 
We are on it, it is a bug.

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2546517.svg)](https://doi.org/10.5281/zenodo.2546517)

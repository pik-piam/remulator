#' Set duplicated samples to NA
#' 
#' This function sets duplicated samples (per region, year, model, scenario) to NA. This is
#' useful to clean your data before fitting.
#' 
#' @param data MAgPIE object containing the samples to remove the duplicates from.
#' @return Magpie object with duplicated samples set to NA.
#' @author David Klein
#' @importFrom magclass getNames as.magpie

mute_duplicated <- function(data) {
  
  a <- unwrap(data)
  
  dupli <- apply(a,c(1,2,3),duplicated)
  # put dimenson one (samples) to the end
  dupli <- aperm(dupli,c(2,3,4,1))
  # provide names for samples
  dimnames(dupli)[[4]] <- getNames(data,dim=2)
  # provide name for the sample dimension
  names(dimnames(dupli))[4] <- "sample"
  
  dupli <- as.magpie(dupli)
  
  dupli[dupli] <- NA # duplicated values: TRUE  -> NA
  dupli[!dupli] <- 1 # unique values    : FALSE -> 1
  
  # set duplicated values in data to NA by multiplying with dupli
  data <- data*dupli

  return(data)
}
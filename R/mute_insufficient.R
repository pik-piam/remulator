#' In years and regions with only an unsufficient number of data points set all data points to NA 
#' 
#' This function counts data points (per region, year, scenario) and sets them to NA if the count
#' is less than given in \code{n_suff}. This is useful to clean your data before fitting. No fit will
#' be calculated where no data is available.
#' 
#' @param data MAgPIE object containing the samples to remove the duplicates from.
#' @param n_suff Minial number (default=1) of data points that will be regarded as sufficient to perform a fit.
#' @return Magpie object with duplicated samples set to NA.
#' @author David Klein
#' @importFrom magclass getNames as.magpie

mute_insufficient <- function(data,n_suff) {
  
  # find number of non-NA elements
  nonNA_count <- as.magpie(apply(unwrap(data),c(1,2,3,5),function(x)sum(!is.na(x))))
  
  # How much is "enough" data
  nodata <- nonNA_count[,,"x"]<n_suff
  
  # number of samples that are too few
  insufficient_count <- nonNA_count
  insufficient_count[insufficient_count>=n_suff]=0
  
  # number of remaining samples
  samples_count <- nonNA_count
  samples_count[nonNA_count<n_suff]=0
  
  # create empty object
  insufficient_data <- data + NA
  insufficient_data[nodata] <- data[nodata]
  
  attr(data,"insufficient_flag")  <- nodata
  attr(data,"insufficient_count") <- insufficient_count
  attr(data,"insufficient_data")  <- insufficient_data
  attr(data,"samples_count")      <- samples_count
  
  # set insufficient data to NA
  nodata <- collapseNames(nodata,collapsedim = "variable")
  nodata[nodata]  <- NA
  nodata[!nodata] <- 1
  tmp <- data*nodata
  
  # to keep attributes of "data" (have been erased in "tmp" by multiplication above)
  data[,,] <- tmp
  
  return(data)
}
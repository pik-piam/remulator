#' Find fit coefficients for years where no fit could be calculated
#' 
#' @param fitcoef MAgPIE object containing the fit coefficients
#' @param nodata MAgPIE object of the same shape as \code{fitcoef} containing only logicals that indicate where data is not available (TRUE)
#' @param method Choose method that finds fit coefficients for years with no fit. Currently there is only one method availalbe that in a first step
#' takes the fit from the subsequent year. If all subsequent years have no fit it takes the fit from the preceding year. 
#' @return Magpie object with the updated fit coefficients. Has an attribute attached providing the years missing fits were taken from.
#' @author David Klein
#' @importFrom magclass collapseNames getYears getRegions getNames setYears
#' @export

fill_missing_years <- function(fitcoef, nodata, method=1) {

  if (method == 1) {
    # Rule: for each region: take fit from the next available year, if there
    # is no fit in any of the next years, take it from the year before
    
    data_copied <- fitcoef + NA # create empty object
    takenfrom   <- nodata  + NA # create empty object
    
    for (s in getNames(fitcoef,dim=1)) {
      for (r in getRegions(fitcoef)) {
        # There is no fit for any year
        if (length(which(!nodata[r,,]))==0) {
          cat("There is no fit available for any year in",s,r,"!\n")
        } else {
          # 1. Go from end to beginning and take fit from year after
          for (y in (length(getYears(fitcoef))-1):1) {
            if (nodata[r,y,s] & !setYears(nodata[r,y+1,s])) {
              fitcoef[r,y,s] <- setYears(fitcoef[r,y+1,s])
              data_copied[r,y,s] <- setYears(fitcoef[r,y+1,s])
              # update nodata for two reasons:
              # 1. for the next iteration of this for-loop the second part of the condition above "!setYears(nodata[r,y+1,s])" needs to be true because now there is data available
              # 2. step 2. (for-loop below) will not overwrite values that have already been replaced here
              nodata[r,y,s] <- FALSE 
              takenfrom[r,y,s] <- getYears(fitcoef[,y+1,])
            }
          }
          # 2. Go from beginning to end and for remaining missing years take fit from year before
          for (y in 2:length(getYears(fitcoef))) {
            if (nodata[r,y,s] & !setYears(nodata[r,y-1,s])) {
              fitcoef[r,y,s] <- setYears(fitcoef[r,y-1,s])
              data_copied[r,y,s] <- setYears(fitcoef[r,y-1,s])
              nodata[r,y,s] <- FALSE # updata nodata for next iteration of this for-loop
              takenfrom[r,y,s] <- getYears(fitcoef[,y-1,])
            }
          }
        }
      }
    }
    # Add takenfrom as attribute to fitcoef
    attr(fitcoef,"takenfrom")   <- takenfrom
    attr(fitcoef,"copied_data") <- data_copied
    
  } else {
    stop("Unknown method")
  }

  return(fitcoef)
}
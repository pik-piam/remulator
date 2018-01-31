#' Set outlier samples to NA
#' 
#' This function sets outlier samples (per region, year, model, scenario) to NA. This is useful to 
#' clean your data before fitting. A data point is considered an outlier if it is more than \code{range} times
#' of the interquartile range away from the the upper or lower end of the interquartile range
#' \url{http://colingorrie.github.io/outlier-detection.html}
#' 
#' @param data MAgPIE object containing the samples to remove the duplicates from.
#' @param range Multiplied with the interquartile range this is the maximal distance a data point may have to not be considered an outlier.
#' @return Magpie object with duplicated samples set to NA.
#' @seealso \code{\link[graphics]{boxplot}}
#' @author David Klein
#' @importFrom stats complete.cases lm
#' @importFrom magclass getNames as.magpie
#' @importFrom graphics boxplot

mute_outliers <- function(data,range=1.5) {
  
  .outliers <- function(dat,range) {
    # remove NA
    dat <- dat[complete.cases(dat),,drop=FALSE]

    # check for sufficient number of data points
    if (length(dat)<1) {
      res <- list(number_of_outliers= NA,
                  outliers = NA)
      return(res)
    }
    
    # find outliers
    outliers <- 0
    mm <- lm(dat[,"y"]~dat[,"x"]) # perform a linear fit
    # define data points as outliers that are more than range times of the interquartile
    # range away from the the upper or lower end of the interquartile range
    # http://colingorrie.github.io/outlier-detection.html
    bb <- boxplot(mm$residuals,plot=FALSE,range=range)
    number_of_outliers <- length(bb$out)
    if (length(bb$out)>0) {
      # names of samples that are outliers
      outliers <- names(bb$out)
    }
    
    return(list(number_of_outliers=number_of_outliers, outliers=outliers))
  }
  
  a <- unwrap(data)
  
  # code for debugging
  # dat <- a["NAM","y2005",,,c("x","y")]
  # plot(dat)
  # mm <- lm(dat[,"y"]~dat[,"x"])
  # abline(mm)

  tmp <- apply(a[,,,,c("x","y"),drop=FALSE],c(1,2,3),.outliers,range)

  # helper function to extract lists from tmp into different variables
  pick <- function(x,i)return(x[[1]][[i]])
  
  # separate fitcoefficients and message
  outliers_count <- apply(tmp,c(1,2,3),pick,1)
  outliers_index <- apply(tmp,c(1,2,3),pick,2)

  
  data_outliers <- data + NA
  # create MAgPIE object 'outliers' of the same shape as 'data' and initialize with FALSE
  outliers <- collapseNames(data[,,"x"],collapsedim = "variable") # remove variable only, keep scenario
  outliers[,,] <- FALSE
  # set outlier data to NA and fill 'outliers' with TRUE where outliers exist or fill all samples with NA where no info is available
  for (r in getRegions(data)) {
    for(y in getYears(data)) {
      for(s in getNames(data,dim="scenario")) {
        if(!is.na(outliers_index[r,y,s])) {
          idx <- unlist(outliers_index[r,y,s])
          # copy outliers from data to data_outliers
          data_outliers[r,y,s][,,idx] <- data[r,y,s][,,idx]
          # clear outliers in data
          data[r,y,s][,,idx] <- NA
          outliers[r,y,s][,,idx] <- TRUE
        }
        else {
          outliers[r,y,s] <- NA
        }
      }
    }
  }
  
  attr(data,"outliers_flag")  <- outliers==1 # for some reason the TRUE/FALSE from above are translated into 1/0 -> make it TRUE/FALSE again
  attr(data,"outliers_count") <- as.magpie(outliers_count)
  attr(data,"outliers_data")  <- data_outliers
  
  return(data)
}
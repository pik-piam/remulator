#' Use bisection to find x so that myform(param, x) is close to approx_this
#' 
#' @param param Parameters applied in \code{myform}
#' @param myform User defined function used in approximation
#' @param approx_this y-value for which x-value should be approximated
#' @param lower Lower limit of interval to search in
#' @param upper Upper limit of interval to search in
#' @param eps Maximal distance of x to approx_this
#' @return x-value that is within the eps distance to approx_this
#' @author David Klein

bisect <- function(param, myform, approx_this, lower,upper, eps) {

  # actual bisection function 
  .bisect <- function (param, myform, approx_this, lower,upper, eps) {

    # swap lower and upper if function is decreasing (otherwise bisection does not work)
    fl <- myform(param,lower)
    fu <- myform(param,upper)
    
    if (any(is.na(c(fl,fu)))) {
      # if no function value could be calculated (probably because one of the coefficients is NA)
      return(NA)
    } else if ( fl == fu) {
      # if function is flat
      return(Inf)
    } else  if ( fl < fu) {
      # if approx_this is beyond range return upper limit
      if (fu < approx_this) return(upper)
      # if function is increasing call .bisect normally
    } else {
      # if approx_this is below range return lower limit
      if (fl > approx_this) return(lower)
      # if function is decreasing swap lower and upper
      temp <- lower
      lower <- upper
      upper <- temp
    }
    
    middle <- (lower+upper)/2
    delta <- approx_this - myform(param,middle)
    #print(c(middle,delta))
    if (abs(delta)<eps) {
      res <- middle
    } else if (delta > 0) {
      res <- bisect(param, myform,approx_this,lower=middle,upper=upper,eps)
    } else {
      res <- bisect(param, myform,approx_this,lower=lower,upper=middle,eps)
    }
    return(res)
  }
  
  # try to call .bisect
  res <- tryCatch(
    {res_try <- .bisect(param, myform,approx_this,lower,upper,eps)
    return(res_try)
    }, error = function(err) {
      print(paste("BISECT ERROR:  ",err))
      return(NA)
    }
    , warning = function(war) {
      print(paste("BISECT WARNING:  ",war))
      return(NA)
    }
  )
  
  return(res)
}

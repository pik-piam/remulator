#' Minimizes the sum of the error squares
#' 
#' This is an internal function of the remulator package. It fits a user defined function to data minimizing the sum of the error squares.
#'  
#' @param dat Array with data points to fit containing one column with sample numbers, one column with x values, and one column with y values
#' @param initial_values Initial values for the parameters to be optimized over.
#' @param userform Fit function for which least squares will be calaculated using the \code{optim} function.
#' @param ... Arguments passed on to the \code{optim} function. Useful to define bounds on fit coefficients.
#' @return list containing fit coefficients and messages
#' @author David Klein
#' @importFrom stats optim complete.cases

minimize_least_squares <- function(dat,initial_values,userform,...){
    
  message <- ""
  # remove NA and duplicates
  #if (sum(!complete.cases(dat))>0) cat("calculate_fit: Removed",sum(!complete.cases(dat)),"incomplete cases from your data!\n")
  dat <- dat[complete.cases(dat),,drop=FALSE]
  
  # remove duplicates
  if (length(dat)>2) {
    nrmdup <- sum(duplicated(dat))
    if (nrmdup>0) { 
      cat("calculate_fit: Removed",nrmdup,"duplicates from your data!\n")
      message <- paste0("D",nrmdup)
    }
    dat <- dat[!duplicated(dat),,drop=FALSE]
  }
  
  # check for sufficient number of data points
  if (length(dat)<1) {
    res <- list(coefficients = rep(NA,length(initial_values)),  message = paste("n = 0",message,"No points"))
    return(res)
  }
  
  # The function to be minimized, with first argument the vector of parameters
  # over which minimization is to take place. It should return a scalar result.
  formula_least_squares <- function (param,userform,x,y) {
    sum((y - userform(param,x))^2,na.rm = TRUE)
  }
  
  # minimize error squares using formula_least_squares
  out <- tryCatch({
    
    opt <- optim(initial_values,fn=formula_least_squares,userform=userform,x=dat[,"x"],y=dat[,"y"],method="L-BFGS-B",...) #control=list(maxit=1000),
    
    wasauchimmer <- list(coefficients = opt$par, message = paste("n =",nrow(dat),message,opt$message))
    
  }, error = function(err) {
    
    print(paste("FIT ERROR:  ",err))
    res <- list(coefficients = rep(NA,length(initial_values)), message = err$message)
    return(res)
    
  }, warning = function(war) {
    
    print(paste("FIT WARNING:  ",war))
    res <- list(coefficients = rep(NA,length(initial_values)), message = war$message)
    return(res)
  }
  )
  
  return(out)
}
  
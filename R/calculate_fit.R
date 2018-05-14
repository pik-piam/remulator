#' Calculate fit by minimizing sum of error squares
#' 
#' This is an internal function of the remulator package. It fits a curve to given data. For each region and year it minimizes the function \code{sum((y - (a + b * x ^d))^2}
#' x and y are the data provided by the user, and a, b, and d are the coefficients to be found. So, the fit is \code{y = a+b*x^d}.
#' 
#' @param data MAgPIE object with data to fit.
#' @param initial_values Initial values for the parameters to be optimized over.
#' @param form Fit function for which least squares will be calaculated using the \code{optim} function.
#' @param ... Arguments passed on to the \code{optim} function. Useful to define bounds on fit coefficients.
#' @return MAgPIE object with fit coefficients a, b, and d
#' @author David Klein
#' @importFrom stats optim complete.cases
#' @importFrom magclass unwrap as.magpie getSets<-

calculate_fit <- function(data,initial_values=c(1,1,1), form,...) {
  
  # convert MAgPIE object into matrix
  a <- unwrap(data)

  # fit supplycurves for all regions (1), all years (2), and all scenarios (3)
  tmp <- apply(a,c(1,2,3),minimize_least_squares,initial_values,form,...)
  #minimize_least_squares(dat=a["PAS","y2035",,,],initial_values = initial_values, userform = form,...)
  #minimize_least_squares(dat=a["LAM","y2095",,,],initial_values = initial_values, userform = form,...)
  #minimize_least_squares(dat=a["PAO","y2065",,,],initial_values = initial_values, userform = form,...)
  
  # helper function to extract coefficients and messages from tmp into different variables
  pick <- function(x,i)return(x[[1]][[i]])
  
  # separate fitcoefficients and message
  fitcoef <- apply(tmp,c(1,2,3),pick,1)
  message <- apply(tmp,c(1,2,3),pick,2)

  # if there is only ONE fit coefficient there is no extra dimension for the coefficients. Thus, add a dummy dimension 
  # so that the remainder of the script works also for a one-coefficient-fit
  if (length(dim(fitcoef==3))) {
    merke <- dimnames(fitcoef)
    fitcoef <- array(fitcoef,dim=c(1,dim(fitcoef))) # add dummy dimension in the first dimension
    dimnames(fitcoef) <- c("dummy",merke) # add dummy name (will be updated below)
  }
  # put dimenson one (coefficients) to the end
  fitcoef <- aperm(fitcoef,c(2,3,4,1)) 
  # provide names for fit coefficients
  dimnames(fitcoef)[[4]] <- letters[1:dim(fitcoef)[4]]
  # provide name for the dimension of fit coefficients
  names(dimnames(fitcoef))[4] <- "coeff"

  fitcoef <- as.magpie(fitcoef)
  message <- as.magpie(message)
  attr(fitcoef,"message") <- message

  return(fitcoef)
}

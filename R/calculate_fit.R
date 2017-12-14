#' Calculate fit by minimizing sum of error squares
#' 
#' This is an internal function of the remulator package. It fits a curve to given data. For each region and year it minimizes the function \code{sum((y - (a + b * x ^d))^2}
#' x and y are the data provided by the user, and a, b, and d are the coefficients to be found. So, the fit is \code{y = a+b*x^d}.
#' 
#' 
#' @param data MAgPIE object with data to fit
#' @param ... Arguments passed on to the \code{optim} function. Useful to define bounds on fit coefficients.
#' @return MAgPIE object with fit coefficients a, b, and d
#' @author David Klein
#' @importFrom stats optim 
#' @importFrom magclass unwrap as.magpie getSets<-

calculate_fit <- function(data,...) {
  
  ##########################################################################
  ##### D A T A: Fit supplycurces using optimization (with apply) ##########
  ##########################################################################
  
  least_squares <- function(dat,...){
    
    form1 <- function (param,x,y) {
      # provide start values for parameters
      a<-param[1]; b<-param[2]; d<-param[3]
      # sum of error squares 
      sum((y - (a + b * x ^d))^2,na.rm = TRUE)
    }
    
    # minimize error squares using form1 subject to lower bounds on coefficients
    res <- tryCatch( 
      {opt <- optim(par=c(1,1,1),fn=form1,x=dat[,"x"],y=dat[,"y"],method="L-BFGS-B",...)
         return(opt$par)
      }, error = function(err) {
         print(paste("MY_ERROR:  ",err))
         return(c(0,0,0)) 
      }
      #, warning = function(war) {
      #   print(paste("MY_WARNING:  ",war))
      #   return(c(0,0,0))
      #}
    )
    return(res)
  }
  
  # convert MAgPIE object into matrix
  a <- unwrap(data)
  
  # fit supplycurves for all regions (1), all years (2), and all scenarios (3)
  tmp <- apply(a,c(1,2,3),least_squares,...)
  
  # provide names for fit coefficients
  dimnames(tmp)[[1]] <- c("a","b","d")
  
  fitcoef <- as.magpie(tmp)
  
  getSets(fitcoef) <- c("region","year","coeff","scenario")
  
  return(fitcoef)
}

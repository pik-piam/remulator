#' Calculate fit by minimizing sum of error squares
#' 
#' This is an internal function of the remulator package. It fits a curve to given data. For each region and year it minimizes the function \code{sum((y - (a + b * x ^d))^2}
#' x and y are the data provided by the user, and a, b, and d are the coefficients to be found. So, the fit is \code{y = a+b*x^d}.
#' 
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
  
  ##########################################################################
  ##### D A T A: Fit supplycurces using optimization (with apply) ##########
  ##########################################################################
  
  minimize_least_squares <- function(dat,initial_values,userform,...){
     
    # The function to be minimized, with first argument the vector of parameters
    # over which minimization is to take place. It should return a scalar result.
    formula_least_squares <- function (param,userform,x,y) {
      sum((y - userform(param,x))^2,na.rm = TRUE)
    }
    
    # remove NA and duplicates
    message <- ""
    #if (sum(!complete.cases(dat))>0) cat("calculate_fit: Removed",sum(!complete.cases(dat)),"incomplete cases from your data!\n")
    dat <- dat[complete.cases(dat),,drop=FALSE]
    if (length(dat)>2) {
      nrmdup <- sum(duplicated(dat))
      if (nrmdup>0) { 
        cat("calculate_fit: Removed",nrmdup,"duplicates from your data!\n")
        message <- paste0("D",nrmdup)
      }
      dat <- dat[!duplicated(dat),,drop=FALSE]
    }

    if (length(dat)<1) {
      res <- list(coefficients = rep(NA,length(initial_values)), message = paste("n = 0",message,"No points"))
      return(res)
    }
    
    # minimize error squares using formula_least_squares
    out <- tryCatch({
      
        opt <- optim(initial_values,fn=formula_least_squares,userform=userform,x=dat[,"x"],y=dat[,"y"],method="L-BFGS-B",...) #control=list(maxit=1000),
        wasauchimmer <- list(coefficients = opt$par, message = paste("n =",nrow(dat),message,opt$message))

      }, error = function(err) {

        print(paste("FIT ERROR:  ",err))
        res <- list(coefficients = rep(NA,length(initial_values)), message = err)
        return(res)
        
      }, warning = function(war) {
        
        print(paste("FIT WARNING:  ",war))
        res <- list(coefficients = rep(NA,length(initial_values)), message = war)
        return(res)
      }
    )
    return(out)
  }
  
  # # convert MAgPIE object into matrix
  a <- unwrap(data)

  # fit supplycurves for all regions (1), all years (2), and all scenarios (3)
  tmp <- apply(a,c(1,2,3),minimize_least_squares,initial_values,form,...)
  #minimize_least_squares(dat=a["PAS","y2035",,,],initial_values = initial_values, userform = form,...)
  
  # helper function to extract coefficients and messages from tmp into different variables
  pick <- function(x,i)return(x[[1]][[i]])
  
  # separate fitcoefficients and message
  fitcoef <- apply(tmp,c(1,2,3),pick,1)
  message <- apply(tmp,c(1,2,3),pick,2)
  
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

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
#' @importFrom stats optim 
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
    
    # if number of points to fit is less than three don't try to fit
    if (sum(!is.na(dat[,"x"]),na.rm=TRUE)<3) {
      cat("Insufficient number of points.\n")
      res <- c(0,0,0)
      attributes(res) <- list(message = "Insufficient number of points")
      return(res)
    }

    # minimize error squares using formula_least_squares
    out <- tryCatch(
      { opt <- optim(initial_values,fn=formula_least_squares,userform=form,x=dat[,"x"],y=dat[,"y"],method="L-BFGS-B",...) #control=list(maxit=1000),
        if(opt$convergence>1) {
          cat("\n",opt$message,"\n   ",opt$convergence)
          #opt$par <- c(0,0,0)
        }
        res <- opt$par
        attributes(res) <- list(message = "ok")
        return(res)
      }, 
        error = function(err) {
        print(paste("MY_ERROR:  ",err))
        res <- c(0,0,0)
        attributes(res) <- list(message = err)
        return(res)
      },
        warning = function(war) {
        print(paste("MY_WARNING:  ",war))
        res <- c(0,0,0)
        attributes(res) <- list(message = war)
        return(res)
      }
    )
    return(out)
  }
  
  # # convert MAgPIE object into matrix
  a <- unwrap(data)

  # fit supplycurves for all regions (1), all years (2), and all scenarios (3)
  tmp <- apply(a,c(1,2,3),minimize_least_squares,initial_values,form,...)

  # put dimenson one (coefficients) to the end
  tmp <- aperm(tmp,c(2,3,4,1)) 
  # provide names for fit coefficients
  dimnames(tmp)[[4]] <- letters[1:dim(tmp)[4]]
  # provide name for the dimension of fit coefficients
  names(dimnames(tmp))[4] <- "coeff"

  # # Alternative approach: for-loops instead of apply
  # tmp <- array(data=NA, dim=c(dim(a)[1:3],3), dimnames=c(dimnames(a)[1:3],list(coeff=letters[1:3])))
  # for (r in dimnames(a)$region) {
  #   for (y in dimnames(a)$year) {
  #     for (s in dimnames(a)$scenario) {
  #       cat("\n",r," ",y," ",s," ")
  #       tmp[r,y,s,] <- minimize_least_squares(a[r,y,s,,],initial_values = initial_values,userform = form,...)
  #     }
  #   }
  # }
  
  fitcoef <- as.magpie(tmp)
  
  return(fitcoef)
}

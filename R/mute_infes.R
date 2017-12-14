#' Set data of infeasible years and their successors to NA
#' 
#' This function sets data of infeasible years and all years after the first infeasible
#' year to NA because results after an infeasible year are not meaningful. The modelstatus
#' has to be provided as one of the variables in the data. This function will look for the
#' modelstatus variable with the name given in \code{name}. If \code{return_infes} is TRUE
#' this function return the matrix with infeasible years instead of the data. 
#' 
#' @param data MAgPIE object containing the results of a MAgPIE run and the modelstatus.
#' @param name String providing the name of the variable that holds the modelstatus.
#' @param return_infes Logical. If TRUE function return matrix with modelstatus instead of data.
#' @param infeasible Integer vector defining which modelstatus will be treated as infeasible.
#' @return Magpie object with either filtered model data or the modelstatus.
#' @author David Klein
#' @importFrom magclass collapseNames as.magpie

mute_infes <- function(data,name="Modelstatus (-)",infeasible=5,return_infes=FALSE) {
  
  ##########################################################################
  ##### D A T A: set infeasible years and their successors to NA ###########
  ##########################################################################
  
  # mark infesible years of scenarios with TRUE
  infes <- FALSE
  for (i in infeasible) {
    tmp <- data["GLO",,name]==i
    infes <- tmp | infes
  }
  
  # keep the scenario even if it's only one
  infes <- collapseNames(infes, collapsedim = "variable")
  
  # set infeasible years to NA
  infes[infes]  <- NA
  # and feasible years to 0 (so that they don't affect the cumsum below)
  infes[!infes] <- 0
  # use cumsum to set all years after the first infeasible year to NA because results after an infeasible year are not meaningful
  infes <- as.magpie(apply(X = infes, MARGIN = c(1,3), FUN = cumsum))
  # set feasible years from 0 to 1 so that they don't change the results of the multiplication below)
  infes[!is.na(infes)] <- 1
  if (!return_infes) {
    # multiplying keeps only feasible years and sets infeasible years to NA
    data <- data*infes
    return(data)
  } else {
    return(infes)
  }
}

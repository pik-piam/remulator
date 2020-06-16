#' Replace fits in years where they are flat with fits from other years
#' 
#' This function considers fits with a slope less than 0.01 flat and tries to replace them with non-flat fits from other years.
#' 
#' @param path_to_postfit_Rdata Path to the Rdata file that contains raw data, fitted data and fitcoefficients saved by the emulator function
#' @param emu_path Path the emulator results have been saved to by the emulator function.
#' @param fitname Name that describes the fit (default: linear) and will be used for naming the output folders.
#' @author David Klein
#' @seealso \code{\link{emulator}} \code{\link{fill_missing_years}}
#' @importFrom magclass getNames write.magpie
#' @export

replace_flat_fits <- function(path_to_postfit_Rdata,emu_path="output/emulator",fitname="replaced-flat") {

  # path_to_postfit_Rdata="output/emulator/SSP2-26/linear/data_postfit_SSP2-26.Rdata"
  # emu_path="output/emulator"
  # fitname="replaced-flat"
  
  data     <- NULL
  filtered <- NULL
  userfun  <- NULL
  
  cat("Loading",path_to_postfit_Rdata,".\n")
  load(path_to_postfit_Rdata)
  zero <- fitcoef[,,"b"]<0.01
  
  # Find regions that have only flat fits and set their fitcoefficients to NA for all years.
  # All NAs will be replaced by MOINPUT with artificial fits (= high prices)
  all_flat <-apply(zero,1,all)
  all_flat[is.na(all_flat)] <- FALSE # if no fit available
  reg_all_flat <- names(all_flat)[all_flat]
  if(length(reg_all_flat)>0) {
    cat("No non-flat fit available for",reg_all_flat,". Setting fitcoefficients to NA.\n")
    fitcoef[reg_all_flat,,] <- NA
  }
  
  # replace fitcoefficients for years that have flat fits (b==0)
  cat("Replacing flat fits.\n")
  fitcoef <- fill_missing_years(fitcoef,zero)
  
  # Calculate number of fitcoefficients that have been replaced
  n <-sum(!is.na(attributes(fitcoef)$takenfrom))
  cat("Number of fits that are considered flat and have been replaced:",n,"\n")
  
  print(attributes(fitcoef)$takenfrom)
  
  # Save data to files
  scen     <- getNames(fitcoef,dim = "scenario")
  path_plots <- file.path(emu_path,scen,fitname)
  ifelse(!dir.exists(path_plots), dir.create(path_plots), FALSE)
  
  if (n>0) {
    f <- file.path(emu_path,scen,fitname,paste0("f30_bioen_price_",scen,"_replaced_flat.cs4r"))
    cat("Writing fit coefficients to textfile",f,".\n")
    write.magpie(fitcoef,file_name = f)
  
    f <- file.path(emu_path,scen,fitname,paste0("data_postfit_",scen,"_replaced_flat.Rdata"))
    cat("Saving data to",f,"\n")
    save(data,filtered,fitcoef,userfun,file = f)

    # Calculate and plot supplycurves
    cat("Calculating supplycurves.\n")
    supplycurve_commonY <- calc_supplycurve(data,fitcoef,myform=userfun)
    supplycurve_indiviY <- calc_supplycurve(data,fitcoef,myform=userfun,ylimit="individual")
    
    plot_curve(filtered[,,"raw",invert=TRUE],supplycurve_commonY,supplycurve_indiviY,infes=NA,emu_path=emu_path,fitname=fitname,create_pdf=FALSE)
    
    logfile <- file.path(emu_path,scen,fitname,paste0("replace-flat-fits-",scen,".log"))
    
  } else {
    
    cat("Replaced nothing. Stopping here.\n")
    logfile <- file.path(emu_path,scen,fitname,paste0("nothing-replaced-",scen,".log"))
    
  }

  sink(logfile)
  cat("Number of fits that are considered flat and have been replaced:",n,"\n")
  print(attributes(fitcoef)$takenfrom)
  sink()

}
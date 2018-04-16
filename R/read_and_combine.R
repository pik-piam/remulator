#' Read multiple report mif files from REMIND or MAgPIE and combine into single magpie object
#' 
#' This function reads model output from mif files from the provided list of folders. It also reads the GAMS
#' modelstatus from the fulldata.gdx and appends it to the report. If more than one folder is
#' given it combines the contents of the multiple reports into a single magpie object and returns it.
#' If the name of an output file is provided the object is additionally stored in this file. In each of the
#' folders there must be a report_scenname.mif, a fulldata.gdx, a spatial_header.rda, and config.Rdata.
#' 
#' 
#' @param list_of_directories Vector of strings providing the folder names from which reports will be read.
#' @param outfile Path to an output file which the output of the function can optionally be stored to.
#' @author David Klein
#' @importFrom magclass read.report add_columns mbind
#' @importFrom magpie modelstat
#' @importFrom madrat regionscode
#' @export

read_and_combine <- function(list_of_directories, outfile=NULL) {
  cat("Collecting results.\n")
  mag_res <- NULL
  regionscode_previous <- NULL
  
  for (f in list_of_directories) {
    # extract scenario name from path: any string between "/" and EOL ($) containing "-x" with x being a one or two digit number
    #scenario <- gsub(".*\\/(.*-[0-9]{1,2}$)","\\1",f)
    cfg <- NULL
    load(paste0(f,"/config.Rdata"))
    scenario <- cfg$title
    
    load(paste0(f,"/spatial_header.rda"))
    regionscode_current <- regionscode
    
    if (!is.null(regionscode_previous)) {
      if(regionscode_previous != regionscode_current) {
        stop("Region codes are not the same for all runs!")
      }
    }
    
    regionscode_previous <- regionscode_current
    
    # if region codes of the current and the previous run are identical (stop() above was not executed), keep current one
    cat("Regionscode:",regionscode_current,"\n")
    cat("Reading:",paste0(f,"/report.mif"),"\n")
    modelstat <- modelstat(paste0(f,"/fulldata.gdx"))
    report <- read.report(paste0(f,"/report.mif"),as.list=FALSE)
    # if MAgPIE was infeasible in a year the report misses all subsequent years. Thus, the report has less years than 
    # modelstat that was read from gdx. The gdx always has all years. Therefore, add the missing years to the report, so 
    # modelstat and report have the same years, and the data from all scenarios have the same years
    missing_years <- setdiff(getYears(modelstat),getYears(report))
    if (!identical(missing_years, character(0))) {
      report <- add_columns(report,dim=2.1,addnm=missing_years)
    }
    report <- add_columns(report,dim=3.3,addnm="Modelstatus (-)")
    report["GLO",,"Modelstatus (-)"] <- modelstat
    mag_res <- mbind(mag_res,report)
  }
  
  attr(mag_res,"regionscode") <- regionscode_current
  
  if (!is.null(outfile)) {
    # save collected results to Rdata file
    cat("Saving results to",outfile,"\n")
    save(mag_res,file=outfile)
  }  
  return(mag_res)
}

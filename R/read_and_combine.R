#' Read multiple report mif files from REMIND or MAgPIE and combine into single magpie object
#' 
#' This function reads model output from mif files from the provided list of folders. It also reads the GAMS
#' modelstatus from the fulldata.gdx and appends it to the report. If more than one folder is
#' given it combines the contents of the multiple reports into a single magpie object and returns it.
#' If the name of an output file is provided the object is additionally stored in this file. In each of the
#' folders there must be a report_scenname.mif, a fulldata.gdx, and config.Rdata.
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
  rcode <- NULL
  for (f in list_of_directories) {
    # extract scenario name from path: any string between "/" and EOL ($) containing "-x" with x being a one or two digit number
    #scenario <- gsub(".*\\/(.*-[0-9]{1,2}$)","\\1",f)
    cfg <- NULL
    load(paste0(f,"/config.Rdata"))
    scenario <- cfg$title
    if (!is.null(rcode)) {
      if(rcode != regionscode(paste0("./",cfg$regionmapping))) {
        stop("Region codes are not the same for all runs!")
      }
    }
    # if region codes of the current and the previous run are identical (stop() above was not executed), keep current one
    rcode <- regionscode(paste0("./",cfg$regionmapping))
    cat("Regionscode:",rcode,"\n")
    report <- read.report(paste0(f,"/report_",scenario,".mif"),as.list=FALSE)
    report <- add_columns(report,dim=3.3,addnm="Modelstatus (-)")
    report["GLO",,"Modelstatus (-)"] <- modelstat(paste0(f,"/fulldata.gdx"))
    mag_res <- mbind(mag_res,report)
  }
  
  attr(mag_res,"regionscode") <- rcode
  
  if (!is.null(outfile)) {
    # save collected results to Rdata file
    cat("Saving results to",outfile,"\n")
    save(mag_res,file=outfile)
  }  
  return(mag_res)
}
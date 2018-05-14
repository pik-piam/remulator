#' Calculate and plot multiple supplycurves to one plot
#' 
#' This function calculates supplycurves of multiple scenarios and plots them into one figure to compare them.
#' 
#' @param folders Vector giving the paths to the fitted data (usually within the 'output/emulator' folder)
#' @param pdfname If you want the figures to be compiled in a pdf provide the name of the file here
#' @author David Klein
#' @seealso \code{\link{emulator}}
#' @importFrom magclass getYears is.magpie
#' @importFrom ggplot2 ggplot ggsave aes_string facet_wrap theme_grey geom_line labs xlab ggtitle geom_point
#' @importFrom luplot gginput
#' @importFrom lusweave swopen swclose swfigure swlatex
#' @export

# compare multiple fits

plot_compare_supplycurves <- function(folders,pdfname=NULL) {

    path_comp <- file.path("comparison")
    cat("Creating folder",path_comp,"\n")
    ifelse(!dir.exists(path_comp), dir.create(path_comp), FALSE)
    
    if (!is.null(pdfname)) pdfname <- file.path(path_comp,pdfname)
    
    # read and combine the results of different scenarios
    folders <- gsub("/$","",folders)
    files <- paste0(folders,"/data_postfit_",folders,".Rdata")
    
    tmp_filtered <- NULL
    tmp_fitcoef <- NULL
    
    for (f in files) {
      cat("Loading file",f,"\n")
      load(f) # the file contains the variables "data", "filtered", "fitcoef", and "userfun"
      tmp_filtered <- mbind(tmp_filtered,filtered)
      tmp_fitcoef <- mbind(tmp_fitcoef,fitcoef)
    }
    
    filtered <- tmp_filtered
    fitcoef <- tmp_fitcoef
    
    rm(tmp_filtered,tmp_fitcoef,files,f)
    
    # calculate supplycurves for all scenarios
    
    cat("Calculating supplycurves\n")
    supplycurves <- calc_supplycurve(collapseNames(filtered[,,"fitted"],collapsedim = "type"),
                                            fitcoef,myform = function(param,x)return(param[[1]] + param[[2]] * x ),
                                            ylimit = "individual")
    
    f <- file.path(path_comp,"data_compare_supplycurves.Rdata")
    cat("Saving supplycurve data to",f,"\n")
    save(supplycurves,file = f)

    # plot supplycurves (all regions per year)

    if(!is.null(pdfname)) {
      template <-  c("\\documentclass[a4paper, portrait ]{article}",
                     "\\setlength{\\parindent}{0in}",
                     "\\usepackage{float}",
                     "\\usepackage[bookmarksopenlevel=section]{hyperref}",
                     "\\hypersetup{bookmarks=true,pdfauthor={PIK}}",
                     "\\usepackage{graphicx}",
                     "\\usepackage{rotating}",
                     "\\usepackage[strings]{underscore}",
                     "\\usepackage[margin=2cm]{geometry}",
                     "\\usepackage{fancyhdr}",
                     "\\pagestyle{fancy}",
                     "\\begin{document}",
                     "<<echo=false>>=",
                     "options(width=90)",
                     "@") 
      sw <- swopen(outfile = pdfname, template = template)
      swlatex(sw,c(paste0("\\title{Compare supplycurves}"),'\\author{Zentrum f\\"ur politische Sch\\"onheit}',"\\maketitle","\\tableofcontents"))
      swlatex(sw,"\\section{Supplycurve per year}")
    }
    
    for(y in getYears(supplycurves)){
      cat("Creating figures for ",y," and saving them to folder '",path_comp,"'\n",sep="")
      if (!is.null(pdfname)) swlatex(sw,paste0("\\subsection{",y,"}"))
      dat <- gginput(supplycurves[,y,], scatter = "type",verbose = FALSE)
      p <- ggplot(dat, aes_string(x=".value.x",y=".value.y")) + geom_line(aes_string(colour="scenario")) + facet_wrap(~.spat1 ,scales = "fixed") +
        geom_point(data=gginput(filtered["GLO",,,invert=TRUE][,y,][,,"fitted"],scatter = "variable",verbose = FALSE),aes_string(x=".value.x",y=".value.y",colour="scenario"),size=1) +
        theme_grey(base_size = 6) + labs(title = y, y ="$/GJ", x = "EJ")
      ggsave(filename = file.path(path_comp,paste0(y,".png")),plot=p,width=10,height=6)
      if (!is.null(pdfname)) swfigure(sw,print,p,fig.width=1)
    }
    
    if (!is.null(pdfname)) {
      # delete temporary files created by knitr
      cat("Printing pdf to",pdfname,"\n")
      swclose(sw,clean_output = TRUE)
      unlink(c(paste0(path_comp,"/figure"),
               paste0(path_comp,"/",gsub("\\.pdf$",".log",pdfname)),
               paste0(path_comp,"/",gsub("\\.pdf$",".rda",pdfname))),recursive=TRUE, force=TRUE)
    }
}
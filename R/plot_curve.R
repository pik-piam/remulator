#' Plot fitted curve to png files and additionally compile them in a pdf file.
#' 
#' This function produces plot showing the raw data and the fittet curves. It saves it to a folder named after the
#' scenario and it produces a pdf containing all the figures.
#' 
#' @param raw MAgPIE object containing the same raw data that was used to calculate the fitcoefficients.
#' @param supplycurve MAgPIE object containing the points of the curve (output of \code{\link{calc_supplycurve}})
#' @param infes MAgPIE object containing the modelstatus (optional output of \code{\link{mute_infes}})
#' @param emu_path Name of the folder the figures and pdf will be saved to.
#' @author David Klein
#' @seealso \code{\link{calc_supplycurve}} \code{\link{mute_infes}}
#' @importFrom magclass getNames getYears
#' @importFrom ggplot2 ggplot ggsave
#' @importFrom luplot gginput
#' @importFrom lusweave swopen swclose swfigure swlatex
#' @importFrom grDevices colorRampPalette
#' @importFrom stats na.omit

plot_curve <- function(raw, supplycurve, infes, emu_path="emulator") {

  ifelse(!dir.exists(emu_path), dir.create(emu_path), FALSE)
  
  for (scen in getNames(supplycurve,dim=1)) {
    path_plots <- file.path(emu_path,scen)
    ifelse(!dir.exists(path_plots), dir.create(path_plots), FALSE)
    outfile <- file.path(emu_path,paste0(scen,"_emulator.pdf"))
    
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
    sw <- swopen(outfile = outfile, template = template)
    swlatex(sw,c(paste0("\\title{MAgPIE emulator for ",scen,"}"),'\\author{Zentrum f\\"ur politische Sch\\"onheit}',"\\maketitle","\\tableofcontents"))
    
    #==== P L O T: infeasible years ====
    
    swlatex(sw,"\\section{Modelstat}")
    # heatmap(i, Colv = NA, Rowv = NA,col=c("#df2424", "#4cce0f"))
    dat <- as.ggplot(infes[,,scen])
    dat$Value[is.na(dat$Value)] <- 0 # for nicer plot replace NA with 0
    modelstat <- ggplot(dat,aes(x = Data2, y=Year))  + geom_tile(aes(fill = Value), colour = "white") + 
      scale_fill_gradient(low = "#df2424", high = "#4cce0f") + xlab("Scenario") + ggtitle(scen) + 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0,0)) + theme_bw(base_size = 6)
    ggsave(filename = file.path(path_plots,paste0("modelstat-",scen,".png")),plot=modelstat,width=6,height=3)
    swfigure(sw,print,modelstat)
    
    #==== P L O T: raw data ====
    
    swlatex(sw,"\\section{Raw data}")
    dat <- na.omit(gginput(raw[,,]["GLO",,"modelstat",invert=TRUE],scatter = "variable"))
    scatter_raw <- ggplot(dat, aes(x=.value.x,y=.value.y,color=scenario)) + geom_point(size=0.3) +
      facet_grid(.temp1~.spat1 ,scales = "fixed") + labs(title = "Raw data of feasible runs", y ="$/GJ", x = "EJ") +
      theme_minimal(base_size = 6) # + ylim(0, 50)
    ggsave(filename = file.path(path_plots,paste0("scatter-raw-all.png")),plot=scatter_raw,width=10,height=6)
    swfigure(sw,print,scatter_raw,fig.width=1)
    
    #==== Overviewplot Supplycurves 1/2 ====
    
    swlatex(sw,"\\section{Supplycurve matrix}")
    dat <- gginput(supplycurve[,,scen], scatter = "type")
    p <- ggplot(dat, aes(x=.value.x,y=.value.y)) +
      geom_point(data=gginput(raw["GLO",,"modelstat",invert=TRUE][,,scen],scatter = "variable"),aes(x=.value.x,y=.value.y),size=1) +
      geom_line(aes(colour=scenario)) + labs(y ="$/GJ", x = "EJ") + 
      theme_grey(base_size = 6) + facet_grid(.temp1~.spat1 ,scales = "fixed")
    ggsave(filename = file.path(path_plots,paste0("scatter-fit-",scen,".png")),plot=p,width=10,height=6)
    swfigure(sw,print,p,fig.width=1)
    
    #==== Overviewplot Supplycurves 2/2 ====
    
    #Define colors for years
    color_years <- colorRampPalette(c("blue", "yellow", "red"))(length(getYears(raw)))
    names(color_years) <- gsub("y","",getYears(raw))
    
    swlatex(sw,"\\section{Supplycurve all years}")
    dat <- gginput(supplycurve[,,scen], scatter = "type")
    dat$year <- as.character(dat$year)
    dat_scatter <- gginput(raw["GLO",,"modelstat",invert=TRUE][,,scen],scatter = "variable")
    dat_scatter$year <- as.character(dat_scatter$year)
    p <- ggplot(dat, aes(x=.value.x,y=.value.y)) + 
      geom_point(data=dat_scatter,aes(x=.value.x,y=.value.y,color=year),size=1) +
      geom_line(aes(colour=year)) + facet_wrap(~.spat1 ,scales = "free") + theme_gray(base_size = 6) +
      scale_colour_manual(values=color_years) + labs(title = scen, y ="$/GJ", x = "EJ")
    ggsave(filename = file.path(path_plots,paste0("scatter-fit-allyears-",scen,".png")),plot=p,width=10,height=6)
    swfigure(sw,print,p,fig.width=1)
    
    #==== Supplycurves for each year ====
    
    swlatex(sw,"\\section{Supplycurve per year}")
    for(y in getYears(supplycurve)){
      swlatex(sw,paste0("\\subsection{",y,"}"))
      dat <- gginput(supplycurve[,y,scen], scatter = "type")
      p <- ggplot(dat, aes(x=.value.x,y=.value.y)) + geom_line(aes(colour=scenario)) + facet_wrap(~.spat1 ,scales = "fixed") +
        geom_point(data=gginput(raw["GLO",,"modelstat",invert=TRUE][,y,scen],scatter = "variable"),aes(x=.value.x,y=.value.y),size=1,color="gray") +
        theme_grey(base_size = 6) + labs(y ="$/GJ", x = "EJ")
      ggsave(filename = file.path(path_plots,paste0("scatter-fit-",scen,"-",y,".png")),plot=p,width=10,height=6)
      swfigure(sw,print,p,fig.width=1)
    }
    
    # delete temporary files created by knitr
    swclose(sw)
    unlink(c(paste0(emu_path,"/figure"),
             paste0(emu_path,"/",scen,"_emulator.log"),
             paste0(emu_path,"/",scen,"_emulator.rda")),recursive=TRUE, force=TRUE)
    
  } # end scenario loop  
}

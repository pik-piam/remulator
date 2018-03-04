#' Plot fitted curve to png files and additionally compile them in a pdf file.
#' 
#' This function produces plot showing the raw data and the fittet curves. It saves it to a folder named after the
#' scenario and it produces a pdf containing all the figures.
#' 
#' @param raw MAgPIE object containing the same raw data that was used to calculate the fitcoefficients.
#' @param supplycurve_commonY MAgPIE object containing the points of the curve (with common y limit) (output of \code{\link{calc_supplycurve}})
#' @param supplycurve_indiviY MAgPIE object containing the points of the curve (with individual y limit) (output of \code{\link{calc_supplycurve}})
#' @param infes MAgPIE object containing the modelstatus (optional output of \code{\link{mute_infes}})
#' @param emu_path Name of the folder the figures and pdf will be saved to.
#' @param create_pdf Logical indicating whether a pdf should be produced that compiles all figures.
#' @author David Klein
#' @seealso \code{\link{calc_supplycurve}} \code{\link{mute_infes}}
#' @importFrom magclass getNames getYears
#' @importFrom ggplot2 ggplot ggsave aes_string geom_tile scale_fill_gradient scale_x_discrete scale_y_discrete theme_bw ggsave facet_wrap theme_gray geom_line scale_colour_manual labs theme_grey xlab ggtitle geom_point facet_grid theme_minimal theme
#' @importFrom luplot gginput as.ggplot
#' @importFrom lusweave swopen swclose swfigure swlatex swtable
#' @importFrom grDevices colorRampPalette
#' @importFrom stats na.omit

plot_curve <- function(raw, supplycurve_commonY, supplycurve_indiviY, infes, emu_path="emulator", create_pdf=TRUE) {

  ifelse(!dir.exists(emu_path), dir.create(emu_path), FALSE)
  
  for (scen in getNames(supplycurve_commonY,dim=1)) {
    path_plots <- file.path(emu_path,scen)
    ifelse(!dir.exists(path_plots), dir.create(path_plots), FALSE)
    cat("Saving plots to",path_plots,"\n")
    outfile <- file.path(emu_path,paste0(scen,"_emulator.pdf"))
    
    if(create_pdf) {
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
    }

    #==== P L O T: infeasible years ====
    if (any(!is.na(infes))) {
      if (create_pdf) swlatex(sw,"\\section{Modelstat}")
      
      # heatmap(i, Colv = NA, Rowv = NA,col=c("#df2424", "#4cce0f"))
      dat <- as.ggplot(infes["GLO",,scen])
      dat$Value[is.na(dat$Value)] <- 0 # for nicer plot replace NA with 0
      modelstat <- ggplot(dat,aes_string(x = "Data2", y="Year"))  + geom_tile(aes_string(fill = "Value"), colour = "white") + 
        scale_fill_gradient(low = "#df2424", high = "#4cce0f") + xlab("Scenario") + ggtitle(scen) + 
        scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0,0)) + theme_bw(base_size = 6) + theme(legend.position="none")
      ggsave(filename = file.path(path_plots,paste0("modelstat-",scen,".png")),plot=modelstat,width=6,height=3)
      if (create_pdf) swfigure(sw,print,modelstat)
    }
    
    #==== P L O T: raw data ====
    
    if (create_pdf) swlatex(sw,"\\section{Raw data of feasible runs}")
    dat <- na.omit(gginput(raw[,,scen]["GLO",,,invert=TRUE],scatter = "variable",verbose = FALSE))
    scatter_raw <- ggplot(dat, aes_string(x=".value.x",y=".value.y",color="type")) + geom_point(size=0.3) +
      theme_minimal(base_size = 6) + facet_grid(.temp1~.spat1 ,scales = "fixed") + labs(y ="$/GJ", x = "EJ")
    ggsave(filename = file.path(path_plots,paste0("scatter-raw-all.png")),plot=scatter_raw,width=10,height=6)
    if (create_pdf) swfigure(sw,print,scatter_raw,fig.width=1)
    
    #==== Overviewplot Supplycurves 1/2 ====
    
    if (create_pdf) swlatex(sw,"\\section{Supplycurve matrix}")
    dat <- gginput(supplycurve_commonY[,,scen], scatter = "type",verbose = FALSE)
    p <- ggplot(dat, aes_string(x=".value.x",y=".value.y")) +
      geom_point(data=gginput(raw["GLO",,,invert=TRUE][,,scen][,,"fitted"],scatter = "variable",verbose = FALSE),aes_string(x=".value.x",y=".value.y"),size=0.3) +
      geom_line(aes_string(colour="scenario")) + 
      theme_minimal(base_size = 6) + facet_grid(.temp1~.spat1 ,scales = "fixed") + labs(y ="$/GJ", x = "EJ") + theme(legend.position="none")
    ggsave(filename = file.path(path_plots,paste0("scatter-fit-",scen,".png")),plot=p,width=10,height=6)
    if (create_pdf) swfigure(sw,print,p,fig.width=1)
    
    #==== Overviewplot Supplycurves 2/2 ====
    
    #Define colors for years
    color_years <- colorRampPalette(c("blue", "yellow", "red"))(length(getYears(raw)))
    names(color_years) <- gsub("y","",getYears(raw))
    
    if (create_pdf) swlatex(sw,"\\section{Supplycurve all years}")
    dat <- gginput(supplycurve_commonY[,,scen], scatter = "type",verbose = FALSE)
    dat$year <- as.character(dat$year)
    dat_scatter <- gginput(raw["GLO",,,invert=TRUE][,,scen][,,"fitted"],scatter = "variable",verbose = FALSE)
    dat_scatter$year <- as.character(dat_scatter$year)
    p <- ggplot(dat, aes_string(x=".value.x",y=".value.y")) + 
      geom_point(data=dat_scatter,aes_string(x=".value.x",y=".value.y",color="year"),size=1) +
      geom_line(aes_string(colour="year")) + facet_wrap(~.spat1 ,scales = "free") + theme_gray(base_size = 6) +
      scale_colour_manual(values=color_years) + labs(title = scen, y ="$/GJ", x = "EJ")
    ggsave(filename = file.path(path_plots,paste0("scatter-fit-allyears-",scen,".png")),plot=p,width=10,height=6)
    if (create_pdf) swfigure(sw,print,p,fig.width=1)
    
    # the same but with fixed scales
    p <- ggplot(dat, aes_string(x=".value.x",y=".value.y")) + 
      geom_point(data=dat_scatter,aes_string(x=".value.x",y=".value.y",color="year"),size=1) +
      geom_line(aes_string(colour="year")) + facet_wrap(~.spat1 ,scales = "fixed") + theme_gray(base_size = 6) +
      scale_colour_manual(values=color_years) + labs(title = scen, y ="$/GJ", x = "EJ")
    ggsave(filename = file.path(path_plots,paste0("scatter-fit-allyears-fixed-",scen,".png")),plot=p,width=10,height=6)
    if (create_pdf) swfigure(sw,print,p,fig.width=1)

    #==== Supplycurves for each year ====
    
    if (create_pdf) swlatex(sw,"\\section{Supplycurve per year}")
    for(y in getYears(supplycurve_commonY)){
      if (create_pdf) swlatex(sw,paste0("\\subsection{",y,"}"))
      dat <- gginput(supplycurve_commonY[,y,scen], scatter = "type",verbose = FALSE)
      p <- ggplot(dat, aes_string(x=".value.x",y=".value.y")) + geom_line(aes_string(colour="scenario")) + facet_wrap(~.spat1 ,scales = "fixed") +
        geom_point(data=gginput(raw["GLO",,,invert=TRUE][,y,scen][,,"fitted"],scatter = "variable",verbose = FALSE),aes_string(x=".value.x",y=".value.y"),size=1,color="gray") +
        theme_grey(base_size = 6) + labs(title = y, y ="$/GJ", x = "EJ") + theme(legend.position="none")
      ggsave(filename = file.path(path_plots,paste0("scatter-fit-",scen,"-",y,".png")),plot=p,width=10,height=6)
      if (create_pdf) swfigure(sw,print,p,fig.width=1)
    }
    
    #==== Supplycurves for each region ====
    
    if (create_pdf) swlatex(sw,"\\section{Supplycurve per region}")
    for(r in getRegions(supplycurve_indiviY)){
      if (create_pdf) swlatex(sw,paste0("\\subsection{",r,"}"))
      dat <- gginput(supplycurve_indiviY[r,,scen], scatter = "type",verbose = FALSE)
      dat_raw <- gginput(raw["GLO",,,invert=TRUE][r,,scen],scatter = "variable",verbose = FALSE)
      
      # @importFrom dplyr filter %>% group_by summarize ungroup inner_join
      # # Remove values from supplycurve that are greater than the underlying raw data
      # # 1. find maximum x value of raw data for each region, year, and scenario
      # lim <-dat_raw %>% 
      #   group_by(region,scenario,year) %>% 
      #   summarize(maxi = max(.value.x,na.rm=TRUE)) %>%
      #   ungroup()
      # 
      # # 2. Add maxi column to dat and keep values only that are below max
      # dat <- inner_join(dat,lim,by=c("region"="region","scenario"="scenario","year"="year")) %>% 
      #   filter(.value.x < maxi)

      p <- ggplot(dat, aes_string(x=".value.x",y=".value.y")) + geom_line(aes_string(colour="scenario")) + facet_wrap(~.temp1 ,scales = "free") +
        geom_point(data=dat_raw[dat_raw$type=="fitted",],aes_string(x=".value.x",y=".value.y"),size=1,color="black") +
        geom_point(data=dat_raw[dat_raw$type!="fitted",],aes_string(x=".value.x",y=".value.y",color="type"),alpha=0.5,size=1) +
        theme_grey(base_size = 6) + labs(title = r, y ="$/GJ", x = "EJ")# + theme(legend.position="none")
      ggsave(filename = file.path(path_plots,paste0("scatter-fit-",scen,"-",r,".png")),plot=p,width=10,height=6)
      if (create_pdf) swfigure(sw,print,p,fig.width=1)
    }

    if (create_pdf) {
      # delete temporary files created by knitr
      cat("Printing pdf to",outfile,"\n")
      swclose(sw)
      unlink(c(paste0(emu_path,"/figure"),
               paste0(emu_path,"/",scen,"_emulator.log"),
               paste0(emu_path,"/",scen,"_emulator.rda")),recursive=TRUE, force=TRUE)
    }
    
  } # end scenario loop  
}

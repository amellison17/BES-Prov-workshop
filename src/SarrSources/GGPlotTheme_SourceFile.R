#ggplot2 theme &c for Scaling Sarracenia book
# 5 June 2018
# NJG
# Updated 1 December 2018
# AME
#----------------------------------------------
# this sourcing file lives in the root of the Draft Figures Folder
# in all of the subfolders for individual chapters, source this file as follows for any scripts used to generate figures:

# source("../GGPlotTheme_SourceFile.R")
#----------------------------------------------


library(ggplot2)
library(ggthemes)
library(extrafont)


#be sure to have run ttf_import() once after installing extrafont (not necessary on mac)
#to install system fonts into R
#We need Gill Sans Monotype

#be sure to loadfonts on windowos after running ttf_import
#only need to do this once per machine after upgrade
#loadfonts(device="win")

#Our theme_scalesarr() is modified from the theme_tufte in ggthemes and theme_tufte_revised from
#https://github.com/jrnold/ggthemes/issues/33
#which itself is a modification of theme_bw
#which itself is a modification of theme_grey

theme_scalesarr <- function(base_size = 10, base_family="Gill Sans MT", ticks=TRUE) {

  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
          axis.line = element_line(color = 'black', size=.2),
		axis.text = element_text(size=rel(1.1), colour="black"),
		axis.title = element_text(size=rel(1.25), colour="black"),
		axis.ticks = element_line(colour="black", size=.2),
		axis.ticks.length = unit(1.5, "mm"),
          legend.background = element_blank(), 
          legend.key = element_blank(), 
          legend.title = element_text(face="plain"),
          panel.background = element_blank(), 
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank()
    )

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }

  ret
}

#########################################################
#Use theme_scalesarr_facet for faceted plots
#########################################################

theme_scalesarr_facet <- function(base_size = 10, base_family="Gill Sans MT", ticks=TRUE) {

  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
          axis.line = element_line(color = 'black', size=.2),
		axis.text = element_text(size=rel(1.1), colour="black"),
		axis.title = element_text(size=rel(1.25), colour="black"),
		axis.ticks = element_line(colour="black", size=.2),
		axis.ticks.length = unit(1.5, "mm"),
          legend.background = element_blank(), 
          legend.key = element_blank(), 
          #legend.title = element_text(face="plain"),
		legend.position="none",
          panel.background = element_blank(), 
          panel.border = element_rect(fill=NA),
          panel.grid = element_line(colour="grey"),
          plot.background = element_blank(),
          strip.background = element_rect(fill=NA)
    )

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }

  ret
}

 
#export using cairo_pdf() to get the fonts right

#########################################################

#########################################################
#Here's an example:
	
#a <- rnorm(100,0,1)
#b <- rnorm(100,1,3)
#ab <-data.frame(a,b)
#names(ab) <- c("Alpha", "Beta")

#cairo_pdf("testplot.pdf", width=5, height=7)
#ggplot(ab, aes(x=Alpha, y=Beta)) +
#	geom_point() +
#	theme_scalesarr()
#dev.off()

#end example
##########################################################


theme_scalesarr_ternary <- function(base_size = 10, base_family="Gill Sans MT", ticks=TRUE) {

  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
          axis.line = element_line(color = 'black', size=.2),
		axis.text = element_text(size=rel(1.1), colour="black"),
		axis.title = element_text(size=rel(1.25), colour="black"),
		axis.ticks = element_line(colour="black", size=.2),
		axis.ticks.length = unit(1.5, "mm"),
          legend.background = element_blank(), 
          legend.key = element_blank(), 
          #legend.title = element_text(face="plain"),
          panel.background = element_blank(), 
          panel.border = element_blank(),
          #panel.grid = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank(),
		tern.panel.grid.major.show=TRUE,
		legend.position="none"
		
    )

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }

  ret
}

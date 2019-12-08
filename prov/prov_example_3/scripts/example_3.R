# updated versions of figure7_2 (stage distributions)
# and figure7_3 (stage numbers through time)
# 5 June 2018
# NJG
#-------------------------------------
## Calculation of basic demographic stats and dirichelet sampling of uncertainty
## from the basic demographic models presented in:

# Gotelli, N.J. and A.M. Ellison. 2002. Nitrogen deposition and extinction risk in the northern pitcher plant, Sarracenia purpurea. Ecology 83: 2758-2765.

# Gotelli, N.J. and A.M. Ellison. 2006. Forecasting extinction risk with non-stationary matrix models. Ecological Applications 16: 51-61.

# 24 October 2017
# NJG

# Load Libraries ----------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(popbio)
library(gtools)
library(reshape2)

# Source Files ------------------------------------------------------------
source("SarrSources/DemoSimFunctions.R")
source("SarrSources/GGPlotTheme_SourceFile.R")
# set.seed(136)

# Read Data ---------------------------------------------------------------
# NA

# -------------------------------------------------------------------------
# Global variables
nStage <- 4 # number of stages in matrix
nStep <- 100 # length of time series
nRep <- 1000 # number of replicated stochastic populations
n0 <- c(1500,23500,1400,500)

# Transitions for Molly demo survey
A_Molly <- matrix(data=c(0, 0, 0, 4,
                    0.1, 0.8540, 0.1770, 0,
                    0, 0.1310, 0.7080, 0.6660,
                    0, 0, 0.1000, 0.3070),
             nrow=nStage,
             byrow=TRUE)
# Transitions for Hawley demo survey
A_Hawley <- matrix(data=c(0, 0, 0, 4,
                    0.1, 0.9540, 0.0900, 0,
                    0, 0.0360, 0.7010, 0.8375,
                    0, 0, 0.1802, 0.1610),
             nrow=nStage,
             byrow=TRUE)

# Estimates from aerial photo and ground sampling of the numbers of Sarracenia across the entire area of Hawley bog
ObsSizeHawley <- c(1500,23500,1400,500)

## Basic deterministic stats
MollyD <- eigen.analysis(A_Molly)
HawleyD <-eigen.analysis(A_Hawley)
#print(MollyD)
#print(HawleyD)

## Create data frame for graph of observed and predicted

rowLabs <- rep(c("Recruits", "Juveniles", "NF Adults", "F Adults"),3)
groups <- rep(c("Molly(E)","Hawley(E)","Hawley(O)"),each=4)
RelFreq <- c(n0/sum(n0),MollyD$stable.stage,HawleyD$stable.stage)
StageFitData <- data.frame(groups,rowLabs,RelFreq)

## reorder the factors in the proper way
StageFitData$rowLabs <- factor(StageFitData$rowLabs, level= c("Recruits","Juveniles","NF Adults", "F Adults"))
StageFitData$groups <- factor(StageFitData$groups, level= c("Hawley(O)", "Hawley(E)", "Molly(E)"))                               

## Create ggplot graph


# Fig7.2 <-  ggplot(data=StageFitData,aes(x=rowLabs,y=RelFreq,fill=groups)) +
#   geom_bar(position="dodge",stat="identity",color="black") +
#   #scale_fill_manual(values=c("#41AB5D", "#737373","grey80")) +
#  # scale_fill_manual(values=c("#41AB5D", "gray75","grey90")) +
#   scale_fill_grey() +
#   labs(x="Stage",y="Relative Proportion") +
#   ylim(0,1) + 
#   guides(fill=guide_legend(title=NULL)) + 
#   theme_scalesarr()
# 
#  cairo_pdf("figure7_2.pdf", width=5, height=7)
# print(Fig7.2)
# dev.off()
  #theme_grey(base_size=15)
#print(Fig7.2)
#ggsave(filename="Figure7_2.jpg", plot=Fig7.2,width=7,height=6)
###################################################

z <- ConstantPopIter(n0=n0,A=A_Hawley,steps=nStep) # run deterministic model

head(z)
######################################################
# use ggplot to kick out a graph and visualize results
                   
  cairo_pdf("../graphics/figure7_3.pdf", width=5, height=7)
 Fig7.3 <- ggplot(data=z, aes(x=TimeStep, y=N,color=Stage)) +
    geom_line(size=0.5) +
   xlim(0,50) + 
   ylim(0,1) +
      xlab("Year") +
      ylab("Relative Proportion") +
   scale_color_grey() +
 theme_scalesarr()
 print(Fig7.3)
 dev.off()
# 
# 
# # theme_gray(base_size=15)
# #ggsave(filename="Figure7_3.jpg", plot=Fig7.3,width=7,height=6)
# # print(Fig7.3)
# # zOut <- replicate(n=nRep,GetSummary(PopIter(n0=n0,At=lapply(X=NitGen(nStep),FUN=NitMat,m=DataSub))))

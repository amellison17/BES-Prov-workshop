#Set of functions to install (if necessary) and load required packages for provenance workshop

#start with a clean workspace

rm(list=ls()) #Removes all objects from working environment

#create function for installing missing packages

is.installed <- function(pkglist){
	is.element(pkglist, installed.packages()[,1])
}

#source files for loading prov libraries

CRANlist <- c("rdtLite", "devtools", "provParseR", "provViz", "provSummarizeR") #packages on CRAN
GITlist <- c("provGraphR", "provDebugR", "provExplainR") #packages on github

#(Install) and load libraries available on CRAN
for(i in 1:length(CRANlist)) {
	if(!is.installed(CRANlist[i])){
		install.packages(CRANlist[i])
	}
	require(CRANlist[i], character.only=TRUE)
}

#(Install) libraries in development on GitHub
for(i in 1:length(GITlist)) {
	if(!is.installed(GITlist[i])){
		devtools::install_github(paste("End-to-end-provenance/",GITlist[i], sep=""), quiet=TRUE)
	}
	require(GITlist[i], character.only=TRUE)
}

if(!is.installed("provClean")){
	devtools::install_github("End-to-end-provenance/provClean", ref="dev", quiet=TRUE)
}
require(provClean)

#load packages for species occurrence analysis (example 3b)

#8 December 2019
#Updating rdtLite on the fly to deal with I/O errors with spocc
remove.packages("rdtLite")
install.packages("tarball/rdtLite_1.2.1.tar.gz")
library(rdtLite)

OCCList = c("spocc", "mapr") 

#(Install) and load libraries available for occurrence data
for(i in 1:length(OCCList)) {
	if(!is.installed(OCCList[i])){
		install.packages(OCCList[i])
	}
	require(OCCList[i], character.only=TRUE)
}


#load packages for SDM modeling (not needed for workshop)

#SDMList=c("dismo", "ecospat", "raster", "maptools","ENMeval", "spdep",
#	"rworldmap", "foreach", "doParallel", "pbapply", "rJava")

#(Install) and load libraries available for SDM data and modeling
#for(i in 1:length(SDMList)) {
#	if(!is.installed(SDMList[i])){
#		install.packages(SDMList[i])
#	}
#	require(SDMList[i], character.only=TRUE)
#}



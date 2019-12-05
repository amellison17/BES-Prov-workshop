#Set of functions to install (if necessary) and load required packages for provenance

#start with a clean workspace

rm(list=ls()) #Removes all objects from working environment

#create function for installing missing packages

is.installed <- function(pkglist){
	is.element(pkglist, installed.packages()[,1])
}

#source files for loading prov libraries

source("provSummarizeR.R") #newer version of provSummarizeR (5-Dec-2019) to capture console prov

CRANlist <- c("rdtLite", "devtools", "provParseR", "provSummarizeR", "provViz") #packages on CRAN
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




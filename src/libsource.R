#start with a clean workspace

rm(list=ls()) #Removes all objects from working environment

#source files for loading prov libraries

require(rdtLite) #RDataTracker

require(devtools) #Development versions of other RDT libraries
install_github("End-to-end-provenance/provGraphR")
install_github("End-to-end-provenance/provDebugR")
install_github("End-to-end-provenance/provExplainR")
install_github("End-to-end-provenance/provClean")


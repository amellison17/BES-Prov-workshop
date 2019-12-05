# BES 2019 workshop on data provenance and reproducible code

## Advance preparation

If your bringing a laptop (helpful, but not required), please ensure that your software is up-to-date and you have installed required packages either from CRAN or GitHub

* Download and install the current version of R from `https:\\www.r-project.org\`. For the 10 December 2019 workshop, the current version is 3.6.1. 
* Download and install the current version of RStudio from `https:\\rstudio.com\`. For the 10 December 2019 workshop, the current version is 1.2.5019. (I note that on one of my desktops, that 1.2.5019 does not work. But 1.1463 does).
* Update all packages: (`update.packages(checkBuilt=TRUE, ask=FALSE)
* Install R package rdtLite and its dependencies (`install.packages("rdtLite", dependencies=TRUE)`
* Install R packages `provGraphR`, `provDebugR`, `provExplainR`, and `provClean` from Github: `devtools::install_github("End-to-end-provenance/<name-of-package>"` where `<name-of-package>` is each of these packages. Note that you must have already installed the R `devtools` package for this to work.
* If you don't have Java on your computer, please install it. Java can be downloaded here: `https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html`. Note that you have to "Accept the license agreement" and create an Oracle account before you can download Java

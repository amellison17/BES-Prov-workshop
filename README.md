# BES 2019 workshop on data provenance and reproducible code

## Rationale and goals

The transparent, reproducible analyses essential for rapid scientific progress in ecology require two steps that have been embraced by ecologists: making data publicly accessible and providing open-source code for their analyses. But author-provided code often is difficult to re-use because it is poorly documented or won’t run on newer operating systems or software versions. Software tools based on collection of *data provenance* enable creation of well-documented, clean (debugged) code that can be run more easily by secondary users.

Workshop participants will
* Learn about data provenance;
* Use provenance-based tools to create clean, reproducible code and improve their understanding of scientific workflows.

## Advance preparation

If your bringing a laptop (helpful, but not required), please ensure that your software is up-to-date and you have installed required packages either from CRAN or GitHub

* Download and install the current version of R from `https:\\www.r-project.org\`. For the 10 December 2019 workshop, the current version is 3.6.1. 
* Download and install the current version of RStudio from `https:\\rstudio.com\`. For the 10 December 2019 workshop, the current version is 1.2.5019. (I note that on one of my desktops, that 1.2.5019 does not work. But 1.1463 does).
* Update all packages: (`update.packages(checkBuilt=TRUE, ask=FALSE)
* Install R package rdtLite and its dependencies (`install.packages("rdtLite", dependencies=TRUE)`
* Install R packages `provGraphR`, `provDebugR`, `provExplainR`, and `provClean` from Github: `devtools::install_github("End-to-end-provenance/<name-of-package>"` where `<name-of-package>` is each of these packages. Note that you must have already installed the R `devtools` package for this to work.
* If you don't have Java on your computer, please install it. Java can be downloaded here: `https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html`. Note that you have to "Accept the license agreement" and create an Oracle account before you can download Java

##Agenda

1. Ice-breakers (*5 minutes*)
2. Trust in data and code (*Sticky-note exercise, group discussion*)
3. The value of provenance (*Presentation*)

BioBreak

4. Recap & Questions
5. Generating provenance and using provenance-based tools using <https://end-to-end-provenance.github.io>
    1. ***rdtLite*** collects basic provenance of an R script (or console session) and saves to `prov.json`
        1. ***provParseR*** parses the `prov.json` into an R object for use by other prov-routines. Required by *provSummarizeR, provExplainR, provDebugR,* and *provClean*.
        2. *provGraphR* creates an adjacency matrix from the provenance object created by *provParseR* to allow for its rapid processing and use. Required by *provDebugR* and *provClean*.
    2. ***provSummarizeR*** creates a concise summary of the computing environment and "coarse-grained" provenance collected by *rdtLite*. This includes:
        1. Computing environment;
        2. Libraries loaded in the script;
        3. User-written functions "sourced" in the script;
        4. Data inputs and outputs;
        5. Console information (only if `prov.run()` was executed);
        6. Errors (only if `prov.run()` was executed.
    3. ***provViz*** is an R interface to a Java-based visualization tool to view and query the provenance graph. Requires Java.
    4. *provDebugR* is a "time-travelling" debugger that allows for rapid testing and debugging of R scripts.
    5. *provExplainR* uses provenance collected on two different executions of a single script and explains why the results differ.
    6. *provClean* uses provenance to simplify a script.
6. Hands-on work
    1. `/src/example_1.R`
    


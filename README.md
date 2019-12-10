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
* Update all packages: (`update.packages(checkBuilt=TRUE, ask=FALSE)`)
* Install R packages
    * This can be done by executing the script `\src\libsource.R` on this repo, or by the following steps:
        * Install R package rdtLite and its dependencies (`install.packages("rdtLite", dependencies=TRUE)`
        * Install R packages `provGraphR`, `provDebugR`, `provExplainR`, from GitHub: `devtools::install_github("End-to-end-provenance/<name-of-package>"` where `<name-of-package>` is each of these packages. Note that you must have already installed the R `devtools` package for this to work.
        * ***Update 10 December 2019:*** For example 3b, you will need the GitHub version of `rdtLite`. Execute the following sequence of commands:
            1. `remove.packages("rdtLite")`
            2. `devtools::install_github("End-to-end-provenance/rdtLite")`
        * Install R package `provClean` from a development branch on GitHub `devtools::install_github("End-to-end-provenance/provClean", ref="dev")`
* If you don't have Java on your computer, please install it. Java can be downloaded here: `https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html`. Note that you have to "Accept the license agreement" and create an Oracle account before you can download Java
* Last, if you want to work with the scripts, data, *etc.* posted on GitHub, you will need a GitHub account, and you may want to install GitHub desktop for easier tools to work with GitHub:
    * Create a GitHub account at <https://github.com/>
    * Download and install GitHub desktop from <https://desktop.github.com> (Current version is 2.2.3)

## Agenda

### 14:00 Introductions

1. Ice-breakers (*5 minutes*)
2. Trust in data and code (*Sticky-note exercise, group discussion*)
3. The what and why of provenance (*Presentation*)

### BioBreak

### 15:00 Hands-on (I)

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
    6. *provClean* (*alpha* development version) uses provenance to simplify a script.
6. Hands-on work (1)
    1. `libsource.R` sets up clean work space, installs packages, loads libraries.
    2. `/src/example_1.R` is a first example using R's "cars" data to illustrate the simplicity and complexity of provenance.
        1. `example_1.R` should be executed with `prov.run(...)` to illustrate "one-stop" provenance collection.
        2. `example_1-interactive.R` should be executed line-at-a-time to illustrate different aspects of provenance collection *via* console
    
### BioBreak

### 16:00 Hands-on (II)

7. Recap of first example and further discussion of the potential value and utility of provenance
8. Hands-on work (2)
    1. `/src/example_2a.R` introduces how to use provenance to assist with debugging
    2. `/src/example_2b.R` illustrates how provenance captures data types to assist with clean (*vs.* lazy) coding
9a. Participant examples?
9b. Alternate/if time: `/src/example_4.R` is a brief example of generating very clean code with `clean()` from *provClean*
10. Recap/discussion of the value of provenance

### BioBreak

### 17:00 Hands-on (III)

11. More complex examples and the challenges of keeping up with open-source software
     1. `src/example_3.R` illustrates a relatively simple simulation of population growth and demographic transitions. Example taken from Gotelli & Ellison ([2002](https://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/ellison-pubs/2002/gotelli_and_ellison_2002b.pdf), [2006](https://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/ellison-pubs/2006/gotelli_ellison_2006_ecolapp.pdf))
     2. `src/example_3a.R` illustrates importing remote data, manipulating the data, plotting it, and a simple ANOVA. Example taken from Fig. 1 of Wakefield *et al.* ([2005](https://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/ellison-pubs/2005/wakefield_etal_2005.pdf)) and data on the Harvard Forest Data Archive, dataset [HF328](https://harvardforest.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF328)
     3. `src/example_3b.R` is a small script that imports species-occurrence data using R library `spocc` from multiple data repositories and maps it. It currently works only with a development version of `rdtLite`. Hope to update on this at the workshop.
         1. `src/example_3b-0.R` uses `occ` in the `spocc` package to query GBIF for distribution data on the ant *Polyergus lucidus*, converts it to a dataframe and maps it using `ggplot`. Provenance is collected accurately for this version of the script.
         2. `src/example_3b-1.R` uses `occ` in the `spocc` package to query GBIF for distribution data on the ant *Polyergus lucidus*, but it then cleans the file names using `fixnames` in the `spocc` package, and maps the data using `map_plot` from the `mapr` package. Although the code in `example_3b-1.R` is more efficient, provenance is not collected accurately for this version of the script.
12. Participant examples
13. Closing discussion
     
### 18:00 End

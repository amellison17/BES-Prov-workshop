# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2018.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

###############################################################################

#' Provenance summarization functions
#' 
#' prov.summarize uses the provenance from the last execution of prov.run and outputs
#' a text summary to the R console
#' 
#' These functions use provenance collected using the rdtLite or rdt packages.
#' 
#' For provenance collected from executing a script file, the summary identifies:
#' \itemize{
#'   \item The name of the script file executed
#'   \item Environmental information identifying when the script was executed, the version of R,
#'      the computing system, the tool and version used to collect the provenance, the 
#'      location of the provenance file, and the hash algorithm used to hash data files.
#'   \item A list of libraries loaded and their versions
#'   \item The names of any scripts sourced
#'   \item The names of files input or output, the file timestamp, and its hashvalue
#'   \item Any URLs loaded and the time loaded
#'   \item Any errors or warnings along with the line on which they occurred, if known.
#'   \item Any messages sent to standard output along with the line on which they occurred, if known.
#' }
#' 
#' For provenance collected from a console session, only the environment and library information
#' appears in the summary.
#' 
#' Creating a zip file depends on a zip executable being on the search path.
#' By default, it looks for a program named zip.  To use a program with 
#' a different name, set the value of the R_ZIPCMD environment variable.  This
#' code has been tested with Unix zip and with 7-zip on Windows.  
#' 
#' @param prov.file the path to the file containing provenance
#' @param save if true saves the summary to the file prov-summary.txt in the 
#' provenance directory
#' @param create.zip if true all of the provenance data will be packaged up
#'   into a zip file stored in the current working directory.
#' 
#' @export
#' @examples 
#' \dontrun{prov.summarize ()}
#' @rdname summarize
prov.summarize <- function (save=FALSE, create.zip=FALSE) {
  # Determine which provenance collector to use
  tool <- get.tool()
  if (tool == "rdtLite") {
    prov.json <- rdtLite::prov.json
  } else {
    prov.json <- rdt::prov.json
  }
  
  prov <- provParseR::prov.parse(prov.json(), isFile = FALSE)
  summarize.prov (prov, save, create.zip)
}

#' prov.summarize.file
#' 
#' prov.summarize.file reads a JSON file that contains provenance and outputs
#' a text summary to the R console
#' 
#' @export
#' @examples 
#' testdata <- system.file("testdata", "prov.json", package = "provSummarizeR")
#' prov.summarize.file (testdata)
#' @rdname summarize
prov.summarize.file <- function (prov.file, save=FALSE, create.zip=FALSE) {
  if (!file.exists(prov.file)) {
    cat("Provenance file not found.\n")
    return()
  } 
  
  prov <- provParseR::prov.parse(prov.file)
  summarize.prov (prov, save, create.zip)
}

#' prov.summarize.run
#'
#' prov.summarize.run executes a script, collects provenance, and outputs a
#' text summary to the console.
#'
#' @param r.script the name of a file containing an R script
#' @param ... extra parameters are passed to the provenance collector.  See rdt's prov.run function
#'    or rdtLites's prov.run function for details.

#' @export 
#' @examples 
#' \dontrun{
#' testdata <- system.file("testscripts", "console.R", package = "provSummarizeR")
#' prov.summarize.run (testdata)}
#' @rdname summarize
prov.summarize.run <- function(r.script, save=FALSE, create.zip=FALSE, ...) {
  # Determine which provenance collector to use
  tool <- get.tool()
  if (tool == "rdtLite") {
    prov.run <- rdtLite::prov.run
    prov.json <- rdtLite::prov.json
  } else {
    prov.run <- rdt::prov.run
    prov.json <- rdt:: prov.json
  }
  
  # Run the script, collecting provenance, if a script was provided.
  tryCatch (prov.run(r.script, ...), error = function(x) {print (x)})

  # Create the provenance summary
  prov <- provParseR::prov.parse(prov.json(), isFile=FALSE)
  summarize.prov (prov, save, create.zip)
}

#' summarize.prov summarizes provenance where the provenance is in a string
#' 
#' @param prov the json in a string
#' @param save if true saves the summary to the file prov-summary.txt in the 
#' provenance directory
#' @param create.zip if true all of the provenance data will be packaged up
#'   into a zip file stored in the current working directory.
#' @noRd
summarize.prov <- function (prov, save, create.zip) {
  environment <- provParseR::get.environment(prov)
  
  if (save) {
    save.to.text.file(prov, environment)
  }
  else {
    generate.summaries(prov, environment)
  }
  
  if (create.zip) {
    save.to.zip.file (environment)
  }
  
}

#' get.tool determines whether to use rdt or rdtLite to get the provenance
#' 
#' If rdtLite is loaded, "rdtLite" is returned.  If rdtLite is not loaded, but rdt
#' is, "rdt" is returned.  If neither is loaded, it then checks to see if either
#' is installed, favoring "rdtLite" over "rdt".
#' 
#' Stops if neither rdt or rdtLite is available.
#' 
#' @return "rdtLite" or "rdt"
#' @noRd 
get.tool <- function () {
  # Determine which provenance collector to use
  loaded <- loadedNamespaces()
  if ("rdtLite" %in% loaded) {
    return("rdtLite")
  } 
  if ("rdt" %in% loaded) {
    return("rdt")
  } 

  installed <- utils::installed.packages ()
  if ("rdtLite" %in% installed) {
    return("rdtLite")
  } 
  if ("rdt" %in% installed) {
    return("rdt")
  }
   
  stop ("One of rdtLite or rdt must be installed.")
}

#' save.to.text.file saves the summary to a text file
#' @param prov the parsed provenance
#' @param environment a data frame containing the environment information
#' @noRd 
save.to.text.file <- function(prov, environment) {
  prov.path <- environment[environment$label == "provDirectory", ]$value
  prov.file <- paste(prov.path, "/prov-summary.txt", sep="")
  sink(prov.file, split=TRUE)
  generate.summaries(prov, environment)
  sink()
  cat(paste("Saving provenance summmary in", prov.file))
}

#' generate.summaries creates the text summary, writing it to the
#' current output sink(s)
#' @param prov the parsed provenance
#' @param environment the environemnt data frame extracted from the provenance
#' @noRd
generate.summaries <- function(prov, environment) {
  script.path <- environment[environment$label == "script", ]$value
  script.file <- sub(".*/", "", script.path)
  
  generate.environment.summary (environment, provParseR::get.tool.info(prov), script.file)
  generate.library.summary (provParseR::get.libs(prov))

  if (script.file != "") generate.script.summary (provParseR::get.scripts(prov))

  generate.file.summary ("INPUTS:", provParseR::get.input.files(prov), prov)
  generate.file.summary ("OUTPUTS:", provParseR::get.output.files(prov), prov)

  if (script.file != "") {
    generate.stdout.summary (prov)
    generate.error.summary (prov)
  }
}

#' generate.environment.summary creates the text summary of the environment, writing it to the
#' current output sink(s)
#' @param environment the environemnt data frame extracted from the provenance
#' @param tool.info the data frame containing information about the provenance collection tool that was used
#' @param script.file the name of the script executed.  For provenance collected from 
#'    a console session, the value is an empty string ("")
#' @noRd
generate.environment.summary <- function (environment, tool.info, script.file) {
  if (script.file != "") {
    cat (paste ("PROVENANCE SUMMARY for", script.file, "\n\n"))
  } else {
    cat (paste ("PROVENANCE SUMMARY for Console Session\n\n"))
  }
  
  cat (paste ("ENVIRONMENT:\n"))
  cat (paste ("Executed at", environment[environment$label == "provTimestamp", ]$value, "\n"))
  cat (paste ("Total execution time is", environment[environment$label == "totalElapsedTime", ]$value, "seconds\n"))
  
  if (script.file != "") {
    cat (paste ("Script last modified at", environment[environment$label == "scriptTimeStamp", ]$value, "\n"))
  }
  
  cat (paste ("Executed with", environment[environment$label == "langVersion", ]$value, "\n"))
  cat (paste ("Executed on", environment[environment$label == "architecture", ]$value,
      "running", environment[environment$label == "operatingSystem", ]$value, "\n"))
  cat (paste ("Provenance was collected with", tool.info$tool.name, tool.info$tool.version, "\n"))
  cat (paste ("Provenance is stored in", environment[environment$label == "provDirectory", ]$value, "\n"))
  cat (paste ("Hash algorithm is", environment[environment$label == "hashAlgorithm", ]$value, "\n" ))
  cat ("\n")
}

#' generate.library.summary creates the text summary of the libraries used, writing it to the
#' current output sink(s)
#' @param libs the data frame containing information about the libraries used
#' @noRd
generate.library.summary <- function (libs) {
  cat ("LIBRARIES:\n")
  cat (paste (libs$name, libs$version, collapse="\n"))
  cat ("\n\n")
}

#' generate.script.summary creates the text summary of the scripts sourced, writing it to the
#' current output sink(s)
#' @param scripts the data frame containing information about the scripts sourced
#' @noRd
generate.script.summary <- function (scripts) {
  cat (paste ("SOURCED SCRIPTS:\n"))
  if (nrow(scripts) > 1) {
    script.info <- dplyr::select(scripts[2:nrow(scripts), ], "script", "timestamp")
    for (i in 1:nrow(script.info)) {
      cat(script.info[i, "script"], "\n")
      cat("  ", script.info[i, "timestamp"], "\n")
    }
  } else {
    cat("None\n")
  }
  cat ("\n")
}

#' generate.file.summary creates the text summary of files read or written by the script, writing it to the
#' current output sink(s)
#' @param direction the heading to proceed the file list, intended to identify them as input or output files
#' @param files the data frame containing information about the files read or written by the script
#' @param prov the provenance object
#' @noRd
generate.file.summary <- function (direction, files, prov) {
  cat(direction, "\n")
  if (nrow(files) == 0) {
    cat ("None\n")
  }
  else {
    file.info <- dplyr::select(files, "type", "name", "value", "location", "hash", "timestamp")
    
    # Figure out which tool and version we are using.
    tool.info <- provParseR::get.tool.info(prov)
    tool <- tool.info$tool.name
    version <- tool.info$tool.version
    if (tool == "rdtLite" && utils::compareVersion (version, "1.0.3") < 0) {
      use.original.timestamp <- TRUE
    }
    else if (tool == "rdt" && utils::compareVersion (version, "3.0.3") < 0) {
      use.original.timestamp <- TRUE
    }
    else {
      use.original.timestamp <- FALSE
    }
    
    # In rdtLite before 1.0.3, and in rdt before 3.0.3, file times were
    # not preserved when copying into the data directory.  Therefore, we needed
    # to get the timestamp from the original file.  In later versions of the
    # tools, the timestamps are preserved, so we use the timestamp in the
    # saved copies.
    if (use.original.timestamp) {
      file.info$filetime <- as.character (file.mtime(file.info$location))
    }
    else {
      environment <- provParseR::get.environment(prov)
      prov.dir <- environment[environment$label == "provDirectory", ]$value
      file.info$filetime <- as.character (file.mtime(paste0 (prov.dir, "/", file.info$value)))
    }
    
    for (i in 1:nrow(file.info)) {
      cat(file.info[i, "type"], ": ")
      cat(file.info[i, "name"], "\n")
      if (is.na (file.info[i, "filetime"])) {
        if (file.info[i, "timestamp"] != "") {
          cat("  ", file.info[i, "timestamp"], "\n")
        }
      }
      else {
        cat("  ", file.info[i, "filetime"], "\n")
      }
        
      if (file.info[i, "hash"] != "") cat("  ", file.info[i, "hash"], "\n")
    }
  }
  cat("\n")
}

#' generate.stdout.summary creates the text summary for messages sent to
#' standard output.  It identifies
#' the line of code that produced the message as well as the message.
#' If the output is long, it identifies the snapshot file instead.
#' @param prov the provenance object
#' @noRd
generate.stdout.summary <- function (prov) {
  # Get the standard output nodes
  stdout.nodes <- provParseR::get.stdout.nodes(prov)
  
  generate.message.summary(prov, stdout.nodes, "CONSOLE")
}

#' generate.error.summary creates the text summary for errors and warnings.  It identifies
#' the line of code that produced the error as well as the error message
#' @param prov the provenance object
#' @noRd
generate.error.summary <- function (prov) {
  # Get the error nodes
  error.nodes <- provParseR::get.error.nodes(prov)
  generate.message.summary(prov, error.nodes, "ERRORS")
}

#' generate.message.summary creates the text summary for messages sent to
#' standard output.  It identifies
#' the line of code that produced the message as well as the message.
#' If the output is long, it identifies the snapshot file instead.
#' @param prov the provenance object
#' @noRd
generate.message.summary <- function (prov, output.nodes, msg) {
  cat (msg, ":\n", sep="")
  if (nrow(output.nodes) == 0) {
    cat ("None\n\n")
    return()
  }
  
  # Get the proc-data edges and the proc nodes
  proc.data.edges <- provParseR::get.proc.data(prov)
  proc.nodes <- provParseR::get.proc.nodes(prov)
  
  # Merge the data frames so that we have the output and the operation that
  # produced that output in 1 row
  output.report <- merge (output.nodes, proc.data.edges, by.x="id", by.y="entity")
  output.report <- merge (output.report, proc.nodes, by.x="activity", by.y="id")
  
  
  # Get the scripts and remove the directory name
  scripts <- provParseR::get.scripts(prov)
  scripts <- sub (".*/", "", scripts$script)
  
  # Output the error information, using line numbers if it is available
  for (i in 1:nrow(output.nodes)) {
    script.name <- scripts[output.report[i, "scriptNum"]]
    if (!is.na (script.name)) {
      cat ("In", scripts[output.report[i, "scriptNum"]])
      if (is.na (output.report[i, "startLine"])) {
        cat (" on line:\n")
        cat ("  ", output.report[i, "name"], "\n")
      }
      else if (output.report[i, "startLine"] == output.report[i, "endLine"] || 
          is.na (output.report[i, "endLine"])){
        cat (" on line ", output.report[i, "startLine"], ":\n")
        # Omit source code if the line number is known
        #cat ("  ", output.report[i, "name"], "\n")
      }
      else {
        cat (" on lines ", output.report[i, "startLine"], " to ", output.report[i, "endLine"], ":\n")
        # Omit source code if the line number is known
        #cat ("  ", output.report[i, "name"], "\n")
      }
    }
    cat ("  ", output.report[i, "value"], "\n")
  }
  cat("\n")
}

#' save.to.zip.file creates a zip file of the provenance directory
#' @param environment the environemnt data frame extracted from the provenance
#' @noRd
save.to.zip.file <- function (environment) {
  # Determine where the provenance is 
  cur.dir <- getwd()
  prov.path <- environment[environment$label == "provDirectory", ]$value
  setwd(prov.path)
  
  # Determine the name for the zip file
  prov.dir <- sub (".*/", "", prov.path)
  zipfile <- paste0 (prov.dir, "_", 
      environment[environment$label == "provTimestamp", ]$value, ".zip")
  zippath <- paste0 (cur.dir, "/", zipfile)

  if (file.exists (zippath)) {
    warning (zippath, " already exists.")
  }
  
  else {
    # Zip it up
    zip.program <- Sys.getenv("R_ZIPCMD", "zip")
    if (.Platform$OS.type == "windows" && endsWith (zip.program, "7z.exe")) {
      # 7z.exe a prov.zip . -r -x!debug 
      zip.result <- utils::zip (zippath, ".", flags="a", extras="-r -x!debug")
    }
    else {
      # zip -r prov.zip . -x debug/ 
      zip.result <- utils::zip (zippath, ".", flags="-r", extras="-x debug/")
    }
    
    # Check for errors
    if (zip.result == 0) {
      cat (paste ("Provenance saved in", zipfile))
    }
    else if (zip.result == 127) {
      warning ("Unable to create a zip file.  Please check that you have a zip program, such as 7-zip, on your path, and have the R_ZIPCMD environment variable set.")
    }
    else if (zip.result == 124) {
      warning ("Unable to create a zip file.  The zip program timed out.")
    }
    else {
      warning ("Unable to create a zip file.  The zip program ", zip.program, " returned error ", zip.result)
    }
  }
  
  # Return to the directory where the user executed the command from.
  setwd(cur.dir)
}

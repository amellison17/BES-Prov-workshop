################################################################################
# Script 3 of 3 used to model the impacts of climate change 
# on carnivorous plant habitat as described in Fitzpatrick & Ellison (2017) 
#
# Written by M Lisk and MC Fitzpatrick at UMCES-Appalachian Lab, Frostburg, MD 
#
# Code is provided as is. We apologize in advance that we 
# are unable to provide support.
################################################################################

options(stringsAsFactors = FALSE)
##libraries
library(dismo)
library(raster)
library(SDMTools)
library(rasterVis)
library(colorRamps)
library(rworldmap)
library(plyr)
library(ggplot2)
library(reshape2)
library(reshape)
library(foreach)
library(doParallel)
library(pbapply)
library(maptools)
library(shape)

driveDir <- "/.../"

years <- c("2050", "2080")             ##years to project
#rcp <- c("rcp4_5", "rcp8_5")   ##RCPs to project
rcp <- c("rcp8_5")

##location of the species files
inTaxaFiles <- paste(driveDir, "/.../", sep="")
##folder containing the rasters of current predicted distribution
predDir <- paste(driveDir, "/.../", sep="")
##lists the species object files
maxentMods <- list.files(path=inTaxaFiles, pattern=".RData", 
                         recursive=F, full.names=T)

##list spp occurrence files to load, will need to update directory once all points exported
sppOccFiles <- list.files(paste(driveDir, "/...", sep=""), pattern=".csv", 
                          recursive=F, full.names=T)

##where to write the velocities out to
veloOut <- paste(driveDir, "/.../", sep="")

##list the completed species and remove them from further projections
listProjSpp <- list.files(paste(predDir, "rcp8_5/", sep=""), 
                          full.names=F, recursive=T)
chopYear <- sapply(strsplit(listProjSpp, "/"), "[[", 2)
splitGenus <- sapply(strsplit(chopYear, "_"), "[[", 1)
splitSpp <- sapply(strsplit(chopYear, "_"), "[[", 2)
fullName <- unique(paste(splitGenus, "_", splitSpp, sep=""))
fullNameProj <- table(paste(splitGenus, "_", splitSpp, sep=""))
##which ones don't have full projections done
compNamesProj <- names(which(fullNameProj==63))

listVelSpp <- list.files(paste(veloOut, "rcp8_5/", sep=""), full.names=F, recursive=T)
chopYear <- sapply(strsplit(listVelSpp, "/"), "[[", 2)
splitSpp <- sapply(strsplit(chopYear, "_velo"), "[[", 1)
fullName <- unique(paste(splitGenus, "_", splitSpp, sep=""))
fullNameVel <- table(splitSpp)
##which ones don't have full projections done
compNamesVel <- names(which(fullNameVel==2))

projCompIndex <- unlist(sapply(compNamesProj, grep, x=maxentMods))
maxentMods <- maxentMods[projCompIndex]

##########################################################################
##########################################################################
calcVelocity <- function(pr1, pr2, yrs){
  #################
  #pr1 <- holdGeo  ##current prediction
  #pr2 <- futPreds[[23]]   ##future/ other time projection
  #################
  
  ##calculate velocity
  spatial <- slope(pr1, latlon=TRUE)
  ##turn 0s into decimals to remove dividing by 0
  #spatial[spatial==0] <- 0.000000000000000000001
  spatial[spatial==0] <- NA
  temporal <- (pr2-pr1)/yrs
  velocity <- temporal/spatial
  
  ##modify values to remove positive and negative Infs
  velocity[velocity > quantile(velocity, prob=0.99)] <- quantile(velocity,prob=0.99)
  velocity[velocity < quantile(velocity, prob=0.01)] <- quantile(velocity,prob=0.01)
  
  return(velocity)
}
##########################################################################
cycleVelocities <- function(maxOb, projLoc, outDir, yrs, rcp){
  #################
  #maxOb <- maxentMods[1]  ##model object character name
  #projLoc <- projDir      ##directory from which to read in the projections
  #yrs <- years            ##character year
  #rcp <- rcp              ##character rcp
  #################
  
  ##get species name
  splitName <- sapply(strsplit(maxOb, "/"), "[[", length(strsplit(maxOb, "/")[[1]]))
  sppName <- sapply(strsplit(splitName, ".R"), "[[", 1)
  
  ##loads the species object, object called outOb
  load(maxOb)
  
  ##extracts the current projections from the maxent object
  currPreds <- lapply(outOb$mapPreds, raster)
  ####################################
  ##rename raster for local creation
  obRastDir <- outOb$mapPreds[[1]]
  renameRastDir <- gsub("/.../", "/.../", obRastDir)
  holdGeo <- raster(renameRastDir)
  ####################################
  ##lists off the the created projections for the species 
  listSppPreds <- list.files(projLoc, paste(sppName, "_", sep=""), 
                             recursive=T, full.names=T)
  
  ####just around the input points#############
  ##load species file
  #############################################
  
  
  ##cycles through projections
  for(y in yrs){
    yr <- as.numeric(sapply(strsplit(y, "20"), "[[", length(strsplit(y, "20")[[1]])))
    
    for(r in rcp){
     
      ##greps the projection for the year/rcp combination
      grepRCP <- listSppPreds[grep(paste("/", r, "/", y, "/", sep=""), listSppPreds)]
      
      ##stacks future predictions
      futPreds <- lapply(grepRCP, raster)
      ##remove NA predictions
      goodPreds <- lapply(futPreds, function(r){if(is.finite(r@data@max)==T & is.finite(r@data@min)==T){return(r)}})
      if(length(which(sapply(goodPreds,is.null)==T))>0){
        futPreds <- goodPreds[-(which(sapply(goodPreds,is.null)==T))]
      }
      
      ##create the velocities
      futVelocities <- lapply(futPreds, calcVelocity, pr1=holdGeo, yrs=yr)
      
      ##average volocities, or write them out to disk?
      aveAll <- sum(stack(futVelocities))/length(futVelocities)
      
      ##make sure output directory exists
      dir.create(paste(outDir, r, "/", y, "/", sep=""), showWarnings=F, recursive=T)
      
      ##write the rasters out to save
      writeRaster(aveAll, 
                  paste(outDir, r, "/", y, "/", sppName, "_velocityAverage.tif", sep=""), 
                  type="GTiff", overwrite=T)
    }
  }
}
##########################################################################
##########################################################################


cl <- makeCluster(50)
registerDoParallel(cl)

checkVelos <- foreach(k=1:length(maxentMods), .errorhandling="pass", .verbose=T, .packages=c("raster", "dismo", "SDMTools")) %dopar%
  cycleVelocities(maxentMods[k], projLoc=projDir, yrs=years, rcp=rcp, outDir=veloOut)

stopCluster(cl)

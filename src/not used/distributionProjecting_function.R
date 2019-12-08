################################################################################
# Script 2 of 3 used to model the impacts of climate change 
# on carnivorous plant habitat as described in Fitzpatrick & Ellison (2017) 
#
# Written by M Lisk and MC Fitzpatrick at UMCES-Appalachian Lab, Frostburg, MD 
#
# Code is provided as is. We apologize in advance that we 
# are unable to provide support.
################################################################################

##libraries
library(dismo)
library(rJava)
library(rJava, lib.loc="/.../")
library(rgeos)
library(grid)
library(rgdal)
library(sm)
library(foreach)
library(doParallel)
library(raster)
library(pbapply)

driveDir <- "/.../"

##load testing model objects
inTaxaFiles <- paste(driveDir, "/.../", sep="")
##loaded object name currently: foreachRun
years <- c("2050", "2080")             ##years to project
#rcp <- c("rcp4_5", "rcp8_5")           ##RCPs to project
rcp <- c("rcp8_5")

inClimateDir <- paste(driveDir, "/.../", sep="")

# folder containing future climate rasters
# download from: http://www.ccafs-climate.org/data_spatial_downscaling/
climPathDirs <- paste(inClimateDir, years, "/IPCC_AR5/", sep="")
scenarios <- list.files(climPathDirs[[1]])
##currently writing out to local location
outWriteDir <- paste(driveDir, "/.../", sep="")

maxentMods <- list.files(path=inTaxaFiles, pattern=".RData", recursive=F, full.names=T)

##geoFile
geoFile <- read.csv(paste(driveDir, "/.../sppGeoDivide.csv", sep=""))

##########################################################################
##########################################################################
##cycles through each maxent model
makeProjection <- function(maxMod, maxWieght, clims, projGeo){
  #################
  #maxMod <- maxMods[[1]]
  #maxWieght <- maxWeights[1]
  #clims <- clFiles
  #projGeo <- holdGeo
  #################
  
  ##gets correct varList by sppName
  sppVars <- colnames(maxMod@presence)
  holdVars <- sppVars
  
  sppVarFiles <- clims[sapply(holdVars, grep, x=clims)]
  
  ##stacks climate files
  stackFutClim <- stack(sppVarFiles)
  
  ##crop climate files
  cropFutClim <- crop(stackFutClim, projGeo)
  climAllFut <- mask(cropFutClim, projGeo)
  
  ##rename the raster files so that the names match the models
  names(climAllFut) <- sppVars
  
  mod_bias_Fut <- predict(maxMod, climAllFut)
  
  ##return projection raster
  return(mod_bias_Fut)
}
##########################################################################
modProj <- function(m, scen, outWrite, r, yr, clFiles, gFile){
  #################
  #m <- mods[1]
  #scen <- scens[1]
  #outWrite <- outDir
  #r <- r
  #yr <- year
  #clFiles <- climFiles
  #gFile <- geoDivFile
  #################
  
  ##get species name
  splitName <- sapply(strsplit(m, "/"), "[[", length(strsplit(m, "/")[[1]]))
  sppName <- sapply(strsplit(splitName, ".R"), "[[", 1)
  
  ##get geo subset from species name
  whichGeo <- gFile[which(gFile$Species==sppName),"clipExtent"]
  ##if needed, change where to find climate files
  if(whichGeo!="fullWorld"){
    clFiles <- gsub("fullWorld", whichGeo, clFiles)
    clFiles <- gsub(".asc", ".tif", clFiles)
  }
  
  ##checks to see if output files already exists
  if(file.exists(paste(outWrite, r, "/", yr, "/", sppName, "_", scen, "_maxent_biasCorrected.tif", sep=""))==FALSE){
    ##load maxent model object, called outOb
    load(m)
  
    ##isolates maxent model part of the object
    maxMods <- unlist(outOb$modelObjs)
    #maxMods <- unlist(maxMods)
    ####################################
    ##rename raster for local creation
    obRastDir <- outOb$mapPreds[[1]]
    renameRastDir <- gsub("/.../", "/.../", obRastDir)
    holdGeo <- raster(renameRastDir)
    ####################################
    maxWeights <- unlist(outOb$modelWeights)
    
    ##create scenerio projection rasters
    spMaxStack_bias_Fut <- stack(mapply(makeProjection, maxMod=maxMods, maxWieght=maxWieghts, MoreArgs=list(clims=clFiles, projGeo=holdGeo), SIMPLIFY=F))
    #spMaxStack_bias_Fut <- stack(spMaxStack_bias_Fut)
  
    ##writes out the projections
    writeRaster(sum(spMaxStack_bias_Fut)/dim(spMaxStack_bias_Fut)[3], 
              paste(outWrite, r, "/", yr, "/", sppName, "_", scen, "_maxent_biasCorrected.tif", sep=""), 
              type="GTiff", overwrite=T)
  }
}
##########################################################################
runProjections <- function(scens, yrs, rcp, outDir, mods, cores, geoDivFile){
  #################
  #scens <- scenarios
  #yrs <- climPathDirs
  #rcp <- rcp
  #outDir <- outWriteDir
  #mods <- maxentHold
  #cores <- 7
  #geoDivFile <- geoFile
  #################
  for(y in yrs){
    #y <- yrs[1]
    
    year <- sapply(strsplit(y, "/"), "[[", length(strsplit(y, "/")[[1]])-1)
    
    for(r in rcp){
      #r <- rcp[1]
      
      ##make sure output directory exists
      dir.create(paste(outDir, r, "/", year, "/", sep=""), showWarnings=F, recursive=T)
      
      for(sc in scens){
        #sc <- scens[1]
        
        ##checks to make sure rcp is available for scenerio
        if(r %in% dir(paste(y, sc, "/", sep=""))==TRUE){
          path <- paste(y, sc, "/", r, sep="")
          climFiles <- list.files(path, pattern="bio", full.names=T, recursive=F)
          
          cl <- makeCluster(cores)
          registerDoParallel(cl)
          
          foreach(k=1:length(mods), .verbose=T, .export=c("modProj", "makeProjection"), .errorhandling="remove", .packages=c("raster", "sm", "dismo", "rgeos", "rgdal", "grid")) %dopar%
            modProj(mods[k], scen=sc, outWrite=outDir, r=r, yr=year, clFiles=climFiles, gFile=geoDivFile)
          
          stopCluster(cl)
        }
      }
    } 
  }
}
##########################################################################
##########################################################################


##run worldclim projections
lapRun <- pblapply(scenarios, function(scen){try(runProjections(scens=scen, 
                  yrs=climPathDirs, rcp=rcp, outDir=outWriteDir, mods=maxentHold, 
                  cores=50, geoDivFile=geoFile))})

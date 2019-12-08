################################################################################
# Script 1 of 3 used to model the impacts of climate change 
# on carnivorous plant habitat as described in Fitzpatrick & Ellison (2017) 
#
# Written by M Lisk and MC Fitzpatrick at UMCES-Appalachian Lab, Frostburg, MD 
#
# Code is provided as is. We apologize in advance that we 
# are unable to provide support.
################################################################################

##load libraries, functions, data, etc ------------------------------------
library(dismo)
library(ecospat)
library(raster)
library(maptools)
library(rJava, lib.loc="/.../")
library(ENMeval)
library(spdep)
library(rworldmap)
library(foreach)
library(doParallel)
library(pbapply)

driveDir <- "/.../"

##ecoregion shapefile to estimate accessibility region
## download from: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
ecoReg <- readShapeSpatial(paste(driveDir, ".../wwf_terr_ecos.shp", sep=""))
ecoReg <- ecoReg[ecoReg$ECO_NAME!="Lake",]
ecoReg <- ecoReg[ecoReg$ECO_NAME!="Rock and Ice",]
disEcos <- unionSpatialPolygons(ecoReg, ecoReg$ECO_NAME)
findNeighbors <- poly2nb(disEcos)

##list spp occurrence files to load, will need to update directory once all points exported
sppOccFiles <- list.files(paste(driveDir, "...", sep=""), pattern=".csv", 
                          recursive=F, full.names=T)

#...and path to folder containing the current climate data
# download link (version 1.4): http://www.worldclim.org/current 
climDir <- paste(driveDir, "...", sep="")

## folder where to write species prediction rasters 
predDir <- paste(driveDir, "/.../", sep="")
##if needed, creates directory to store current predictions
dir.create(predDir, showWarnings=F)

##where to write out the maxent data objects
obOutDir <- paste(driveDir, "/.../", sep="")
dir.create(obOutDir, showWarnings=F)

##########################################################################
##########################################################################
cleanTemp <- function(){
  ##remove temp files no longer needed
  tD <- tempdir()
  #tD <- gsub("\\", "/", tD)
  getDir <- sapply(strsplit(tD, "Rtmp"), "[[", length(strsplit(tD, "Rtmp")[[1]]))
  getPath <- sapply(strsplit(tD, "Rtmp"), "[[", 1)
  
  ##list temp directories in the dir
  listTemps <- list.dirs(getPath, full.names=T, recursive=F)
  rTemps <- listTemps[grep("Rtmp", listTemps)]
  ##remove current temp
  rTemps <- rTemps[-grep(paste("Rtmp",getDir,sep=""),rTemps)]
  sapply(rTemps, unlink, recursive=T, force=T)
}
##########################################################################
maxentTrainAndModel <- function(sppOccFile, whichPts, ecoRegions, regionNeighbors, climDir, climVars, 
                                nPerms, kVal, bet, predWriteLoc, ojbWriteLoc, minPts){  #, cores
  #################
  #sppOccFile <- sppOccFiles[224]
  #whichPts <- "inside"         ##determines which points to use in reguards to range: "inside"=inside range only, "unknown"=inside and unknown points used, "all"=all yes points used
  #ecoRegions <- disEcos          ##the ecoregion shapes to use
  #regionNeighbors <- findNeighbors  ##a list of ecoregion neighbors, added cause takes so long to compute
  #climDir <- climDir
  #climVars <- c("bio_10", "bio_11", "bio_4", "bio_18", "bio_19")   ##vector of the climate variables to use
  #climVars <- c("bio_10", "bio_11", "bio_18", "bio_19")
  #nPerms <- 10                 ##number of test/train permutations
  #kVal <- 4                    ##the k value to use
  #bet <- seq(0, 15, by=0.25)    ##beta values to sequence along
  #predWriteLoc <- predDir
  #ojbWriteLoc <- obOutDir
  #minPts <- 15
  #cores <- 7
  #################
  ##build the data tables for model fitting ------------------------------------
  ##grab desired points to model
  readSpp <- read.csv(sppOccFile)
  ##get Spp name
  getSpp <- sapply(strsplit(sppOccFile, "/"), "[[", length(strsplit(sppOccFile, "/")[[1]]))
  getSpp <- sapply(strsplit(getSpp, ".csv"), "[[", 1)
  print(getSpp)
  #message(getSpp)
  
  ##if output model object already exists, the assume model has been completed and skip
  if(!file.exists(paste(ojbWriteLoc, "/", getSpp, ".RData", sep=""))){
    validOcc <- readSpp[readSpp$usePoint=="yes",]
    ##subsets the records based on range value
    if(whichPts=="inside"){
      sppOcc <- validOcc[validOcc$range=="inside",]
    }else if(whichPts=="unknown"){
      sppOcc <- validOcc[validOcc$range!="outside",]
    }else if(whichPts=="all"){
      sppOcc <- validOcc
    }else{
      stop("whichPts argument needs a valid character input")
    }
    
    ##end if no rows available
    if(nrow(sppOcc)<1){
      stop("no rows to get coordinates from")
    }
    
    ##isolate coordinates
    sppOcc <- sppOcc[,c("longitude", "latitude")]
    
    ##load current climate data
    climStack <- stack(sapply(climVars, function(v){list.files(climDir, pattern=paste(v,"$",sep=""), full.names=T, recursive=F)}))
    ##modifies extent to remove decimals from the ymax
    #extent(climStack) <- c(-180, 180, -60, 90)
    
    ##makes the points spatial, and makes sure all objects in same projection
    sppOcc <- SpatialPoints(sppOcc, proj4string=climStack@crs)
    ecoRegions@proj4string <- sppOcc@proj4string
    
    ##start by defining the ecoregions in which the species occurs
    ##as an estimate of accessibility (Barve et al. 2011)
    ##then find neighboring regions
    regIndex <- na.omit(unique(over(sppOcc, ecoRegions)))
    ##identify and extract regions neighboring those the spp occurs in
    getNeighbors <- unique(c(unlist(sapply(regIndex, function(i){regionNeighbors[[i]]})), regIndex))
    accRegions <- ecoRegions[getNeighbors]
    accContinent <- accRegions
    
    climCrop <- crop(climStack, accContinent)
    climCrop <- mask(climCrop, accContinent)
    climAccRast <- climCrop
    climAcc <- extract(climAccRast, accRegions, cellnumbers=T)
    climAcc <- do.call(rbind, climAcc)
    climAcc <- data.frame(na.omit(climAcc))
    
    ##extract climate at the species occurrence localities
    climPts <- data.frame(na.omit(extract(climAccRast, sppOcc, cellnumbers=T)))
    ##if there are less than break value points, end the function
    if(nrow(na.omit(unique(climPts)))<minPts){
      stop("don't have minimum number of records left")
    }
    
    ##remove occurence points from background & get x-y coords of BG pts
    climAcc <- climAcc[!(climAcc$cell %in% climPts$cell),]
    backXY <- xyFromCell(climCrop[[1]], climAcc$cell)
    
    ##put together the env / occurrence data needed for model fitting
    ##create backgroun env. with  points
    if(nrow(climAcc) < 10000){
      samps <- 1:nrow(climAcc)
      env.bg <- climAcc[,-1]
    }else{
      samps <- sample(1:nrow(climAcc), 10000, replace=F)
      env.bg <- climAcc[samps,-1]
    }
    ##env. at occurrence records
    env.occ <- climPts[,-1]
    
    ##combine occurrence clim and background clim into one object
    env  <- as.data.frame(rbind(env.occ, env.bg))
    resp <- c(rep(1,nrow(env.occ)),rep(0,nrow(env.bg)))
    
    ##=================================================================================
    ##10 runs using a random splits of data for training and testing ESM
    ##=================================================================================
    eval.test.esm <-  NULL    # for evaluation results of ESM
    
    outMods <- list()
    outVect <- list()
    outMapPred <- list()
    for(i in 1:nPerms){
      #i <- 1
      print(paste("perm: ", as.character(i), sep=""))
      ##split data for training and testing (75:25)
      mykfold <- kfold(resp, k=kVal, by=resp)
      sel.train  <- mykfold != 1
      sel.test   <- mykfold == 1
      resp.train <- resp[sel.train]
      resp.test  <- resp[sel.test]
      
      xy <- coordinates(sppOcc)
      colnames(xy) <- c("x", "y")
      xy <- rbind(xy, backXY[samps,])
      
      ##build ESM as weighted average of bivariate Maxent models
      weight.vec <- vector()                 ##vector with weights of single bivariate models
      pred.biva.sum <- rep(0, length(resp))  ##for ensemble prediction of bivariate models
      pred.biva.map <- climCrop[[1]]>1000
      listOMods <- list()
      
      ##build all bivariate predictor combinations
      for(m in 1:(dim(env)[2]-1)){
        for(n in (m+1):dim(env)[2]){
          
          spMaxMods_beta <- lapply(bet, function(bs){maxent(x=env[sel.train,c(m,n)], p=resp.train,
                                                            args=c("-P", paste("betamultiplier", "=", bs,sep=""), "hinge=false", "threshold=false", "product=false", "outputgrids=false"))})
          
          spMaxStackRAW_beta <- lapply(spMaxMods_beta, function(spB){predict(spB,  climAccRast[[which(names(climAccRast) %in% names(spB@presence))]], args='outputformat=raw')})
          
          spMaxMods_aic <- mapply(function(spB,spStR){calc.aicc(nparam=get.params(spB),predictive.maps=spStR,occ=xy[sel.test,])}, spB=spMaxMods_beta, spStR=spMaxStackRAW_beta, SIMPLIFY=F)
          
          aicc <- do.call(rbind, spMaxMods_aic)
          
          if(length(unique(aicc[,1]))==1 & is.na(unique(aicc[,1]))==T){
            #print("thing")
            break
          }
          
          ##maxent model to give user
          me.biva <- spMaxMods_beta[[which.min(aicc[,1])]]
          
          ##prediction
          pred.biva <- predict(me.biva, env)
          
          ##evaluate bivariate model and calculate its weight for ensemble prediction
          eval.train.biva <- evaluate(p=pred.biva[sel.train][resp.train==1], a=pred.biva[sel.train][resp.train==0])
          
          ##use Somers' D as weighting factor
          weight <- eval.train.biva@auc*2-1
          if(weight < 0) {weight <- 0}
          weight.vec <- c(weight.vec, weight)
          
          ##build weighted sum of bivariate predictions
          pred.biva.sum <- pred.biva.sum + (pred.biva * weight)
          pred.biva.map <- pred.biva.map +
            (predict(spMaxMods_beta[[which.min(aicc[,1])]],
                     climCrop[[which(names(climCrop) %in%
                                       names(spMaxMods_beta[[which.min(aicc[,1])]]@presence))]]))*weight
          
          listOMods[[length(listOMods)+1]] <- me.biva
        }
      }
      
      ##if the models for the test-training split did not fit, move on to next perm,
      ##otherwise (models did fit), include data as part of output object
      if(sum(weight.vec)<=0){
        warning("weight.vec is less than or equal to zero, removing perm bivariate split from output")
      }else{
        outMods[[i]] <- listOMods
        outVect[[i]] <- weight.vec
        
        ##calculate ESM prediction
        pred.esm <- pred.biva.sum/sum(weight.vec)
        pred.biva.map.out <- pred.biva.map/sum(weight.vec)
        
        outMapPred[[i]] <- pred.biva.map.out
        
        ##evaluate ESM
        auc.test <- evaluate(p=pred.esm[sel.test ][resp.test ==1], a=pred.esm[sel.test ][resp.test ==0])@auc
        boyce.test <- ecospat.boyce(pred.esm[sel.test],pred.esm[sel.test][resp.test==1], PEplot=F)$Spearman.cor
        eval.test.esm <- rbind(eval.test.esm , cbind(auc=auc.test,boyce=boyce.test))
      }
    }
    
    ##removes nulls from output objects
    if(length(which(sapply(outMapPred,is.null)==T))>0){
      outMapPred <- outMapPred[-(which(sapply(outMapPred,is.null)==T))]
      outMods <- outMods[-(which(sapply(outMods,is.null)==T))]
      outVect <- outVect[-(which(sapply(outVect,is.null)==T))]
    }
    ##if all output was null from model not fitting, end processing
    if(length(outMapPred)<1){
      #cleanTemp()
      stop("no valid output, ending processing")
    }
    
    ##average raster stack output
    avePerms <- mean(stack(outMapPred))
    ##write out raster output
    predRastName <- paste(predWriteLoc, "/", getSpp, ".tif", sep="")
    writeRaster(avePerms, predRastName, overwrite=T)
    
    ##create output object structure
    outOb <- structure(list(sppName = getSpp,
                            mapPreds = predRastName,
                            evalStats = eval.test.esm,
                            modelObjs = outMods,
                            modelWeights = outVect))
    
    save(outOb, file=paste(ojbWriteLoc, "/", getSpp, ".RData", sep=""))
  }
}  
##########################################################################  
##########################################################################


##########################################################################  
##########################################################################
##foreach version
cl <- makeCluster(1)
registerDoParallel(cl)
testRun <- foreach(k=1:length(sppOccFiles), .verbose=T, .errorhandling="pass", .packages=c("rgdal", "dismo", "rworldmap", "ENMeval", "ecospat")) %dopar%
  maxentTrainAndModel(sppOccFile=sppOccFiles[k], whichPts="inside", ecoRegions=disEcos, regionNeighbors=findNeighbors,
                      climDir=climDir, climVars=c("bio_5", "bio_11", "bio_12", "bio_15"), nPerms=10, 
                      kVal=4, bet=seq(0, 15, by=0.25), predWriteLoc=predDir, ojbWriteLoc=obOutDir, minPts=10)
stopCluster(cl)
cleanTemp()
##########################################################################  
##########################################################################

# Comprehensive compilation of functions for DemoSim models
# 5 December 2017
# NJG


###################################################
# FUNCTION: PopIter
# iterate population series
# input:
# n0 = initial population vector
# At = list of transition matrices at time t
# output: nt = matrix of stages x time with n 
#------------------------------------------------- 
PopIter <- function(n0=NULL,At=NULL) {
if(is.null(n0) & is.null(At)){
  n0 <- c(1500,23500,1400,500)
  At <- replicate(n=3,matrix(data=runif(16),nrow=4),simplify=FALSE)
  #At <- array(data=runif(1600),dim=c(4,4,100))
  At[[1]] <- matrix(data=rep(0,16),nrow=4) # set null elements for first transition matrix
} 
# Create matrix for holding output of population sizes
nt <- matrix(data=rep(0,times=length(At)*dim(At[[1]])[1]),nrow=dim(At[[1]])[1])

# Fill the first entry with the initial vector
nt[,1] <- n0
# Iterate matrix multiplication
for (i in 2:ncol(nt)){
m <- At[[i]] 
nt[,i] <- At[[i]]%*%nt[,i-1]
nt[,i] <- trunc(nt[,i]) # truncate population sizes to integers
if(sum(nt[,i]) <=1) {
  nt[i] <- 0
  break
}
}
  return(nt)
}
#------------------------------------------------- 
##################################################
# FUNCTION: NitMat
# Nitrogen matrix construction
# input:  Nitro = value of nitrogen level
# output:  nM = transition matrix for that level of nitrogen
# Data pulled from Delphi program on line with G&E 2008
#------------------------------------------------- 
NitMat <- function(Nitro=runif(n=1,min=-4,max=1),m=DataSub) {
  
  # create empty matrix to hold transition elements
  zMat <- matrix(data=rep(x=0,times=nStage^2),nrow=nStage)
  
  # log nitrogen values are constant for all interpolations
  xNit <- c(-3,-2,-1,0,1,2)
  
  # set up double for loop to cycle through all of the elements
 for (i in 1:nStage) {
    for (j in 1:nStage) {
  # create the vector of 6 points transition values for the interpolation    
  yNit <- m$Element[as.numeric(m$RowLab)==i & as.numeric(m$ColLab)==j]
  
  # call the approx function to do the linear interpolation
  # and pass the interpolated element to the matrix slot
  zMat[i,j] <- approx(x=xNit,y=yNit,xout=Nitro,method="linear",rule=2)[[2]]
    }
 }
  
  # return a matrix with the transition matrix 
  return(zMat)
  
}


##################################################
# FUNCTION: NitGen
# Generates a time series of nitrogen values
# input: nstep, number of time steps
# output: Nvec, a vector of N values of length nstep 
# Parameter values taken from Gotelli and Ellison 2002

# For Quabbin:
# N(t + 1) = a + b*t + e (linear)
# a =  0.391, b =  -0.004 mg N/L, and sigma of e =  0.075
# N(t + 1) =  b*(N(t)) + e (autoregressive) 
# b = 0.953, sigma of e = 0.042 (set N(1) = 0.391)

# For Shelburne:
# N(t + 1) = a + b*t + e (linear)
# a =  0.477, b =  -0.001 mg N/L, and sigma of e =  0.065
# N(t + 1) =  b*(N(t)) + e (autoregressive) 
# b = 0.979, sigma of e = 0.158 (set N(1) = 0.477)
#------------------------------------------------- 
NitGen <- function(t=100, parms=list(a=0.477,b=1.00,e=0.158,site="Molly")) {
v <- vector(mode="numeric",length=t)  
a <- parms$a
b =  parms$b 
e =  parms$e
site = parms$site
time <- 1:t
noise <- rnorm(n=t,mean=0,sd=e)
#---------------------------------------
# linear calculation
# v[time] <- a + b*time + noise # 
#---------------------------------------
# autoregressive calculation
v[1] <- a
for (i in 2:t) {
  v[i] <- b*v[i - 1] + noise[i]
}
#---------------------------------------
v[v <=0] <- 0.01 # reset negatives to a small value
v <- log10(v) # convert to log(10) for nitrogen conversion

return(v)
}
##################################################
# FUNCTION: GetSummary
# Calculates the final population size and the length of the run
# input: list of age classes x time with numbers 
# output: list of population size and the length of the run
#------------------------------------------------- 
GetSummary <- function(Xvec=NULL) {
  if(is.null(Xvec)) {
    Xvec <- replicate(n=100,rpois(n=4,lambda=2),simplify=FALSE)
  }
zz <- apply(X=as.matrix(unlist(Xvec)),MARGIN=2,FUN=sum)
FinalSize <- zz[length(zz)]
if (min(zz) > 0) FirstZero <- length(zz) else
  FirstZero <-  min(which(zz==0))

zzTrim <- zz[zz>0]
lifeSpan <- length(zzTrim) # get lifespan
if(lifeSpan==1) zzTrim <- c(zzTrim,1)
  

  MyModel <- lm(log(zzTrim)~seq_along(zzTrim))
  lambda <- unlist(MyModel$coefficients[2])
  return(list(FinalSize,FirstZero,lifeSpan,lambda))
}
##################################################

##################################################
# FUNCTION: DirichletMaker
# creates sampled transition matrix based on sampling uncertainty
# input: Original Molly or Hawley matrix
# output: a single dirichlet sampled matrix of same structure 
#------------------------------------------------- 
DirichletMaker <- function(m) {
 msample <- matrix(data=rep(0,16),nrow=4)
 
# rough guestimate for p~ 0.10 on recruitment transition
msample[2,1] <- rdirichlet(n=1,alpha=c(2,18))[1,1]

# pull sampling from second column (juveniles, n= 200)
msample[c(2,3),2] <- rdirichlet(n=1,alpha=200*c(m[2,2],
                                                m[3,2],
                                               (1 - m[2,2] -
                                                  m[3,2])))[1,c(1,2)]

# pull sampling from third column (NF adults, n= ~175)
msample[c(2,3,4),3] <- rdirichlet(n=1,alpha=175*c(m[2,3],
                                                m[3,3],
                                                m[4,3],
                                               (1 - m[2,3] -
                                                  m[3,3] - 
                                                  m[4,3])))[1,c(1,2,3)]


# pull sampling from fourth column (F adults, n= ~25)
msample[c(3,4),4] <- rdirichlet(n=1,alpha=25*c(m[3,4],
                                                m[4,4],
                                               (1 - m[3,4] -
                                                  m[4,4])))[1,c(1,2)]
# try something ad-hoc for fecundity entry
# no sampling variability but assume lambda comes from a uniform

 msample[1,4] <- 4 # fixed lambda as in original publication
# msample[1,4] <- runif(n=1,min=0,max=8) # uniform sampler for lambda
# msample[1,4] <- rpois(n=1,lambda=4) # poisson draw from fixed lambda
# msample[1,4] <- rpois(n=1,lambda=runif(n=1,min=0,max=8)) # poisson variable lambda
  return(msample)
}

###################################################
# FUNCTION: ConstantPopIter
# iterate population series
# input:
# n0 = initial population vector
# A = transition matrix
# output: nt = matrix of stages x time with n 
#------------------------------------------------- 
ConstantPopIter <- function(n0=NULL,A=NULL,steps=NULL) {
if(is.null(n0) & is.null(A) & is.null(steps)) {
  steps <- 10
  n0 <- c(1500,23500,1400,500)
  A <- matrix(data=runif(16),nrow=4)
} 
# Create matrix for holding output of population sizes
nt <- matrix(data=rep(0,times=steps*dim(A)[1]),nrow=dim(A)[1])

# Fill the first entry with the initial vector
nt[,1] <- n0
# Iterate matrix multiplication
for (i in 2:ncol(nt)){
  
nt[,i] <- A%*%nt[,i-1]
nt[,i] <- trunc(nt[,i]) # truncate population sizes to integers
if(sum(nt[,i]) <=1) {
  nt[i] <- 0
  break
}
}

# Transform to relative abundances
nt <- apply(nt,2, function(x) x/sum(x))

# Transpose and then convert to long form
nt <- as.data.frame(t(nt))


colnames(nt) <- c("Recruits","Juveniles","NF Adults", "F Adults")


nt <- melt(nt,variable.name="Stage",value.name="N")
nt <- cbind(rep(1:(nrow(nt)/4),4),nt)
names(nt)[1] <- c("TimeStep")
# nt$N <- log10(nt$N)
  return(nt)
}

# This SinglePGen function converts the simple look-up functions for N to a N and P 
# multiple regression model.
# input: N and P point values and vector of experimental results
# output: single transition matrix probability
SinglePGen <- function(Npoint=runif(1),
                 Ppoint=runif(1),
                 survProbs=runif(8)){
  
# set up treatment levels from E&G 2002
N <- c(0,0.1,1.0,0,0,0.1,1.0,1.0)
P <- c(0,0,0,0.025,0.25,0.25,0.25,0.025)

# add in stage transition probabilities
trans <- survProbs

# combine into a single data frame
tDat <- data.frame(N,P,trans)

# fit linear model with interaction terms
regMod <- lm(trans~N+P+N*P,data=tDat)

# create data frame of input points
NPpoint <- data.frame(N=Npoint,P=Ppoint)

#hmap <- expand.grid(N=seq(0,1,length.out=10),P=seq(0,0.25,length.out=10))
predP <- predict(regMod,newdata=NPpoint)

#rescale if out of bounds and return
if(length(predP)==0) predP <- 0
if(predP<0) predP <- 0
if(predP>1) predP <- 1

return(predP)
}


# FUNCTION NPMatGen 
# Creates a single transition matrix by repeated calls to SinglePGen
NPMatGen <- function(Npoint=runif(1),
                     Ppoint=runif(1)) {
 # set up empty matrix 
#----------------------------------------
.mat <- matrix(0,nrow=4,
                ncol=4,
               dimnames=list(c("R_1", "J_1", "NF_1", "F_1"),
                             c("R_0", "J_0", "NF_0", "F_0")))
.mat[1,4] <- 4 # assumed fecundity
.mat[2,1] <- 0.10 # assumed recruitment survival
#----------------------------------------

# Loop through each of the transitions to fill
#----------------------------------------
TransL <- c("Juvenile Juvenile", "Juvenile NF-Adult", "Juvenile F-Adult",
            "NF-Adult Juvenile", "NF-Adult NF-Adult", "NF-Adult F-Adult",
            "F-Adult Juvenile", "F-Adult NF-Adult", "F-Adult F-Adult")
TreatL <- c("dH2O", "Low-N", "High-N", "Low-P", 
            "High-P", "N:P(1)", "N:P(2)", "N:P(3)")
# Nran <- rgamma(1,shape=0.1,scale=1)
# Pran <- rgamma(1,shape=0.1,scale=1)
  .mat[2,2] <- SinglePGen(Npoint=Npoint,Ppoint=Ppoint,survProbs=TransP[TreatL,TransL[1]])
  .mat[3,2] <- SinglePGen(Npoint=Npoint,Ppoint=Ppoint,survProbs=TransP[TreatL,TransL[2]])
#  .mat[4,2] <- SinglePGen(Npoint=Nran,Ppoint=Pran,survProbs=TransP[TreatL,TransL[3]])
  .mat[2,3] <- SinglePGen(Npoint=Npoint,Ppoint=Ppoint,survProbs=TransP[TreatL,TransL[4]])
  .mat[3,3] <- SinglePGen(Npoint=Npoint,Ppoint=Ppoint,survProbs=TransP[TreatL,TransL[5]])
  .mat[4,3] <- SinglePGen(Npoint=Npoint,Ppoint=Ppoint,survProbs=TransP[TreatL,TransL[6]])
  .mat[2,4] <- SinglePGen(Npoint=Npoint,Ppoint=Ppoint,survProbs=TransP[TreatL,TransL[7]])
  .mat[3,4] <- SinglePGen(Npoint=Npoint,Ppoint=Ppoint,survProbs=TransP[TreatL,TransL[8]])
  .mat[4,4] <- SinglePGen(Npoint=Npoint,Ppoint=Ppoint,survProbs=TransP[TreatL,TransL[9]])
  
# rescale if columnsums are > 1.0
  # if(sum(.mat[2:4,]) >= 1.0) print(.mat)
  for (z in 2:4) {
  surTot <- runif(1,min=0.98,max=1.00) #set shave limits here
  if(sum(.mat[2:4,z]) >= 1.0) {
    nonZ <- sum(.mat[2:4,z]>0)
    shave <- (sum(.mat[2:4,z]) -  surTot)/nonZ
    big <- which(.mat[2:4,z]>0) + 1
    .mat[big,z] <- .mat[big,z] - shave
  }
  }
  .mat[.mat<0] <- 0
  
  #------------------------------------------------------------
  # add logarithmic dimunition for 1 < N < 10
  if (1 < Npoint) {
    for (i in 2:4) {
      for (j in 2:4) {
      .mat[i,j] <- approx(x=c(0,1),y=c(.mat[i,j],0),xout=log10(Npoint),rule=2)$y 
        
      }
    }
  .mat[.mat<0] <- 0
  }
  
  
  #------------------------------------------------------------
lam <- lambda(.mat)
gen <- generation.time(.mat)
stab <- damping.ratio(.mat)
probPer <- as.numeric(lambda(.mat)>1) # binary for growing or shrinking population
# e <- elasticity(.mat)
# eMax <- which(e==max(e))
# if(eMax %in% 1:4) eStage="Recruit" else
# if(eMax %in% 5:8) eStage="Juvenile" else
# if(eMax %in% 9:12) eStage="NF-Adult" else
# if(eMax %in% 13:16) eStage="F-Adult"
# return(list(A=.mat,N=Nran,P=Pran,lambda=lam,gen=gen,damping=stab,eMax=eMax,eStage=eStage))
return(list(A=.mat,N=Npoint,P=Ppoint,lambda=lam,gen=gen,damping=stab,pPer=probPer))
                     }
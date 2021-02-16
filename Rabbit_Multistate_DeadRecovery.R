#####################################################################################
## Multi-state, dead-recovery model for 18 years of data from Turretfield rabbit pop. 
## Data collected by Biosecurity, South Australia, Department of Primary Industries and Regions
##
## Code accompanies paper in review.
##
## Barnett,LK, Prowse,TAA, Peacock, DE, Mutze, GJ, Sinclair, RG, Kovaliski,J,
## Cooke, BD, Bradshaw CJA. Previous exposure to myxomatosis reduces survival
## of European rabbits during outbreaks of rabbit haemorrhagic disease for
## biological control
##
##
## Louise Barnett
## November 2017
##
## Before running this script you will need to install 
## Program MARK from
## http://www.phidot.org/software/mark/downloads/
##
## Mac and Linux users might find this post helpful:
## http://www.phidot.org/forum/viewtopic.php?f=21&t=3233&p=10967&hilit=install+RMark#p10967
##
##
##
##
## testing how previous disease exposure/immunity state and recurring outbreaks
## affect rabbit survival (S) and immunity state transitions (Psi)
##
## immunity state / previous exposure categories:
## N - Immunity to neither virus
## M - Immunity to myxoma virus only
## R - Immunity to rabbit haemorrhagic disease virus (RHDV) only
## B - Immunity to both viruses
##
## 
## Age groups:
## Kittens <= 600 g (may have residual maternal immunity to RHD)
## Adults > 600 g (unlikely to have residual maternal immunity)
##
#####################################################################################


######################################################################
## Run Multistate Live-recapture dead-recovery Model on rabbit data ##
######################################################################

# clear the workspace
rm(list=ls())

# load required libraries
require(RMark)

# set the working directory to folder containing capture history
  # and trip covariate files
    # e.g.
# setwd("~/Dropbox/RabbitDisease")

######################################################################
#################### Import and Format Data #######################
######################################################################

# Import Trip Covariates
TripCovs<- read.csv(file="TripCovariates.csv")

# Import Capture History
CH<-read.csv(file="CaptureHist.csv")

# Subset to three columns
CH<- subset(CH, select=c("ch", "Rabbit", "InitAge"))
CH$ch<- as.character(CH$ch)

# set the number of trips
TripNo=107

######################################################################
################## Make design data for the model ####################
######################################################################

# Process data
mstrata.processed=process.data(CH, model="MSLiveDead",
                               strata.labels = c("N","M","R","B"),# labels for strata
                               groups=c("InitAge"),# set initial ages
                               age.var=1,
                               initial.ages =c(0,1),
                               time.intervals=TripCovs$Ints,# set intervals (prop. of mean)
                               nocc=TripNo) # number of capture occasions

# Make design data
ddl= make.design.data(mstrata.processed,
                      parameters=list(Psi=list(age.bins=c(0,1,(TripNo+3)), # bin ages
                                               subtract.stratum=c("N","M","R","B")),
                                      S=list(age.bins=c(0,1,(TripNo+2)))),
                      right=FALSE)

# Ignore warning

# Check age is working
head(ddl$S$age)
head(ddl$Psi$age)

# check the subtract stratum are correct
table(ddl$Psi[,c("stratum","tostratum")])
# should have 0s on the diagonal
  # for transitions estimated by subtraction

################################################################
############# Tidy up age covariates for analysis ##############
################################################################

## Set the levels of age (in the default age column)
levels(ddl$Psi$age)=c("a1kitten", "c1adult")
levels(ddl$S$age)=c("a1kitten","c1adult")
head(ddl$S$age)

## Now make a new age categories
# S
ddl$S$AgeCat<- 0
ddl$S$AgeCat[ddl$S$age=="a1kitten"]<- "Kitten"
ddl$S$AgeCat[ddl$S$age=="c1adult"]<- "Adult"

# Psi
ddl$Psi$AgeCat<- 0
ddl$Psi$AgeCat[ddl$Psi$age=="a1kitten"]<- "Kitten"
ddl$Psi$AgeCat[ddl$Psi$age=="c1adult"]<- "Adult"

# Make numeric columns/dummy variables for Kitten and Adult
# S
ddl$S$Adult<- ifelse(ddl$S$AgeCat=="Adult",1,0)
ddl$S$Kitten<- ifelse(ddl$S$AgeCat=="Kitten",1,0)

# Psi
ddl$Psi$Kitten<- ifelse(ddl$Psi$AgeCat=="Kitten",1,0)
ddl$Psi$Adult<- ifelse(ddl$Psi$AgeCat=="Adult",1,0)

################################################################
##### Add Trip Covariates to design data for p, S and Psi ######
################################################################

## Add standardised Trap Effort to capture probability (p) design data
dfp=data.frame(time=unique(ddl$p$time),TrapEffort=TripCovs$SEffort[2:(TripNo)])
ddl$p=merge_design.covariates(ddl$p,dfp)
head(ddl$p)

## Add Myxo to S
dfp=data.frame(time=unique(ddl$S$time),MV=TripCovs$MV[1:(TripNo)]) # make
ddl$S=merge_design.covariates(ddl$S,dfp)

## Add Myxo to Psi
dfp=data.frame(time=unique(ddl$Psi$time),MV=TripCovs$MV[1:(TripNo)])
ddl$Psi=merge_design.covariates(ddl$Psi,dfp)

## Add RHD to S
dfp=data.frame(time=unique(ddl$S$time),RHDV=TripCovs$RHDV[1:(TripNo)]) # make
ddl$S=merge_design.covariates(ddl$S,dfp)
head(ddl$S) # check

## Add RHD to Psi
dfp=data.frame(time=unique(ddl$Psi$time),RHDV=TripCovs$RHDV[1:(TripNo)]) 
ddl$Psi=merge_design.covariates(ddl$Psi,dfp)
head(ddl$Psi)

################################################################
##### fix values for transitions that are not possible     #####
################################################################

# Rabbits do not lose immunity
  # so set transitions that can't occur to zero
ddl$Psi$fix[ddl$Psi$stratum=="M"&ddl$Psi$tostratum=="N"] <- 0
ddl$Psi$fix[ddl$Psi$stratum=="R"&ddl$Psi$tostratum=="N"] <- 0
ddl$Psi$fix[ddl$Psi$stratum=="B"&ddl$Psi$tostratum=="N"] <- 0

ddl$Psi$fix[ddl$Psi$stratum=="M"&ddl$Psi$tostratum=="R"] <- 0
ddl$Psi$fix[ddl$Psi$stratum=="R"&ddl$Psi$tostratum=="M"] <- 0
ddl$Psi$fix[ddl$Psi$stratum=="B"&ddl$Psi$tostratum=="M"] <- 0
ddl$Psi$fix[ddl$Psi$stratum=="B"&ddl$Psi$tostratum=="R"] <- 0

################################################################
### Make dummy variables to set equal survival effects      ####
################################################################

# set equal survival effect for N & M during RHDV outbreaks
ddl$S$NMduringRHD<-0
ddl$S$NMduringRHD[ddl$S$stratum=="M"&ddl$S$RHDV==1]<-1
ddl$S$NMduringRHD[ddl$S$stratum=="N"&ddl$S$RHDV==1]<-1

# set equal survival effect for N & R during MV outbreaks
ddl$S$NRduringMV<-0
ddl$S$NRduringMV[ddl$S$stratum=="R"&ddl$S$MV==1]<-1
ddl$S$NRduringMV[ddl$S$stratum=="N"&ddl$S$MV==1]<-1

# Check this
head(ddl$S)

###############################################################
######################## Run Initial Model ####################
###############################################################

# Need to run a simple version of the model first
  # to set intial estimates (helps model converge)
run.init=function(){
  # Process data
  mstrata.processed=mstrata.processed
  # Create default design data
  mstrata.ddl=ddl
  
  # Create formula for capture probability
  p.stratum=list(formula=~TrapEffort)
  
  # additive effect of prev. exposure at all times
  S.stratum.d=list(formula=~-1+
                     Kitten+
                     Kitten:MV+
                     Kitten:RHDV+ 
                     stratum:Adult+ 
                     B:Adult:MV+
                     R:Adult:MV+
                     NMduringRHD:Adult:RHDV+
                     B:Adult:MV+
                     M:Adult:MV+
                     NRduringMV:Adult:MV)
  
  # Formula for Psi
  Psi.stratum=list(formula=~-1+
                     Kitten+
                     Kitten:MV+
                     Kitten:RHDV+
                     stratum:tostratum:Adult +
                     stratum:tostratum:Adult:MV +
                     stratum:tostratum:Adult:RHDV,
                   link="logit")
  
  
  model.list=create.model.list("MSLiveDead")
  
  mstrata.results=mark.wrapper(model.list,data=mstrata.processed,
                               ddl=ddl)
  return(mstrata.results) } 

############################################################
init<- run.init()


################################################################
############ Set up a function to run the model ###############
###############################################################

########
run.mstrata=function(){
  # Process data
  mstrata.processed=mstrata.processed
  # Create default design data
  mstrata.ddl=ddl

  # Create formula 
  
  p.stratum=list(formula=~TrapEffort)

  # All synergistic
  S.stratum.a=list(formula=~-1+
                     Kitten+
                     Kitten:MV+
                     Kitten:RHDV+
                     stratum:Adult+
                     stratum:Adult:MV+
                     stratum:Adult:RHDV)
  
  # only synergistic for Myxo positive in RHDV outbreaks
  # additive for during Myxo outbreaks
   S.stratum.b=list(formula=~-1+
                      Kitten+
                      Kitten:MV+
                      Kitten:RHDV+
                      stratum:Adult+
                      stratum:Adult:RHDV+
                      B:Adult:MV+
                      M:Adult:MV+
                      NRduringMV:Adult:MV)
   
  # # only synergistic for RHDV positive during myxo outbreaks
  # # additive during RHD outbreaks
   S.stratum.c=list(formula=~-1+
                      Kitten+
                      Kitten:MV+
                      Kitten:RHDV+
                      stratum:Adult+
                      stratum:Adult:MV+
                      B:Adult:RHDV+
                      R:Adult:RHDV+
                      NMduringRHD:Adult:RHDV)
   
   # additive effect of prev. exposure at all times
   S.stratum.d=list(formula=~-1+
                      Kitten+
                      Kitten:MV+
                      Kitten:RHDV+ 
                      stratum:Adult+ 
                      B:Adult:RHDV+
                      R:Adult:RHDV+
                      NMduringRHD:Adult:RHDV+
                      B:Adult:MV+
                      M:Adult:MV+
                      NRduringMV:Adult:MV)
   
   # Formula for Psi
   Psi.stratum=list(formula=~-1+
                      Kitten+
                      Kitten:MV+
                      Kitten:RHDV+
                      stratum:tostratum:Adult +
                      stratum:tostratum:Adult:MV +
                      stratum:tostratum:Adult:RHDV,
                     link="logit")
 

  model.list=create.model.list("MSLiveDead")
  
  mstrata.results=mark.wrapper(model.list,data=mstrata.processed,
                               ddl=ddl, initial=init[[1]])
  return(mstrata.results) } 

results<- run.mstrata()

# cleanup RMark Output files
cleanup(ask=FALSE)

################################################################
###################### View output #############################
################################################################ 

# View model table
results

# Select model to use for figures etc.
Best<- results[[2]]

#### Save output ####
save(file="RabbitDiseaseModels.RData", results, ddl, mstrata.processed)

## Now run Script 2: 'Rabbit_Multistate_Plotting_Output.R' using saved output

##################################################################



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
## Plotting estimates for survival (S) and transition probability (Psi)
## between immunity states
##
## Before running this script you will need to run the analysis
## called 'Rabbit_Multistate_DeadRecovery.R'
## 
## 
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



# load required libraries
require(ggplot2)
require(RMark)


# load Marklist with all the models in it
load(file="RabbitDiseaseModels.RData")


# Fix up model table
results$model.table=model.table(results,
                                use.lnl=TRUE,
                                model.name=FALSE)

# Select the model to predict from
ModelFull<- results[[2]] # (2nd was best fit)
ModelFull$model.name # look at model name

##########################################################################################
############################## Predict for Survival (S) ##################################
##########################################################################################

# Make dataframe of model indices
data=data.frame(RHDV=c(c(0,1,0),rep(c(0,1,0),4)),
                MV=c(c(0,0,1),rep(c(0,0,1),4)),
                Age=c(rep("kitten",3), rep("adult",12)),
                stratum=c(rep("Kitten",3), rep("N",3),rep("M",3), rep("R", 3), rep("B",3)),
                index=c(min(ddl$S[ddl$S$Kitten==1&ddl$S$MV==0&ddl$S$RHDV==0,3]),# kitten 
                        min(ddl$S[ddl$S$Kitten==1&ddl$S$MV==0&ddl$S$RHDV==1,3]),# kitten
                        min(ddl$S[ddl$S$Kitten==1&ddl$S$MV==1&ddl$S$RHDV==0,3]),# kitten
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==0&ddl$S$RHDV==0&ddl$S$stratum=="N",3]),## Neither ##
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==0&ddl$S$RHDV==1&ddl$S$stratum=="N",3]), # RHD only
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==1&ddl$S$RHDV==0&ddl$S$stratum=="N",3]),# Myxo only
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==0&ddl$S$RHDV==0&ddl$S$stratum=="M",3]),## Neither ##
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==0&ddl$S$RHDV==1&ddl$S$stratum=="M",3]),# RHD only
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==1&ddl$S$RHDV==0&ddl$S$stratum=="M",3]),# Myxo only
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==0&ddl$S$RHDV==0&ddl$S$stratum=="R",3]),# Neither ##
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==0&ddl$S$RHDV==1&ddl$S$stratum=="R",3]),# RHD only
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==1&ddl$S$RHDV==0&ddl$S$stratum=="R",3]),# myxo only
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==0&ddl$S$RHDV==0&ddl$S$stratum=="B",3]),# Neither ##
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==0&ddl$S$RHDV==1&ddl$S$stratum=="B",3]),# RHD only
                        min(ddl$S[ddl$S$Adult==1&ddl$S$MV==1&ddl$S$RHDV==0&ddl$S$stratum=="B",3])))# Myxo only
# Generate predictions
Pred=data.frame(covariate.predictions(ModelFull,data=data)$estimates)

## Make labels for Outbreak
Pred$Outbreak<- "A. No Outbreak"
Pred$Outbreak[Pred$RHDV==1&Pred$MV==0]<- "C. RHDV Outbreak"
Pred$Outbreak[Pred$RHDV==0&Pred$MV==1]<- "B. MV Outbreak"

# Convert Outbreak to factor
Pred$Outbreak<- factor(Pred$Outbreak, levels=c("A. No Outbreak", "B. MV Outbreak", "C. RHDV Outbreak"))

# Convert stratum to factor
Pred$stratum<- factor(Pred$stratum, levels=c("Kitten","N", "M", "R", "B"))

## Make figure for Psi and save as a PDF in the working directory ##
pdf(file="RabbitDisease_Survival.pdf", h=3, w=6)
ggplot(Pred, aes(y=estimate, x=stratum))+
  geom_linerange(aes(ymin=lcl, ymax=ucl))+
  geom_point()+
  facet_wrap(~Outbreak)+
  theme_classic()+
  theme(legend.title=element_blank())+
  labs(y="Survival probability (S)",
       x="Immunity state")+theme(axis.text.x=element_text(hjust=1, angle=30))+
  scale_y_continuous(limits=c(0.25,1))
dev.off()

## Export output as CSV ##
S<- subset(Pred, select=c("estimate", "se", "lcl","ucl", "Outbreak","stratum"))
S<-S[order(S$Outbreak, S$stratum),]

# save CSV
write.csv(file="SurvivalEstimates.csv", S)

##########################################################################################
####################### Predict for Transition Probability (Psi) #########################
##########################################################################################

# Make dataframe of model indices
data=data.frame(RHDV=c(0, rep(c(0,1,0),5)),
                MV=c(0,rep(c(0,0,1),5)),
                Age=c("kitten", rep("adult",15)),
                transition=c("N",rep("N to B",3), rep("M to B",3), rep("R to B", 3), rep("N to R",3), rep("N to M",3)),
                index=c(min(ddl$Psi[ddl$Psi$Kitten==1,3]),# kitten all levels # neither
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==0&ddl$Psi$stratum=="N"&ddl$Psi$toB==1,3]),## Neither ##
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==1&ddl$Psi$stratum=="N"&ddl$Psi$toB==1,3]), # RHD only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==1&ddl$Psi$RHDV==0&ddl$Psi$stratum=="N"&ddl$Psi$toB==1,3]),# Myxo only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==0&ddl$Psi$stratum=="M"&ddl$Psi$toB==1,3]),## Neither ##
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==1&ddl$Psi$stratum=="M"&ddl$Psi$toB==1,3]),# RHD only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==1&ddl$Psi$RHDV==0&ddl$Psi$stratum=="M"&ddl$Psi$toB==1,3]),# Myxo only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==0&ddl$Psi$stratum=="R"&ddl$Psi$toB==1,3]),# Neither ##
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==1&ddl$Psi$stratum=="R"&ddl$Psi$toB==1,3]),# RHD only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==1&ddl$Psi$RHDV==0&ddl$Psi$stratum=="R"&ddl$Psi$toB==1,3]),# myxo only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==0&ddl$Psi$stratum=="N"&ddl$Psi$toR==1,3]),# Neither ##
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==1&ddl$Psi$stratum=="N"&ddl$Psi$toR==1,3]),# RHD only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==1&ddl$Psi$RHDV==0&ddl$Psi$stratum=="N"&ddl$Psi$toR==1,3]),# Myxo only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==0&ddl$Psi$stratum=="N"&ddl$Psi$toM==1,3]),# Neither ##
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==0&ddl$Psi$RHDV==1&ddl$Psi$stratum=="N"&ddl$Psi$toM==1,3]),# RHD only
                        min(ddl$Psi[ddl$Psi$Adult==1&ddl$Psi$MV==1&ddl$Psi$RHDV==0&ddl$Psi$stratum=="N"&ddl$Psi$toM==1,3])))# Myxo only
# Generate predictions
PredP=data.frame(covariate.predictions(ModelFull,data=data)$estimates)

## Make labels for outbreak
PredP$Outbreak<- "A. No Outbreak"
PredP$Outbreak[PredP$RHDV==1&PredP$MV==0]<- "C. RHDV Outbreak"
PredP$Outbreak[PredP$RHDV==0&PredP$MV==1]<- "B. MV Outbreak"
PredP$Outbreak<- factor(PredP$Outbreak, levels=c("A. No Outbreak", "B. MV Outbreak", "C. RHDV Outbreak"))

# Tidy up predictions
PredP$transition<- factor(PredP$transition, levels=c("N", "N to M","N to R", "N to B", "M to B", "R to B"))
PredP<- subset(PredP,Age=="adult") # subset to only adults

## Make figure for Psi and save as a PDF in the working directory ##
pdf(file="RabbitDisease_TransitionProbability.pdf", h=3, w=6)
ggplot(PredP, aes(y=estimate, x=transition))+
  geom_linerange(aes(ymin=lcl, ymax=ucl))+
  geom_point()+
  facet_wrap(~Outbreak)+
  theme_classic()+
  theme(axis.text.x=element_text(hjust=1, angle=30))+
  scale_y_continuous(expression("Transition probability (" ~ psi ~ ")"))+
  scale_x_discrete("Immunity state transition")
dev.off()


## Export output as CSV ##
Psi<- subset(PredP, select=c("estimate", "se", "lcl","ucl", "Outbreak","transition"))
Psi<-Psi[order(Psi$Outbreak, Psi$transition),]

# save CSV
write.csv(file="PsiEstimates.csv", Psi)




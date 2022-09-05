######################################
######################################
#
#    Mediation analysis G-formula
#
######################################
######################################

# This is the R code for the illustrative case for the
# paper "Neighbourhood-related socioeconomic perinatal health
# inequalities: an illustration of the mediational
# g-formula and considerations for the big data context."
# Burgos Ochoa et. al.

# This code has been written thinking of clarity but it has
# not been optimized.
# Modify this code to accommodate your dataset and research question.
# Working knowledge of R, use of functions and arrays is required.

# Load required packages
library(dplyr)

# Set working directory 
setwd("H:/Health Inequality LBO/Leefbaarheid")

# Load leefbaarometer social dimension
load("H:/Health Inequality LBO/Leefbaarheid/Leefbaarometer/Leefbaarometer3_fys_soc.RData")

# Load data
load(file="H:/Health Inequality LBO/Crime/Environment/PRN_quintiles_whole.RData")
PRN_dummy <- prn_pc_class_com

# Merge PRN and social scores
PRN_dummy1 <- merge(PRN_dummy, soc, by.x = "POSTCODE.FIN", by.y = "PC4", all.x = T)

# Keep relevant variables
PRN_dummy1 <- PRN_dummy1 %>% dplyr::select(RINPersoon_moeder, RINPersoon_kind, JAAR, POSTCODE.FIN, parity, LFT_cat_3, cat_std.perc.disp.inc.Q, LMH_edu_level.R, ETNGRP2, SGA, PRETERM, LBW, SCP.q.r, Q_NWestM, Q.WOZ, oad_pc4_new_q_year, soc.2014, soc.2018, soc.2014_q, soc.2018_q, soc.2014_dsv, soc.2018_dsv, Std.disp.income.H.Final)

# Assign social scores: assign then classify
PRN_dummy1$soc_score_lfbr <- case_when(PRN_dummy1$JAAR <= 2015 ~ PRN_dummy1$soc.2014, 
                                  PRN_dummy1$JAAR > 2014 ~ PRN_dummy1$soc.2018)

PRN_dummy1$soc_score_lfbr_q <- ntile(PRN_dummy1$soc_score_lfbr, 5) # make quintiles: 1= lowes, 5= highest
PRN_dummy1$soc_score_lfbr_disvt <- ifelse(PRN_dummy1$soc_score_lfbr_q==1, 1,0) # classify as disadvataged if in q1

# Check frequencies and proportions 
prop.table(table(PRN_dummy1$soc_score_lfbr_disvt))*100 # assign scores to all and then classify

# Remove redundant datasets 
remove(prn_pc_class_com, PRN_dummy)
gc()

# Create variables for Disadvantaged SES
PRN_dummy1$ses_disvt <- ifelse(PRN_dummy1$SCP.q.r==5, 1,0) # classify as disadvataged if in q5 (here the qs are reversed)

# Expit function to transform predictions
expit <- function (x) { exp(x) / (1 + exp(x))}


#*------------------------------
#*------------------------------
#
# SYNTAX FOR SIMPLE MODEL WITH 1
# BINARY EXPOSURE AND MEDIATOR
#
#*------------------------------
#*------------------------------


#----------------------------
# FORMULA FOR THE MEDIATOR
#----------------------------

formula.mediator <- c(
  "outcome ~ ses_disvt + factor(JAAR) + parity + ETNGRP2 + factor(cat_std.perc.disp.inc.Q) + factor(LMH_edu_level.R) + factor(LFT_cat_3) + factor(Q_NWestM) + factor(Q.WOZ) "
)

formula.soc_score_lfbr_disvt <- as.formula(sub('outcome', 'soc_score_lfbr_disvt', formula.mediator))

#-------------------------------
# CREATE ARRAYS TO STORE VALUES
#-------------------------------
setwd("H:/Health Inequality LBO/Crime/Environment") # Set working directory

bssize <- 5 # Bootstrap iterations
msize <- 5 # Monte Carlo iterations
nvar <- 2 # 1 mediator and 1 outcome variable.
ngroups <- 1 # If set to 1, it creates a matrix for the population.
# If interested in stratifying the analysis this can be set to
# accommodate the number of groups.
med.scenarios <- 1 # For TDE 

nc.array <- rep(NA, msize*nvar*ngroups) # Array for natural course (NC)
dim(nc.array) <- c(msize, nvar, ngroups) # specifies dimension of array above

cf.array1 <- nc.array # Copy array for counterfactual scenario 1 (CF1)
cf.array2 <- nc.array # Copy array for counterfactual scenario 2  (CF2)


# Array for mediation scenario means
outcomes <- 1
med.array <- rep(NA, msize*med.scenarios*outcomes*ngroups) 
rows <- outcomes*med.scenarios
dim(med.array) <- c(msize, rows, ngroups)

###################################
# ARRAYS FOR BOOTSTRAP VALUES
##################################

# Natural course and 2 counterfactuals
bs.nc.array <- rep(NA,bssize*nvar*ngroups) # NC
dim(bs.nc.array) <- c(bssize,nvar,ngroups)
bs.cf.array1 <- bs.nc.array # CF1
bs.cf.array2 <- bs.nc.array # CF2

# Mediation scenario arrays
bs.tde.array <- rep(NA,bssize*med.scenarios*outcomes*ngroups)
dim(bs.tde.array) <- c(bssize,rows,ngroups)

##################################
# ARRAYS FOR EFFECT CALCULATIONS
##################################
# TE=Total effect, NDE= Natural direct effect
# NIE= Natural indirect effect.

# TE for all the variables + subgroup
te.bs.array <- rep(NA,bssize*nvar*ngroups)
dim(te.bs.array) <- c(bssize,nvar,ngroups)

# Make copy for relative effect
te.rel.bs.array <- te.bs.array

# Copies for IE and % mediated
bs.ie.array <- bs.tde.array #IE
bs.ie.array1 <- bs.ie.array
bs.perc.med.array <- bs.tde.array # % mediated


##################
# THE G-FORMULA
##################

library(cfdecomp)

# Start bootstrap loops (bs)
system.time(for(bs in 1:bssize) { # system.time to obtain how long does it take to run
  
  # Sample with replacement, if multilevel structure then clusters are sampled
  # as done below using the function cluster.resample from cfdecomp package. 
  # If there is no need to account for clustering of individuals, then change
  # cluster.resample(mydata, cluster.name="cluster") for: sample(mydata, replace=TRUE)
  
  # data= your dataset, cluster.name = variable name representing cluster, i.e., (POSTCODE.FIN) neighbourhood
  sample.dat <- cluster.resample(data = PRN_dummy1, cluster.name = "POSTCODE.FIN")
  n <- dim(sample.dat)[1] # number of cases
  
  #----------------------------------------
  # Fit underlying model for the mediator 
  #----------------------------------------
  fit.soc_score_lfbr_disvt <- glm(formula.soc_score_lfbr_disvt, family = "binomial", data= sample.dat, model=F, y=F) 
  
  #----------------------------------------
  # Fit underlying model for the outcome
  #----------------------------------------
  fit.SGA <- glm(SGA ~ ses_disvt + soc_score_lfbr_disvt + factor(JAAR) + parity + ETNGRP2 + factor(cat_std.perc.disp.inc.Q) + factor(LMH_edu_level.R) + factor(LFT_cat_3) + factor(Q_NWestM) + factor(Q.WOZ), family= "binomial", data= sample.dat, model=F, y=F)
  
  print("hello")
  
  #-----------------------
  # Start simulation step 
  #-----------------------
  
  # Monte Carlo loops (m)
 
   for(m in 1:msize) {
    
    ######################
    # NATURAL COURSE (NC)
    ######################
    
    # Copy data
    
    sample.xMx <- sample.dat
    
    # Replace mediator values with newly predicted values 
    # Sample from binomial distribution (rbinom) using predicted probabilities 
    # from underlying model for the mediator
    # Use expit function as shown below to transform predictions.
    # Alternatively within the predict() function use type=response. 
    # Same applies to all simulations.
    sample.xMx$soc_score_lfbr_disvt <- rbinom(nrow(sample.xMx), 1, expit(predict(object = fit.soc_score_lfbr_disvt, newdata=sample.xMx)))
    
    # Last, Simulate new values for the outcome using predicted probabilities 
    # from underlying model for the outcome after mediator has been simulated.
    # Same applies to all simulations.
    sample.xMx$SGA <- rbinom(nrow(sample.xMx), 1, expit(predict(object = fit.SGA, newdata=sample.xMx)))
    
    
    #######################
    # COUNTERFACTUAL STEP
    #######################
    
    #------------------------------------
    # CF1: FIX ALL TO DISADVANTAGED SES
    #------------------------------------
    
    # Copy data
    sample.xstarMxstar1 <- sample.dat 
    
    # Make an intervention: All to disadvantaged SES (SES=1)
    sample.xstarMxstar1$ses_disvt <- 1
    
    # Simulate new values for mediator similarly to NC simulation step.
    sample.xstarMxstar1$soc_score_lfbr_disvt <- rbinom(nrow(sample.xstarMxstar1), 1, expit(predict(object = fit.soc_score_lfbr_disvt, newdata=sample.xstarMxstar1)))
    
    # Simulate new values for the outcome similarly to NC simulation step.
    sample.xstarMxstar1$SGA <- rbinom(nrow(sample.xstarMxstar1), 1, expit(predict(object = fit.SGA, newdata=sample.xstarMxstar1)))
    
    #---------------------------------------
    # CF2: FIX ALL TO NON-DISADVANTAGED SES
    #---------------------------------------
    
    # Copy data
    sample.xstarMxstar2 <- sample.dat 
    
    # Make an intervention: All to non-disadvantaged SES (SES=0)
    sample.xstarMxstar2$ses_disvt <- 0
    
    
    # Simulate new values for mediators
    sample.xstarMxstar2$soc_score_lfbr_disvt <- rbinom(nrow(sample.xstarMxstar2), 1, expit(predict(object = fit.soc_score_lfbr_disvt, newdata=sample.xstarMxstar2)))
    
    # Simulate new values for the outcome
    sample.xstarMxstar2$SGA <- rbinom(nrow(sample.xstarMxstar2), 1, expit(predict(object = fit.SGA, newdata=sample.xstarMxstar2)))
    
    
    #--------------------
    # MEDIATION SCENARIO
    #--------------------
    
    # Fix SES to disadvantaged (SES=1) but Mediator values under SES=0.
    
    # Copy data 
    sample.xstarMx <- sample.dat 
    
    # Neighbourhood SES to all DISADVANTAGED SES
    sample.xstarMx$ses_disvt <- 1
    
    
    # Draw mediator values from counterfactual scenario ALL NON-DISADVANTAGED.
    sample.xstarMx$soc_score_lfbr_disvt <-  sample.xstarMxstar2$soc_score_lfbr_disvt 
    
    
    # Simulate outcome as in previous steps.
    sample.xstarMx$SGA <- rbinom(nrow(sample.xstarMx), 1, expit(predict(object = fit.SGA, newdata=sample.xstarMx)))
    
    
    ###############
    ###############
    #  SAVE MEANS #
    ###############
    ###############
    
    
    # Save means of the current Monte Carlo (MC) loop 
    
    # Natural course
    nc.array[m,1,1] <- mean(sample.xMx$SGA)
    nc.array[m,2,1] <- mean(sample.xMx$soc_score_lfbr_disvt)
    
    
    # Counterfactual 1 (all disadvantaged SES)
    cf.array1[m,1,1] <- mean(sample.xstarMxstar1$SGA)
    cf.array1[m,2,1] <- mean(sample.xstarMxstar1$soc_score_lfbr_disvt)
    
    
    # Counterfactual 2 (all non-disadvantaged SES)
    cf.array2[m,1,1] <- mean(sample.xstarMxstar2$SGA)
    cf.array2[m,2,1] <- mean(sample.xstarMxstar2$soc_score_lfbr_disvt)
    
    # Mediation scenario
    med.array[m,1,1] <- mean(sample.xstarMx$SGA) # general SGA
    
    print(m)
    
  } # End MC loop
  
  
  # After all the MC iterations,
  # save the bootstrap results
  # Calculate effects
  
  ##############
  #      TE    #
  ##############
  
  for(v in 1:nvar){
    
    # v is the variable of interest, i.e. mediator or outcome.
    # Average over the MC iterations to get a stable estimate
    # For the particular bootstrap iteration
    te.bs.array[bs,v,1] <- mean(cf.array2[,v,1])-mean(cf.array1[,v,1])
    
    # Relative effects can be calculated here as done below for the TE, 
    # else, they can be calculated from the averaged bootstrap outcome 
    # means as 1- (CF2/CF1) *100. This value is interpreted as the percentage 
    # change in SGA due to the hypothetical improvement in SES from
    # disadvantaged to advantaged. A negative number refers to a reduction in SGA.
    te.rel.bs.array[bs,v,1] <- 1-(mean(cf.array2[,v,1])/mean(cf.array1[,v,1]))
  }
  
  ###############
  # NDE and NIE #
  ###############
  # Next, calculate the natural direct effect
  # This is done by subtracting the CF2 mean
  # from the mean of the mediation scenario (MedArray)

  ## NDE for SGA ##
  # Column=1 is the column where the SGA mean is located
  bs.tde.array[bs,1,1] <- mean(cf.array2[,1,1] - med.array[,1,1])
  
  # The indirect effect, NIE, is calculated by  Mediation scenario - CF1
  bs.ie.array[bs,1,1] <- mean(med.array[,1,1]) - mean(cf.array1[,1,1])
  
  
  #Percentage mediated will be derived from these
  #in this current bootstrap
  bs.perc.med.array[bs,1,1] <- bs.ie.array[bs,1,1]/te.bs.array[bs,1,1]
  
  
  #Save means from NC and CFs scenarios
  # to assess models against gross misspecification.
  for(v in 1:nvar){
    bs.nc.array[bs,v,1] <- mean(nc.array[,v,1])
    bs.cf.array1[bs,v,1] <- mean(cf.array1[,v,1])
    bs.cf.array2[bs,v,1] <- mean(cf.array2[,v,1])
    
  } # Bracket CF and NC means
  
  print(bs)
} # End bootstrap iteration

) #system time




#################
# Check results 
#################
setwd("H:/Health Inequality LBO/Leefbaarheid/Environment")
save(bs.nc.array, te.bs.array, te.rel.bs.array, bs.tde.array, bs.ie.array, bs.ie.array1, bs.perc.med.array, bs.cf.array1, bs.cf.array2, file="BS_SGA_LBR_r_2.RData")

# NC and CF1, CF2
colMeans(bs.nc.array)
colMeans(bs.cf.array1)
colMeans(bs.cf.array2)



# Total effect 
colMeans(te.bs.array)
quantile(te.bs.array[,1,1], probs = c(0.25,0.75)) # Obtain 25% and 75%
apply(te.bs.array, 2, quantile, probs = c(0.25,0.75)) # Obtain 25% and 75%, 2 refers to column.


# Total direct effect
colMeans(bs.tde.array)

options(scipen = 999)
TDE <- colMeans(bs.tde.array)


# Indirect effect
colMeans(bs.ie.array) 
quantile(bs.ie.array, probs = c(0.25,0.75)) 

# Percentage mediated
colMeans(bs.perc.med.array)*100
quantile(bs.perc.med.array[,3,1], probs = c(0.25,0.75)) # Obtain 25% and 75%

options(scipen = 999)




##################
# CHECK RESULTS 
##################

# Counterfactual scenarios means
CF1 <- data.frame(CF1.mean = colMeans(bs.cf.array1, na.rm = T))
CF2 <- data.frame(CF2.mean = colMeans(bs.cf.array2, na.rm = T))


### Absolute effects: TE, NDE, NIE ###

# TE on outcome (SGA) and mediator (social environment)
te <- data.frame(te.bs.array)
names.te <- c("SGA", "Soc_env") 
colnames(te) <- names.te
# Mean value
te.mean <- data.frame(te.mean = (colMeans(te, na.rm = T)))
# 95% CI
te.low <- data.frame(te.low = apply(X= te, MARGIN = 2, FUN=function(x) {(quantile(x, probs = 0.25))})) # Obtain 25% 
te.up <- data.frame(te.up = apply(X= te, MARGIN = 2, FUN=function(x) {(quantile(x, probs = 0.75))}))# obtain 75%
# Bind columns
te_m_CI <- data.frame(cbind(te.mean, te.low, te.up))

# NDE on SGA
tde <- colMeans(bs.tde.array)
names.tde <- "SGA"
row.names(tde) <- names.tde
# Mean value
tde.mean <- data.frame(tde.mean = (colMeans(bs.tde.array))) 
# 95% CI
tde.low <- data.frame(tde.low = apply(X = bs.tde.array, MARGIN = 2, FUN=function(x) {(quantile(x, probs = 0.25))})) # Obtain 25% 
tde.up <- data.frame(tde.up = apply(X = bs.tde.array, MARGIN = 2, FUN=function(x) {(quantile(x, probs = 0.75))}))# obtain 75%
# Bind columns
tde_m_CI <- data.frame(cbind(tde.mean, tde.low, tde.up))
row.names(tde_m_CI) <- names.tde

# NIE on SGA
names.tie <- "SGA"
# Mean value
tie.mean <- data.frame(tie.mean = round(colMeans(bs.ie.array), digits = 7)) 
# 95% CI
tie.low <- data.frame(tie.low = apply(X = bs.ie.array, MARGIN = 2, FUN=function(x) {round(quantile(x, probs = 0.25), digits = 7)})) # Obtain 25% 
tie.up <- data.frame(tie.up = apply(X = bs.ie.array, MARGIN = 2, FUN=function(x) {round(quantile(x, probs = 0.75), digits = 7)}))# obtain 75%
# Bind columns
tie_m_CI <- data.frame(cbind(tie.mean, tie.low, tie.up))
rownames(tie_m_CI) <- names.tie

# RELATIVE EFFECT VALUES can be calculated from the values obtained above. 
# Relative TE on SGA = Absolute TE / CF1 mean * 100, same procedure for NDE and NIE. 




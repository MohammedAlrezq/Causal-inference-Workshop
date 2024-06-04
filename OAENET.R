install.packages("msaenet")
install.packages("glmnet")
install.packages("MatchIt")
install.packages("lmtest")
install.packages("phonTools")
install.packages("MASS")
install.packages("sandwich")
install.packages("CovSel")
install.packages("mlbench")
install.packages("mltools")
install.packages("data.table")
install.packages("matrixStats")
install.packages("dplyr")
install.packages("tictoc")
install.packages("boot")

########################################
install.packages("msaenet")
library(msaenet)
library(glmnet)
library(MatchIt)
library(lmtest)
library(phonTools)
library(MASS)
library(sandwich)
library(CovSel)
#install.packages("mlbench")
library(mlbench)
#library(rJava)
#library(CovSelHigh)
library(mltools)
library(data.table)
library("matrixStats")
library(dplyr)
#install.packages("dplyr")
library(dplyr)
library(tictoc)
# install.packages("boot")
library(boot)
########################################################

################################
#Processing data

raw_data = read.csv("Statsig.csv")
raw_data = subset(raw_data, select = -c(X) ) # deleting extra column 

#####################################

itr_max = 1

max_var = 1

# #declare a zero matrix to hold att
att.onet = zeros(itr_max)

# #declare zero matrices to store the indexes of selected variables
select.var.list.onet = zeros(itr_max, max_var)

# #Declare zero matrices to store the total number of variables selected
total.num.var.onet = zeros(itr_max)

# #Declare zero matrices to store the total number of variables selected
time.onet = zeros(itr_max)


#########

#will have to change that in the future

for (i in 1:1){
  itr = 1

########################
# Preparing the covariates, outcomes, and treat data frame
# i might need to change the variabels settign for other experiment 

df = raw_data
Y = df$treatment_flag #enter your outcome variable 
A = df$shg_flag #enter your treatment variable 

############################

XA = subset(df, select = -c(treatment_flag)) #outcome
XX = subset(XA, select = -c(shg_flag)) #treatment


############################
# Applying Outcome adaptive elastic net
# Step 1: Fitting Outcome Model Logistic regression for penalty

my_logit_mod = glm(treatment_flag ~ ., data = df, family = "binomial")
odds_ratio = (coef(my_logit_mod))
odds_ratio
odds_ratio = odds_ratio[-c(1,ncol(df))] # remove odds ratio for intercept and udpypnr  #df <- subset(df, select = -c(a, c))

odds_ratio

# Setting the penalty
gamma = 5

penalty = 1/(abs(odds_ratio))^gamma

penalty[is.na(penalty)] <- 10000000000 # large penalty

#Perform cross validation
tic()

K.cv.fit = cv.glmnet(data.matrix(XX), data.matrix(A),  type.measure = "mse",
                     nfolds = 5, gamma = c(0, 0.25, 0.5, 0.75, 1), relax = TRUE, family="binomial") #check

cv.alpha = K.cv.fit$relaxed
alpha.opt = cv.alpha$gamma.1se

#Step 2: Fitting Treatment Model 
fit2=glmnet(as.matrix(XX), as.matrix(A), alpha =alpha.opt, lambda = K.cv.fit$lambda.1se, penalty.factor = penalty , family="binomial")

t=toc()
time.onet [itr]= t$toc - t$tic

beta_ps_raw = as.matrix(coef(fit2))
beta_ps_raw = as.matrix(coef(fit2, s = 0.02)) # check this 
beta_ps_allvar = as.matrix(beta_ps_raw[2:(nrow(beta_ps_raw))]) #remove intercept to the last item

beta_ps_non_zero = row(beta_ps_allvar)[which(abs(beta_ps_allvar) >= 0.1)] # check


xx_true = XX[, beta_ps_non_zero]

#Variables selected by OAENet
onet.true.var.list = names(xx_true)
treat.form = formula(paste("treatment_flag~",paste(onet.true.var.list,collapse="+"))) # here we might i need change for other experiment 

## performing matching using Nearest Neighbor Matching 
mm = matchit(treat.form, data = df, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "control")

#Calculating Treatment Effect
effect.form = formula(paste("treatment_flag ~ shg_flag+",paste(onet.true.var.list,collapse="+")))
fit.effect = lm(effect.form, data = match.data(mm))
final.coef = fit.effect$coefficients
att.onet[itr] = final.coef[2]

print("OAEnet")
print(att.onet[itr])
print("Variables selected by OAENet")
print(onet.true.var.list)
}


# avg treatmetn effect: 9% means that if an individual who participant for treatment is 9% higher to complete treatment than those who do not. Keep in mind that the number (%) might change for each run , but the feature selection are the same (same variables)
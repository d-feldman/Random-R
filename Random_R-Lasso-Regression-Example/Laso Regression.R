###laso regression.
#make sure you install libraries first in your console
#libraries
library(glmnet)
library(tidyr)
library(selectiveInference)

###read data###
#pathway to data
setwd('/Users/danielfeldman/Desktop')
#read into data frame
df <- read.table('STEP1_MNMS_Reducedv2_BaselineNPT1.txt', 
                  header = T, sep =',', dec = '.')
#omit null values of dv
df1 <- df %>% drop_na()

###Setup for laso regression##
###dependant variable
y <- df1$Factor1_EM


#matrix of predictor variables
x <- data.matrix(df1[, c("EMcluster07","Gender..0...Male..1...Female.", "Education",
                            "Group..0...HC..1...rMDD.or.eBP.", "EMcluster05", "Age",
                            "EMcluster04", "EMcluster01", "EMcluster06",
                            "EMcluster03", "Age2GroupINT", "EMcluster02")])

#Simple Model
simple_model <- glmnet(x, y, alpha = 1)

#k-fol cross-validation to find lamba value
cv_model <- cv.glmnet(x, y, alpha=1)

#find best lambda that minimizes test mean squared error
best_lambda <- cv_model$lambda.min
best_lambda

#find best beta
best_beta <- coef(simple_model, x = x, y = y, s = (best_lambda/(factorial(12))),  
                                                   exact = T)[-1]

###ANALYZE###

#find coefficient of best model
best_model <-glmnet(x, y, alpha =1, lambda = best_lambda)
print(coef(best_model, s = 0.05))



#find R-squared-- this is the percentage that the best model is able
#to explain the variation in response values based on the training data
#
rsq <- cv_model$glmnet.fit$dev.ratio
rsq_mean <- mean(rsq)
plot(cv_model$lambda,rsq)

###Visualize
#looking at percent deviance from each coefficient 
plot(simple_model, xvar='dev', label = T)

#plotting all rsq values
plot(cv_model$lambda,rsq)

# produce plot of test mean squared error by lambda value
plot(cv_model)



##working on linear regression
##output with fixed lambda (which is best lambda)
flambda_out <- fixedLassoInf(x = x ,y = y, beta = best_beta, lambda = best_lambda)

###this gives you a stepwise output 
fsfit <- fs(x, y, maxsteps = 500)
plot(fsfit)
fsoutput <- fsInf(fsfit, alpha = 0.05, k=50)
print(fsoutput)


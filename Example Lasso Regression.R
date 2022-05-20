###laso regression.
#make sure you install libraries first in your console
#libraries
library(glmnet)
library(tidyr)

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
                            "EMcluster06", "EMcluster04", "EMcluster01",
                            "EMcluster03", "EMcluster02", "Age2GroupINT")])


#k-fol cross-validation to find lamba value
cv_model <- cv.glmnet(x, y, alpha=1)

#find best lambda that minimizes test mean squared error
best_lambda <- cv_model$lambda.min
best_lambda


###ANALYZE###

#find coefficient of best model
best_model <-glmnet(x, y, alpha =1, lambda = best_lambda)
coef(best_model)



#find R-squared-- this is the percentage that the best model is able
#to explain the variation in response values based on the training data
#
rsq <- cv_model$glmnet.fit$dev.ratio
rsq_mean <- mean(rsq)
plot(cv_model$lambda,rsq)

###Visulize
#looking at percent deviance from each coefficient 
plot(glmnet(x,y), xvar='dev', label = T)

#plotting all rsq values
plot(cv_model$lambda,rsq)

# produce plot of test mean squared error by lambda value
plot(cv_model)




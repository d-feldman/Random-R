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
df1 <- df %>% drop_na(Factor1_EM)

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

# produce plot of test mean squared error by lambda value
plot(cv_model)

###ANALYZE###

#find coeficient of best model
best_model <-glmnet(x, y, alpha =1, lambda = best_lambda)
coef(best_model)

###Exaample of predicted test###
# ###using fitted laso regression model to predict value for hp in new observation
# # mpg = 24, wt = 2.5, drat = 3.5, qsec = 18.5
# #new observation using values above-- needs to be a matrix
# new = matrix(c(24, 2.5, 3.5, 18.5), nrow=1, ncol =4)
# 
# #use laso regression to predict response value
# print(predict(best_model, s= best_lambda, newx = new))



###calculate the R-squared of the model on training data
#use fitted best model to make predictions
y_predicted <- predict(best_model, s= best_lambda, newx=x)

#find SST and SSE
#squared difference between dep. variable and its mean
sst <- sum(((y-mean(y))^2))
#sum of squares due to error
sse <- sum((y_predicted -y)^2, na.rm=T)


#find R-squared-- this is the percentage that the best model is able
#to explain the variation in response values based on the training data
rsq <- 1 - (sse/sst)
rsq
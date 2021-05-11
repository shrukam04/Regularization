#Name: Shruti B. Kamble
#Instructor: Prof. Roy Wada
#Course: Intermediate Analytics
#Title: Regularization
#Date: 05/09/2021


installAllPackages <- function(Pkgs){
  new.Pkgs <- Pkgs[!(Pkgs %in% installed.packages()[, "Package"])]
  if (length(new.Pkgs)) 
    install.packages(new.Pkgs, dependencies = TRUE)
  null<- sapply(Pkgs, require, character.only = TRUE)
}
packages <- c(
  "dplyr","psych","rlang","plyr", "reshape2","UsingR","tidyr","tidyverse","readxl","ggplot2","ggpubr",
  "rmarkdown","formatR","plotly","gmodels","knitr","kableExtra","base64","base64enc", "enc","EDA", 
  "hrbrthemes","ggcorrplot","corrplot","Publish","magrittr", "PerformanceAnalytics", "forecast","xfun",
  "htmltools", "funModeling","corrgram","dummies","patchwork","lsmeans","imputeTS","leaps","lmSubsets",
  "sjPlot","gtsummary","ROCR","InformationValue","caret","ISLR","broom","glmnet","caret","glmnet")
installAllPackages(packages)
library(caret)
library(glmnet)
library(pROC)
headTail(College, top = 7, bottom = 7)     #verifying dataset if loaded

#1. Split the data into a train and test set – refer to the Feature_Selection_R.pdf document for information on how to split a dataset.
data("College")
dim(College)
x <- set.seed(123)
train_size <- floor(0.70 * nrow(College))
train_data <- sample(seq_len(nrow(College)), size = train_size)
train_set <- College[train_data, ]
test_set <- College[-train_data, ]
nrow(train_set) + nrow(test_set)       #verifying split of data

#Removing private column as charater datatype and to prodouce result
train <- train[,-1]
test <- test[,-1]

College_a <- model.matrix(Enroll~.,College)[,-1]  #Conversion of dataset for glmnet function
College_b <- College$Enroll

College_a <- College_a[, -1]


#Ridge Regression
#2.Use the cv.glmnet function to estimate the lambda.min and lambda.1se values. Compare and discuss the values.

#Ridge Regression on College dataset
lambda_grid <- 10^seq(2, -2, by = -.1)

set.seed (123)
ridge_model <- glmnet(College_a, College_b, alpha = 0, lambda = lambda_grid)
ridge_model              #Output of glmnet
college_out = cv.glmnet(College_a, College_b, alpha = 0,lambda = lambda_grid )
college_out             #Output of cv.glmnet
         
cat("lambda.min value = " , college_out$lambda.min, 
    " and lambda.1se value = ", college_out$lambda.1se)

#3.Plot the results from the cv.glmnet function provide an interpretation. What does this plot tell us?
plot(college_out)

#4.Fit a Ridge regression model against the training set and report on the coefficients.Is there anything interesting?
train_a <- model.matrix(Enroll~.,train_set)[,-1]
train_b <- train_set$Enroll
test_a <- model.matrix(Enroll~.,test_set)[,-1]
test_b <- test_set$Enroll

best_lambdaR =college_out$lambda
best_lambdaR

#training model using ridge regression
Train_Rmod <- glmnet(train_a,train_b,alpha=0,lambda=best_lambdaR)
Train_cv_out <- cv.glmnet(train_a,train_b,alpha=0,lambda=best_lambdaR)
Train_cv_out

#Printing out the coefficients and model
plot(Train_Rmod)
plot(Train_cv_out)
coef(Train_Rmod)
coef(Train_cv_out)

# coefplot for the 1se and min error lambda 
library(coefplot)
coefplot::coefplot(Train_cv_out, lambda = "lambda.min", sort = "magnitude", intercept = FALSE)
coefplot::coefplot(Train_cv_out, lamda = "lambda.1se", sort = "magnitude", intercept = FALSE)
view(College)

#5.Determine the performance of the fit model against the training set by calculating
#root mean square error (RMSE).sqrt(mean((actual - predicted)^2))

#Fitting Ridge model on training set
predicted <- predict(ridge_model,newx = train_a, s= best_lambdaR)
Rtrain_Accuracy <- sqrt(Mean(train_b - predicted)^2)   #Calculating Accuracy
Rtrain_Accuracy

#6.Determine the performance of the fit model against the test set by calculating the root mean square error (RMSE). Is your model overfit?
predicted_test <- predict(ridge_model, s= best_lambdaR, newx = test_a)
Rtest_Accuracy <- sqrt(mean(test_b - predicted)^2)   #Calculating Accuracy
Rtest_Accuracy

#LASSO
#7.Use the cv.glmnet function to estimate the lambda.min and lambda.1se values. Compare and discuss the values.
set.seed (123)
lasso_model = glmnet(College_a, College_b, alpha = 1,lambda = lambda_grid )
Lassocollege_cv_out = cv.glmnet(xCollege, yCollege, alpha = 1,lambda = lambda_grid )
cat("lambda.min value = " , Lassocollege_cv_out$lambda.min, 
    "and lambda.1se value is = ", Lassocollege_cv_out$lambda.1se)
plot(Lassocollege_cv_out)


#8.Plot the results from the cv.glmnet function provide an interpretation. What does this plot tell us?
plot(lasso_model)

#9.Fit a LASSO regression model against the training set and report on the coefficients. 
#Do any coefficients reduce to zero? If so, which ones?
best_lambdaL <- Lassocollege_cv_out$lambda
best_lambdaL

#Creating training model using LASSO regression
Train_Lmodel =glmnet(train_a,train_b,alpha=1,lambda=best_lambdaL)
Train_Lcvout =cv.glmnet(train_a,train_b,alpha=1,lambda=best_lambdaL)
Train_Lcvout

#Printing out the coefficients and model
plot(Train_Lmodel)
plot(Train_Lcvout)
coef(Train_Lmodel)

#coefplot for the 1se and min error lambda 
coefplot::coefplot(Train_Lcvout, lambda='lambda.min', sort='magnitude', intercept=F)
coefplot::coefplot(Train_Lcvout, lambda='lambda.1se', sort='magnitude', intercept=F)

#10. Determine the performance of the fit model against the training set by calculating the root mean square error (RMSE). sqrt(mean((actual - predicted)^2))
predicted_Lasso <- predict(lasso_model,s=best_lambdaL ,newx=train_a)
predicted_Lasso
Ltrain_Accuracy <- sqrt(mean((train_b - predicted_Lasso)^2))  #Calculating Accuracy
Ltrain_Accuracy

#11.Determine the performance of the fit model against the test set by calculating the root mean square error (RMSE). Is your model overfit?
predicted_Lasso_01 <- predict(lasso_model,s=best_lambdaL ,newx=test_a)
Ltest_Accuracy <- sqrt(mean(test_b - predicted_Lasso_01)^2)          #Calculating Accuracy
Ltest_Accuracy

#12.Which model performed better and why? Is that what you expected?
#Explanation is in the report

#13. Refer to the Intermediate_Analytics_Feature_Selection_R.pdf document for how to perform stepwise selection and then fit a model. Did this model perform better or as well as Ridge regression or LASSO? Which method do you prefer and why?
set.seed(12345)
lm_model  <- step(lm(Enroll~. , data =  College), 
                  direction = 'both', 
                  steps = 17 , 
                  trace = FALSE)
summary(lm_model)

lm_Train_preds <- predict.lm(lm_model, newdata = train_set)

library(mltools)
lm_Train_rmse <- 
  mltools::rmse(preds= preds_lm_Train, 
                               actuals = train$Enroll, 
                               weights = 1, 
                               na.rm   = FALSE)

lm_Test_preds <- predict.lm(lm_model, newdata = test_set)

lm_Test_rmse <- mltools::rmse(preds   = preds_lm_Test, 
                              actuals = test$Enroll, 
                              weights = 1, 
                              na.rm   = FALSE)
cat("The value of Root mean squar error for Linear model Train data set = ", lm_Train_rmse, " and for Test = ", lm_Test_rmse)

coef(lm_model)
coefplot::coefplot(lm_model, sort='magnitude',intercept=F)

print("Ridge Model") #5
Rtest_Accuracy
print("LASSO Model") #5
Ltrain_Accuracy
Ltrain_MSE
print("LM Model") #3
lm_Train_rmse
lm_Test_rmse


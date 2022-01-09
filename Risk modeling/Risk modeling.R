################################################################################ 
# Exercises

# Exercise 1.1
# Import quotations for any asset of your choice using either provided files or 
# quantmod package from first labs. Then calculate logarithmic returns on that 
# asset and compare Value-at-Risk estimates for 2021 from historical simulation,
# GARCH models and filtered historical simulation (models should be built on the 
# data prior to 01-01-2021 with moving window method). Does the quality of the 
# models change? Are some approaches much better than the others? What caused 
# such behavior? Plot the VaR estimates together with data.

library(xts)
library(ggplot2)
library(utils)
library(rugarch)
library(GAS)
library(ROCR)
library(randomForest)
library(xgboost)

Sys.setlocale("LC_TIME", "English")
setwd("...")

# Import quotations for any asset of your choice
index_data <- read.csv("lse.csv", stringsAsFactors = F)
names(index_data) <- c("Date","Open","High","Low","Close","Volume")

index_data.xts <- xts(index_data[,-1],
                      as.Date(index_data[, 1], "%Y-%m-%d"))
p_value <- .025

# Since we want to forecast for 2021... (until 2021-12-15 is provided)
forecast_horizon <- 240 

# Then calculate logarithmic returns on that asset...
log_returns <- diff(log(index_data.xts$Close), lag = 1)

# For the whole dataset we use 2018 till 2021-12-15
data <- log_returns["2018/2021"]

# Number of training samples
training_sample_len <- nrow(data) - forecast_horizon

# Marking the first obs. on the testing sample 
plot(data)
addEventLines(
  events = xts("Testing Sample", index(data[nrow(data) - forecast_horizon + 1])),
  col = "blue",
  srt = 270,
  pos = 4
)

# Historical simulation
rollHS <- rollapplyr(data, training_sample_len,
                     function(w) {
                       quantile(w, p_value)
                     })

# Lagging the output vector
testHS <- last(lag(rollHS, 1), forecast_horizon)

# GARCH(1,1) with normal distribution
specnorm <-
  ugarchspec(
    variance.model = list(model="sGARCH", garchOrder=c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = 'norm'
  )

# Rolling window approach estimation
rollnorm <- ugarchroll(
  specnorm,
  data,
  n.ahead = 1,
  forecast.length = forecast_horizon,
  refit.every = 1,
  refit.window = 'moving',
  keep.coef = TRUE,
  calculate.VaR = TRUE,
  VaR.alpha = p_value
)

# VaR estimation for testing period
testGarchNORM <- xts(rollnorm@forecast$VaR,
                     as.Date(rownames(rollnorm@forecast$VaR)))[, 1]

# Plotting the conditional variance forecast
plot(cbind(last(data, forecast_horizon), rollnorm@forecast$density$Sigma))


# GARCH(1,1) with skewed normal distribution
# Model specification
specSnorm <-
  ugarchspec(
    variance.model = list(model="sGARCH", garchOrder=c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = 'snorm'
  )

# Rolling window approach estimation
rollSnorm <-
  ugarchroll(
    specSnorm,
    data,
    n.ahead = 1,
    forecast.length = forecast_horizon,
    refit.every = 1,
    refit.window = 'moving',
    keep.coef = TRUE,
    calculate.VaR = TRUE,
    VaR.alpha = p_value
  )

#VaR estimation for testing period
testGarchSNORM <- xts(rollSnorm@forecast$VaR,
                      as.Date(rownames(rollSnorm@forecast$VaR)))[, 1]

# And plot it again to verify the model
plot(cbind(last(data, forecast_horizon), rollSnorm@forecast$density$Sigma))

# GARCH(1,1) with skewed t distribution
# Model specification
specSt <-
  ugarchspec(
    variance.model = list(model="sGARCH", garchOrder=c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = 'sstd'
  )

# Rolling window approach estimation
rollst <-
  ugarchroll(
    specSt,
    data,
    n.ahead = 1,
    forecast.length = forecast_horizon,
    refit.every = 1,
    refit.window = 'moving',
    keep.coef = TRUE,
    calculate.VaR = TRUE,
    VaR.alpha = p_value
  )

#VaR estimation for testing period
testGarchST <- xts(rollst@forecast$VaR,
                   as.Date(rownames(rollnorm@forecast$VaR)))[, 1]

# Is everything ok with the model? Let's plot it out
plot(cbind(last(data, forecast_horizon), rollst@forecast$density$Sigma))

# Filtered Historical Simulation (FHS)
rollFHS <- rollapplyr(data, training_sample_len,
                      function(data) {
                        # Fitting a normal GARCH(1, 1)
                        fit <- ugarchfit(specnorm, data)
                        # Residual standardization
                        res <- rugarch::residuals(fit, standardize = T)
                        # Calculating forecasts of conditional variance for the next timestep
                        hat <-
                          sqrt(
                            fit@fit$coef['omega'] + fit@fit$coef['alpha1'] * tail(rugarch::residuals(fit), 1) ^
                              2 + fit@fit$coef['beta1'] * tail(sigma(fit), 1) ^ 2
                          )
                        # Drawing 20000 standard residulas from the window
                        draw <- sample(res, 20000, replace = T)
                        # Picking the standards residulas which is a p-value quantile form the window we are interested in  
                        draw_var <- quantile(draw, p_value)
                        # Standard var equation
                        var <- draw_var * as.numeric(hat)
                        return(var)
                      })

# Shift the vectors to obtain forecast for the next timestep
testFHS <- last(lag(rollFHS, 1), forecast_horizon)

# Vector of realization of rate of return in test data
testRealised <- last(data, 240)

# Binding forecasts of VaR
var_predictions <- cbind(testHS,
                         testFHS,
                         testGarchNORM,
                         testGarchSNORM,
                         testGarchST)
colnames(var_predictions) <-
  c("HS", "FHS", "GARCH Norm", "GARCH SNORM", "GARCH ST")

# function for calculating basic number of exceptions
excess_count <- function(var, true) {
  # if VaR > true realizaion (lower in absolute terms) then we have an exception
  return(sum(ifelse(coredata(var) > coredata(true), 1, 0)))
}

# we apply this function to each column in out VaR estimates object
sapply(var_predictions, excess_count, true = testRealised)
.025*240 # Aceptable amount of exceptions
# Based on estimation of the models we can see that in this case GARCH ST 

# Runing Kupiec and Christoffersen tests for all models
sapply(var_predictions, function(var) {
  c(
    "Kupiec"=VaRTest(p_value, testRealised, var)$uc.Decision,
    "Christoffersen"=VaRTest(p_value, testRealised, var)$cc.Decision
  )
})

# We can do the same thing with BacktestVaR() from GAS package
# $LRuc: The unconditional coverage test of Kupiec
# $LRcc: The conditional coverage test of Christoffesen
# $DQ$pvalue: p-value for the Dynamic Quantile test of Engle and Manganelli
sapply(var_predictions, function(var) {
  c(
    "Kupiec"=BacktestVaR(testRealised, var, p_value)$LRuc[2],
    "Christoffersen"=BacktestVaR(testRealised, var, p_value)$LRcc[2],
    "DQ"=BacktestVaR(testRealised, var, p_value)$DQ$pvalue
  )
})

# plot with VaRs
ggplot(testRealised, aes(index(testRealised), indeks)) +
  geom_line(aes(y = testGarchNORM, colour = "Garch NORM"), size = 1) +
  geom_line(aes(y = testGarchSNORM, colour = "Garch SNORM"), size = 1) +
  geom_line(aes(y = testGarchST, colour = "Garch ST"), size = 1) +
  geom_line(aes(y = testHS, colour = "HS"), size = 1) +
  geom_line(aes(y = testFHS, colour = "FHS"), size = 1) +
  geom_point(aes(y = testRealised), size = 1) +
  scale_x_date(date_minor_breaks = "1 day") +  scale_colour_manual(
    "",
    breaks = c("Garch NORM", "Garch SNORM", "Garch ST", "HS", "FHS"),
    values = c("red", "green", "blue", "yellow", "brown")
  ) +
  xlab("") + ylab("Returns and VaRs")

# Interpretation:
# The historical simulation based on the p-value of Kupiec and Christoffersen
# rejects the null in which for example the null for Kupiec is "the probability 
# of an exception (p) is equal to the significance level (p???) of the VaR and 
# Probability of Failure (PF) has a chi-square distribution with one degree of 
# freedom" and therefore is not a valid model, even though the exceptions is only 1 
# compare to the acceptable exceptions of 6. 
# Moreover even if it was a valid model, the threshold for reserving is very high
# in other words, the model shows a  harsh and conservative behavior as we can spend
# the more reserved money in the calm periods than we hold in the portfolio (e.g. October and November)
# So the best model for this task would be Filtered HS as the exceptions is completely 
# equals to the expected value of exceptions. The next best one is GARCH ST.

####################################################################
# Exercise 1.2
# Load the data for the credit risk applications. Run necessary preprocessing 
# steps. Train logistic regression, random forest and xgboost models. Compare 
# the models performance for AUC. Are the ML models better or worse than logistic 
# regression? Choose a probability of default threshold level for one of the 
# models and justify your selection.
rm(list = ls())
library(dplyr)

# Load the data for the credit risk applications...
data <- read.csv("default_data.csv", stringsAsFactors = T)
table(data$default) # More or less balanced, no need to use ROSE (Random Over-Sampling Ex- amples)

# let's see what's in the dataset
str(data)

# Let's assign NA to all instances of "?" within the data
data <- data %>%
  mutate_at(vars(names(data)), na_if, "?")

# Converting variables to necessary types
data$A2 <- as.double(as.character(data$A2))
data$A14 <- as.double(as.character(data$A14))

# Removing the rows with missing data
data <- data[complete.cases(data),]

# Train-test split
set.seed(10)
train <- data[sort(sample(nrow(data), nrow(data) * .6)), ]
test <- data[-sort(sample(nrow(data), nrow(data) * .6)), ]

# Logistic regression
set.seed(101)
model_lr <- glm(default ~ ., 
                data = train, 
                family = binomial("logit"))

# That's not a good sign. It might be the case of Quasi-complete separation.
# Let's explore to understand which variable(s)/observation(s) is(are) responsible.
# At this level first we want to make sure if we have any separation or not
# 'detectseparation' is a handy package which provides pre-fit and post-fit methods
# for detecting separation and infinite maximum likelihood (ML) estimates in generalized 
# linear models with categorical responses. 
library(detectseparation)

# And we run the model again
set.seed(92)
model_lr <- glm(default ~ ., 
      data = train, 
      family = binomial('logit'),
      method = detectseparation::detectSeparation) # The diagnostic for separation 
model_lr$separation # True
model_lr

# looks like the A7 and A4 variables are mostly responsible for the infinite ML estimates
# So let's build the LR model without A7 and A4 to see whether it changes the result or not
set.seed(92)
# No warning this time
model_lr <- glm(default ~ ., 
                data = train[,-c(7,4)], # Excluding the problematic variables
                family = binomial("logit"))
summary(model_lr)

# Predicting the defaulting entities
predict <- predict(model_lr, type = 'response', newdata = train)

# Confusion matrix for training set
library(caret)
confusionMatrix(data = as.factor(if_else(predict > .5,1,0)), 
                reference = as.factor(train$default), positive = '1')

# ROC Curve
library(ROCR)
ROCpred <- prediction(predict, train$default)
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
plot(ROCperf, colorize = TRUE,
     main="ROC curve for training set",
     xlab="FPR",
     ylab="TPR",
     sub="Logistic regression")

# AUC value for the training set
auc <- performance(ROCpred, measure = "auc")@y.values[[1]]
paste0("AUC for training is ",round(auc,2))

# On the testing sample
predict_LR <- predict(model_lr, type = 'response', newdata = test)

# Confusion matrix for testing sample
confusionMatrix(data = as.factor(if_else(predict_LR > .5,1,0)), 
                reference = as.factor(test$default),positive = '1')

# ROC Curve for testing sample
ROCpred_t <- prediction(predict_LR, test$default)
ROCperf_t <- performance(ROCpred_t, 'tpr', 'fpr')
plot(ROCperf_t, colorize = TRUE,
     main="ROC curve for testing set",
     xlab="FPR",
     ylab="TPR",
     sub="Logistic regression")

# AUC value for the testing set
auc_t <- performance(ROCpred_t, measure = "auc")@y.values[[1]]
paste0("AUC for training is ",round(auc_t,2))

all_models_aucs <- c("LR"=auc_t)


# Random forest
library(randomForest)
set.seed(10)
modelrf <- randomForest(default ~ ., 
                        data = train, 
                        importance = TRUE,
                        ntree = 100)

# Comparing RF's statistics with previous model
predict_RF <- predict(modelrf, type = 'response', newdata = test)
confusionMatrix(data = as.factor(if_else(predict_RF > .5,1,0)), 
                reference = as.factor(test$default),positive = '1')

ROCpred_t <- prediction(predict_RF, test$default)
ROCperf_t <- performance(ROCpred_t, 'tpr', 'fpr')
plot(ROCperf_t,colorize = TRUE,
     main="ROC curve for testing set",
     xlab="FPR",
     ylab="TPR",
     sub="Random Forest")

# AUC value
auc_t <- performance(ROCpred_t, measure = "auc")@y.values[[1]]
paste0("AUC for training is ",round(auc_t,2))

all_models_aucs <- c(all_models_aucs, "RF"=auc_t)

# XGBoost
# Checking the type of columns again
str(train)

# Converting full dataset to numeric plus we don't have character variables
xgb_train <-
  matrix(unlist(lapply(train[,-which(colnames(train) == "default")], as.numeric)), ncol = ncol(train) - 1)

# And for the testing sample as well
xgb_test <-
  matrix(unlist(lapply(test[,-which(colnames(test) == "default")], as.numeric)), ncol = ncol(test) - 1)

library(xgboost)
set.seed(92)
modelxgb <- xgboost(
  data = xgb_train,
  label = train$default,
  nrounds = 100,
  objective = "binary:logistic"
)

# Prediction on testing set
predict_XGB <- predict(modelxgb, type = 'response', newdata = xgb_test)
# Confusion matrix
confusionMatrix(data = as.factor(if_else(predict_XGB > .5,1,0)), 
                reference = as.factor(test$default),positive = '1')

ROCpred_t <- prediction(predict_XGB, test$default)
ROCperf_t <- performance(ROCpred_t, 'tpr', 'fpr')
plot(ROCperf_t,colorize = TRUE,
     main="ROC curve for testing set",
     xlab="FPR",
     ylab="TPR",
     sub="XGBoost")

# AUC value
auc_t <- performance(ROCpred_t, measure = "auc")@y.values[[1]]
paste0("AUC for testing is ",round(auc_t,2))

all_models_aucs <- c(all_models_aucs, "XGB"=auc_t)
all_models_aucs

# Interpretation: Even without tuning any XGBoost hyper parameters, we can see that based
# on AUC metric the XGBoost outperformed LR and RF. However it's not the cases for all
# datasets and situations since if for example number of observations in training
# data is significantly smaller than the number of features, XGBoost is 
# not recommended. 

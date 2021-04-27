# Huy Pham
# UC Berkeley
# Created: April 2021
# Description: OLS and LASSO study for the Ames Housing data

library(tidyverse)
library(dplyr)


## ----various settings---------------------------------------------------------
rm(list=ls())

point.col = rgb(red=0.3, green=0.3, blue=0.3, alpha=0.4)
## ----read in data-------------------------------------------------------------
dataPath <- '../../data/AmesHousing.csv'
ames.full <- read.csv(dataPath, header=TRUE)[,-1:-2]
ames.numeric <- Filter(is.numeric, ames.full) %>% na.omit()

MS.SubClass <- factor(ames.full$MS.SubClass)
Overall.Qual <- as.numeric(ames.full$Overall.Qual)

## ----exploratory analysis-----------------------------------------------------
summary(ames.full)

## ----Ridge--------------------------------------------------------------------
library(glmnet)


x <- select(ames.numeric, -SalePrice) %>% data.matrix()
y <- ames.numeric$SalePrice

lambdas <- 10^seq(10, -2, length = 100)
ridge_mod <- glmnet(x, y, alpha = 0, lambda = lambdas)
dim(coef(ridge_mod))
plot(ridge_mod)    # Draw plot of coefficients

set.seed(1)

ames.train <- ames.numeric %>% sample_frac(0.5)
ames.test <- ames.numeric %>% setdiff(ames.train)

x.train <- select(ames.train, -SalePrice) %>% data.matrix()
y.train <- ames.train$SalePrice

x.test <- select(ames.test, -SalePrice) %>% data.matrix()
y.test <- ames.test$SalePrice

cv.out <- cv.glmnet(x.train, y.train, alpha = 0) # Fit ridge regression model on training data
bestlam <- cv.out$lambda.min  # Select lambda that minimizes training MSE
plot(cv.out) # Draw plot of training MSE as a function of lambda

ridge.pred <- predict(ridge_mod, s = bestlam, newx = x.test) # Use best lambda to predict test data
mean((ridge.pred - y.test)^2) # Calculate test MSE

out <- glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV

## ----LASSO--------------------------------------------------------------------

lasso.mod <- glmnet(x.train, 
                   y.train, 
                   alpha = 1, 
                   lambda = lambdas) # Fit lasso model on training data

plot(lasso.mod)    # Draw plot of coefficients

cv.out <- cv.glmnet(x.train, y.train, alpha = 1) # Fit lasso model on training data
plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam <- cv.out$lambda.min # Select lamda that minimizes training MSE
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x.test) # Use best lambda to predict test data
mean((lasso.pred - y.test)^2) # Calculate test MSE

out <- glmnet(x, y, alpha = 1, lambda = lambdas) # Fit lasso model on full dataset
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
lasso.coef

## ----Hyp Test-----------------------------------------------------------------
housing.fit.full <- lm(SalePrice ~ ., data = ames.numeric)
housing.fit.cond <- lm(SalePrice ~ Overall.Qual, data = ames.numeric)
housing.fit.1 <- lm(SalePrice ~ 1, data = ames.numeric)

anova(housing.fit.1, housing.fit.cond, housing.fit.full)
anova(housing.fit.cond, housing.fit.full)

library(MASS)
library("Matching")
library(car)


##----EHW-----------------------------------------------------------------------
housing.full.hc0 = sqrt(diag(hccm(housing.fit.full, type="hc0")))
housing.full.hc1 = sqrt(diag(hccm(housing.fit.full, type="hc1")))
housing.full.hc2 = sqrt(diag(hccm(housing.fit.full, type="hc2")))
housing.full.hc3 = sqrt(diag(hccm(housing.fit.full, type="hc3")))
housing.full.hc4 = sqrt(diag(hccm(housing.fit.full, type="hc4")))

# check code11.3.2 second example


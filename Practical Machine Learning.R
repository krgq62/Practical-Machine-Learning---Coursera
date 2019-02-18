

## Practical Machine Learning

setwd("E:/Documents/Coursera/Data Science Certificate/Practical Machine Learning")

library(datasets)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(scales)
library(caret)
library(caretEnsemble)
library(quantmod)
library(Quandl)
library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)
library(rattle)

set.seed(77)

## Load Data into R
tst <- read.csv(file = "pml-testing.csv")
trn <- read.csv(file = "pml-training.csv")

## What variables exist?
names(tst)
names(trn)
str(tst)
str(trn)

## Cleaning the Data
# Remove null values
trn_comp <- complete.cases(t(trn))
tst_comp <- complete.cases(t(tst))
trn_fin <- trn[, trn_comp]
tst_fin <- tst[, tst_comp]

# Remove irrelevant columns
trn_fin <- trn_fin[,-c(1,2,3,4,5,6,7)]
tst_fin <- tst_fin[,-c(1,2,3,4,5,6,7)]

threshold <- nrow(trn_fin)*0.95
na_columns <- !apply(trn_fin, 2, function(x) sum(is.na(x)) > threshold || sum(x=="") > threshold)
trn_fin <- trn_fin[, na_columns]
na_columns <- !apply(tst_fin, 2, function(x) sum(is.na(x)) > threshold || sum(x=="") > threshold)
tst_fin <- tst_fin[, na_columns]



## Exploratory Analysis

summary(trn_fin)
summary(tst_fin)

# Partition traning data set for cross validation
training <- createDataPartition(y = trn_fin$classe, p = 0.70, list = FALSE)
trn_fin1 <- trn_fin[training, ]
trn_fin2 <- trn_fin[-training, ]

dim(trn_fin1)
dim(trn_fin2)

## Model Development

rf_model <- randomForest(classe ~., data = trn_fin1, method = "class")
rf_model

## Prediction Performance
rf_predict <- predict(rf_model, trn_fin2)

confusionMatrix(trn_fin2$classe, rf_predict)

#### Apply the random forest prediction model to the test set
rf_test <- predict(rf_model, tst_fin)

rf_test

## Plots
### Representative Random Forest Plot
rf_model_rep <- rpart(classe ~., data = trn_fin1, method = "class")
fancyRpartPlot(rf_model_rep)





#0. Clear Environment & Load Packages
rm(list = ls())

library(gtrendsR)
library(Stack)
library(stats)
library(dplyr)
library(readr)
library(stringr)
library(DataCombine)
library(tidyverse)
library(caret)
library(glmnet)
library(xgboost)

setwd("C:/Users/aksha_wdr31m6/OneDrive/Documents/School Docs/Sloan 2019/Spring/15.071 Analytics Edge/Group Project/COVID19")

final_data <- read.csv("Final_Merge_COVID.csv")

final_data$w_grocery_and_pharmacy_percent_change_from_baseline[is.na(final_data$w_grocery_and_pharmacy_percent_change_from_baseline)] <- 0
final_data$w_retail_and_recreation_percent_change_from_baseline[is.na(final_data$w_retail_and_recreation_percent_change_from_baseline)] <- 0
final_data$w_workplaces_percent_change_from_baseline[is.na(final_data$w_workplaces_percent_change_from_baseline)] <- 0

final_data <- final_data[!is.na(final_data$TotalPopUnder5_sum),]

model_data <- select(final_data, -c(X, location, geo_code, Inflation.Factor, DMA,
                                    TotalCivPopAmbulatoryDisability_sum, TotalCivPopSelfCareDisability_sum, TotalCivPopIndLivingDisability_sum))
lag = 1
lead = 14

#for (lead in 1:14)
#{
#  for (lag in 1:14)
#  {
    covid <-    model_data %>% drop_na(paste("CaseLagDay.",lag,sep=""))
    covid <-    covid %>% drop_na(paste("CaseLeadDay",lead,sep=""))

    for (i in 1:14)
    {
      covid <- select(covid, -c(paste("DeathLeadDay",i,sep="")))

      if (i != lead){
        covid <- select(covid, -c(paste("CaseLeadDay",i, sep="")))
      }

      if (i > lag){
        covid <- select(covid, -c(paste("CaseLagDay.",i, sep=""),paste("DeathLagDay.",i,sep="")))
      }

    }

    set.seed(144)
    split = createDataPartition(covid$CaseLeadDay1, p = 0.75, list = FALSE)
    covid.train <- covid[split,]
    covid.test <- covid[-split,]

    #Train Linear Model

    model <- lm(CaseLeadDay14 ~ . - date,
                data = covid.train)

    summary(model)

    pred.covid <- predict(model, newdata = covid.test)

    SSE.improved = sum((pred.covid - covid.test$CaseLeadDay14)^2)
    SST.original = sum((mean(covid.train$CaseLeadDay14) - covid.test$CaseLeadDay14)^2)
    OSR2.improved = 1 - (SSE.improved/SST.original)
    OSR2.improved

    #Train LASSO

    #1. Ridge Regression & LASSO Train

    set.seed(15071)

    #1.1. Preparation of the train and test matrices

    x.train=model.matrix(CaseLeadDay14~. - date, data=covid.train)
    y.train= covid.train$CaseLeadDay14
    x.test=model.matrix(CaseLeadDay14~. - date, data=covid.test)
    y.test=covid.test$CaseLeadDay14

    lambdas.lasso <- exp(seq(15, -10, -.01))

    #1.2 Cross Validate

    ridge.tr <- cv.glmnet(x.train, y.train, lambda = lambdas.lasso, nfolds = 10, alpha=0)
    lasso.tr <- cv.glmnet(x.train, y.train, lambda = lambdas.lasso, nfolds = 10, alpha=1)

    plot(ridge.tr)
    plot(lasso.tr)

    ridge.tr$lambda.min
    lasso.tr$lambda.min

    #2. Develop Model
    ridge.fin <- glmnet(x.train, y.train, alpha = 0, lambda = ridge.tr$lambda.min)
    lasso.fin <- glmnet(x.train, y.train, alpha = 1, lambda = lasso.tr$lambda.min)

    #2. Show Coefficients
    coef(ridge.fin)
    coef(lasso.fin)

    #3. R^2 & OSR^2

    #3.1 Prediction
    pred.ridge.train <- predict(ridge.fin, newx = x.train)
    pred.lasso.train <- predict(lasso.fin, newx = x.train)

    pred.ridge.test <- predict(ridge.fin, newx = x.test)
    pred.lasso.test <- predict(lasso.fin, newx = x.test)

    #3.2 Residuals
    ridge.train.resid <- covid.train$CaseLeadDay14 - pred.ridge.train
    lasso.train.resid <- covid.train$CaseLeadDay14 - pred.lasso.train

    ridge.test.resid <- covid.test$CaseLeadDay14 - pred.ridge.test
    lasso.test.resid <- covid.test$CaseLeadDay14 - pred.lasso.test

    #3.3 R^2 & OSR^2

    R2.ridge <- 1 - sum(ridge.train.resid^2) / sum((covid.train$CaseLeadDay14 - mean(covid.train$CaseLeadDay14))^2)
    R2.lasso <- 1 - sum(lasso.train.resid^2) / sum((covid.train$CaseLeadDay14 - mean(covid.train$CaseLeadDay14))^2)

    OSR2.ridge <- 1 - sum(ridge.test.resid^2) / sum((covid.test$CaseLeadDay14 - mean(covid.train$CaseLeadDay14))^2)
    OSR2.lasso <- 1 - sum(lasso.test.resid^2) / sum((covid.test$CaseLeadDay14 - mean(covid.train$CaseLeadDay14))^2)

    R2.ridge
    OSR2.ridge

    R2.lasso
    OSR2.lasso

    #xGBoost

    xgb <- train(y = y.train,
                 x = x.train,
                 method = "xgbTree",
                 trControl = trainControl(method="cv", number=5))

    xgb$bestTune


    # R^2 and OSR^2
    xgb.final <- xgb$finalModel

    preds.xgb.train <- predict(xgb.final, newdata = x.train)
    preds.xgb.test <- predict(xgb.final, newdata = x.test)


    r2.xgb <- 1 - sum((preds.xgb.train - y.train)^2)/sum((mean(y.train) - y.train)^2)
    osr.xgb <- 1 - sum((preds.xgb.test - y.test)^2)/sum((mean(y.train) - y.test)^2)

    r2.xgb
    osr.xgb

    #Feature Importance
    mat <- xgb.importance(colnames(covid.train), model =xgb.final)
    xgb.plot.importance(mat)

    #Just the top variables
    xgb.plot.importance(mat, top_n = 16, left_margin = 16, rel_to_first = T)

    nodes <- xgb.dump(xgb.final, with_stats = T)
    nodes[1:10]

    covid.test.review <- select(covid.test, c("CaseLeadDay14", "date", "hits",
                                              "hits.adj", "New_Cases", "New_Deaths",
                                              "Total_Cases", "Total_Deaths"))

    covid.test.review$Predict <- preds.xgb.test

    covid.test.review$Predict[covid.test.review$Predict < 0] <- 0

#  }
#}

#4. Interpretation of Results

#4.1 Track Prediction Accuracy
covid.test.review %>%


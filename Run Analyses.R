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
library(scales)

setwd("C:/Users/aksha_wdr31m6/OneDrive/Documents/School Docs/Sloan 2019/Spring/15.071 Analytics Edge/Group Project/COVID19")

final_data <- read.csv("Final_Merge_COVID.csv")

final_data$w_grocery_and_pharmacy_percent_change_from_baseline[is.na(final_data$w_grocery_and_pharmacy_percent_change_from_baseline)] <- 0
final_data$w_retail_and_recreation_percent_change_from_baseline[is.na(final_data$w_retail_and_recreation_percent_change_from_baseline)] <- 0
final_data$w_workplaces_percent_change_from_baseline[is.na(final_data$w_workplaces_percent_change_from_baseline)] <- 0

final_data <- final_data[!is.na(final_data$TotalPopUnder5_sum),]

model_data <- select(final_data, -c(X, geo_code, Inflation.Factor, DMA,
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

    #Save and remove geographies to add back on in the end
    covid.train.geos <- covid.train$location
    covid.test.geos <- covid.test$location

    covid.train <- select(covid.train, -c(location))
    covid.test <- select(covid.test, -c(location))

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

#Add back in the locations
covid.test.review$location <- covid.test.geos
covid.test.review$date <- as.Date(covid.test.review$date)
covid.test.review$dateLead14 <- covid.test.review$date + 14

covid.test.review %>%
  group_by(location) %>%
  mutate(n = n()) %>%
  filter(n > 15) -> covid.test.review.common

covid.test.review.common %>%
  filter(location == "Charleston SC") -> charlestonResults

ggplot() +
  geom_line(data = charlestonResults,
             mapping = aes(x = dateLead14, y = CaseLeadDay14, color = "a")) +
  geom_line(data = charlestonResults,
            mapping = aes(x = dateLead14, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Charleston, SC", x = "Date",
     y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review.common %>%
  filter(location == "Lafayette IN") -> lafayetteResults

ggplot() +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead14, y = CaseLeadDay14, color = "a")) +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead14, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Lafayette, IN", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review.common %>%
  filter(location == "San Diego CA") -> sanDiegoResults

ggplot() +
  geom_line(data = sanDiegoResults,
            mapping = aes(x = dateLead14, y = CaseLeadDay14, color = "a")) +
  geom_line(data = sanDiegoResults,
            mapping = aes(x = dateLead14, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in San Diego, CA", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual')) +
  theme(legend.position = "bottom")

ggplot(data = covid.test.review, mapping = aes(x = Predict, y = CaseLeadDay14)) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy", x = "Predicted Cases", y = "Actual Cases") +
  coord_cartesian(xlim = c(0, 15000), ylim = c(0, 15000))

ggplot(data = covid.test.review,
       mapping = aes(x = log(Predict + 1), y = log(CaseLeadDay14 + 1))) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy (Log Transformed)",
       x = "Predicted Cases (Log)",
       y = "Actual Cases (Log)") +
  coord_cartesian(xlim = c(0, log(15000)), ylim = c(0, log(15000)))

covid.test.review %>%
  mutate(bucket = cut(CaseLeadDay14,
                      breaks = c(0, 99, 15000),
                      labels = c("<100 Cases", "100+ Cases"))) %>%
  filter(!is.na(bucket)) %>%
  mutate(residual = CaseLeadDay14 - Predict) %>%
  mutate(residualPercent = residual / CaseLeadDay14) -> covid.test.review

ggplot(data = covid.test.review, mapping = aes(x = bucket, y = residualPercent)) +
  geom_violin() +
  labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
       y = "Percent Error") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(-5, 1))

covid.test.review %>%
  filter(CaseLeadDay14 > 100) -> covid.test.review.10Plus

ggplot(data = covid.test.review.10Plus, mapping = aes(x = bucket, y = residualPercent)) +
geom_violin() +
labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
     y = "Percent Error") +
  scale_y_continuous(labels = percent)


#Predict future hotspots
covid %>%
  filter(date == "2020-03-28") -> covidNext

covidNext.geos <- covidNext$location
covidNext <- select(covidNext, -c(location))

set.seed(15071)
#1.1. Preparation of the train and test matrices

x.test.next=model.matrix(CaseLeadDay14~. - date, data=covidNext)
y.test.next=covidNext$CaseLeadDay14

preds.xgb.next <- predict(xgb.final, newdata = x.test.next)


covid.next.review <- select(covidNext, c("CaseLeadDay14", "date", "hits",
                                          "hits.adj", "New_Cases", "New_Deaths",
                                          "Total_Cases", "Total_Deaths"))

covid.next.review$Predict <- preds.xgb.next

covid.next.review$Predict[covid.next.review$Predict < 0] <- 0

#Add back in the locations
covid.next.review$location <- covidNext.geos

#Reorder the locations factor in descending order of cases at the end
covid.next.review$location <- factor(covid.next.review$location,
                                     levels = covid.next.review$location[order(covid.next.review$CaseLeadDay14, decreasing = T)])
ggplot() +
  geom_point(data = covid.next.review,
             mapping = aes(x = location, y = CaseLeadDay14, color = "a")) +
  geom_point(data = covid.next.review,
             mapping = aes(x = location, y = Predict, color = "p")) +
  labs(title = "Predicted Cases in Each DMA on April 11th", x = "Location",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('p'='blue','a'='red'),
                      labels = c('Actual','Predicted')) +
  theme(legend.position = "bottom", axis.text.x = element_blank())

ggplot() +
  geom_point(data = covid.next.review,
            mapping = aes(x = location, y = log(CaseLeadDay14+1), color = "a")) +
  geom_point(data = covid.next.review,
            mapping = aes(x = location, y = log(Predict+1), color = "p")) +
  labs(title = "Predicted Cases in Each DMA at End of Dataset", x = "Location",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('p'='blue','a'='red'),
                      labels = c('Actual','Predicted')) +
  theme(legend.position = "bottom", axis.text.x = element_blank())

#5. Train/Test model split by geography

#5.1 Break geographies into train/test sections

locations <- unique(covid$location)

set.seed(144)
split = sample(locations, length(locations) * .75)
covid.train <- covid[covid$location %in% split,]
covid.test <- covid[(covid$location %in% split),]

#Save and remove geographies to add back on in the end
covid.train.geos <- covid.train$location
covid.test.geos <- covid.test$location

covid.train <- select(covid.train, -c(location))
covid.test <- select(covid.test, -c(location))

set.seed(15071)

x.train=model.matrix(CaseLeadDay14~. - date, data=covid.train)
y.train= covid.train$CaseLeadDay14
x.test=model.matrix(CaseLeadDay14~. - date, data=covid.test)
y.test=covid.test$CaseLeadDay14


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

#5.2 Track Prediction Accuracy

#Add back in the locations
covid.test.review$location <- covid.test.geos
covid.test.review$date <- as.Date(covid.test.review$date)
covid.test.review$dateLead14 <- covid.test.review$date + 14

covid.test.review %>%
  filter(location == "Charleston SC") -> charlestonResults

ggplot() +
  geom_line(data = charlestonResults,
            mapping = aes(x = dateLead14, y = CaseLeadDay14, color = "a")) +
  geom_line(data = charlestonResults,
            mapping = aes(x = dateLead14, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Charleston, SC", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review %>%
  filter(location == "San Diego CA") -> sanDiegoResults

ggplot() +
  geom_line(data = sanDiegoResults,
            mapping = aes(x = dateLead14, y = CaseLeadDay14, color = "a")) +
  geom_line(data = sanDiegoResults,
            mapping = aes(x = dateLead14, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in San Diego, CA", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review %>%
  filter(location == "Philadelphia PA") -> PhiladelphiaResults

ggplot() +
  geom_line(data = PhiladelphiaResults,
            mapping = aes(x = dateLead14, y = CaseLeadDay14, color = "a")) +
  geom_line(data = PhiladelphiaResults,
            mapping = aes(x = dateLead14, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Philadelphia, PA", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

#6. Predict daily deaths

#5.1 Train Test Split

covid <-    model_data %>% drop_na(paste("CaseLagDay.",lag,sep=""))
covid <-    covid %>% drop_na(paste("CaseLeadDay",lead,sep=""))

for (i in 1:14)
{
  covid <- select(covid, -c(paste("CaseLeadDay",i,sep="")))

  if (i != lead){
    covid <- select(covid, -c(paste("DeathLeadDay",i, sep="")))
  }

  if (i > lag){
    covid <- select(covid, -c(paste("CaseLagDay.",i, sep=""),paste("DeathLagDay.",i,sep="")))
  }

}

set.seed(144)
split = createDataPartition(covid$DeathLeadDay14, p = 0.75, list = FALSE)
covid.train <- covid[split,]
covid.test <- covid[-split,]

#Save and remove geographies to add back on in the end
covid.train.geos <- covid.train$location
covid.test.geos <- covid.test$location

covid.train <- select(covid.train, -c(location))
covid.test <- select(covid.test, -c(location))

set.seed(15071)

x.train=model.matrix(DeathLeadDay14~. - date, data=covid.train)
y.train= covid.train$DeathLeadDay14
x.test=model.matrix(DeathLeadDay14~. - date, data=covid.test)
y.test=covid.test$DeathLeadDay14

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

covid.test.review <- select(covid.test, c("DeathLeadDay14", "date", "hits",
                                          "hits.adj", "New_Cases", "New_Deaths",
                                          "Total_Cases", "Total_Deaths"))

covid.test.review$Predict <- preds.xgb.test

covid.test.review$Predict[covid.test.review$Predict < 0] <- 0

#5.2 Track Prediction Accuracy

#Add back in the locations
covid.test.review$location <- covid.test.geos
covid.test.review$date <- as.Date(covid.test.review$date)
covid.test.review$dateLead14 <- covid.test.review$date + 14

covid.test.review %>%
  filter(location == "San Diego CA") -> sanDiegoResults

ggplot() +
  geom_line(data = sanDiegoResults,
            mapping = aes(x = dateLead14, y = DeathLeadDay14, color = "a")) +
  geom_line(data = sanDiegoResults,
            mapping = aes(x = dateLead14, y = Predict, color = "b")) +
  labs(title = "Predicted Deaths in San Diego, CA", x = "Date",
       y = "Number of Deaths (per day)") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review %>%
  filter(location == "Philadelphia PA") -> PhiladelphiaResults

ggplot() +
  geom_line(data = PhiladelphiaResults,
            mapping = aes(x = dateLead14, y = DeathLeadDay14, color = "a")) +
  geom_line(data = PhiladelphiaResults,
            mapping = aes(x = dateLead14, y = Predict, color = "b")) +
  labs(title = "Predicted Deaths in Philadelphia, PA", x = "Date",
       y = "Number of Deaths (per day)") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

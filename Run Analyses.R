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
library(lubridate)
library(corrplot)

# Split DMAs Up in Train/Test ---------------------------------------------

final_data <- read_csv("Data/Final_Merge_COVID.csv")

final_data %>%
  select(-c(TotalCivPopAmbulatoryDisability_sum,
            TotalCivPopSelfCareDisability_sum,
            TotalCivPopIndLivingDisability_sum)) -> covid

DMAs <- unique(covid$DMA)

set.seed(144)
split = sample(length(DMAs), size = floor(.7*length(DMAs)), replace = F)
dma.train <- DMAs[split]
dma.test <- DMAs[-split]

covid.train <- covid[covid$DMA %in% dma.train,]
covid.test <- covid[covid$DMA %in% dma.test,]

#Save and remove geographies to add back on in the end
covid.train.geos <- covid.train$DMA
covid.test.geos <- covid.test$DMA

covid.train <- select(covid.train, -c(DMA, CasesIn14Days, DeathsIn14Days, DeathsIn28Days))
covid.test <- select(covid.test, -c(DMA, CasesIn14Days, DeathsIn14Days, DeathsIn28Days))

covid.train.x <- select(covid.train, -CasesIn28Days)
covid.train.y <- select(covid.train, CasesIn28Days)
covid.test.x <- select(covid.test, -CasesIn28Days)
covid.test.y <- select(covid.test, CasesIn28Days)

#Train Linear Model

model <- lm(CasesIn28Days ~ . - date, data = covid.train)

summary(model)

pred.covid <- predict(model, newdata = covid.test)

SSE.improved = sum((pred.covid - covid.test$CasesIn28Days)^2)
SST.original = sum((mean(covid.train$CasesIn28Days) - covid.test$CasesIn28Days)^2)
OSR2.improved = 1 - (SSE.improved/SST.original)
OSR2.improved

#Train LASSO

#1. Ridge Regression & LASSO Train

set.seed(15071)

#1.1. Preparation of the train and test matrices

x.train=model.matrix(CasesIn28Days~. - date, data=covid.train)
y.train= covid.train$CasesIn28Days
x.test=model.matrix(CasesIn28Days~. - date, data=covid.test)
y.test=covid.test$CasesIn28Days

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
ridge.train.resid <- covid.train$CasesIn28Days - pred.ridge.train
lasso.train.resid <- covid.train$CasesIn28Days - pred.lasso.train

ridge.test.resid <- covid.test$CasesIn28Days - pred.ridge.test
lasso.test.resid <- covid.test$CasesIn28Days - pred.lasso.test

#3.3 R^2 & OSR^2

R2.ridge <- 1 - sum(ridge.train.resid^2) / sum((covid.train$CasesIn28Days - mean(covid.train$CasesIn28Days))^2)
R2.lasso <- 1 - sum(lasso.train.resid^2) / sum((covid.train$CasesIn28Days - mean(covid.train$CasesIn28Days))^2)

OSR2.ridge <- 1 - sum(ridge.test.resid^2) / sum((covid.test$CasesIn28Days - mean(covid.train$CasesIn28Days))^2)
OSR2.lasso <- 1 - sum(lasso.test.resid^2) / sum((covid.test$CasesIn28Days - mean(covid.train$CasesIn28Days))^2)

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

covid.test.review <- select(covid.test, c("CasesIn28Days", "date", "hits.adj",
                                          "hits.adj", "new_cases", "new_deaths",
                                          "cases", "deaths"))

covid.test.review$Predict <- preds.xgb.test

covid.test.review$Predict[covid.test.review$Predict < 0] <- 0

#4. Interpretation of Results

#4.1 Track Prediction Accuracy

#Add back in the locations
covid.test.review$location <- covid.test.geos
covid.test.review$date <- as.Date(covid.test.review$date)
covid.test.review$dateLead28 <- covid.test.review$date + 28

covid.test.review %>%
  group_by(location) %>%
  mutate(n = n()) %>%
  filter(n > 15) -> covid.test.review.common

covid.test.review.common %>%
  filter(location == "ATLANTA (GA)") -> charlestonResults

ggplot() +
  geom_line(data = charlestonResults,
             mapping = aes(x = dateLead28, y = CasesIn28Days, color = "a")) +
  geom_line(data = charlestonResults,
            mapping = aes(x = dateLead28, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Charleston, SC", x = "Date",
     y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review.common %>%
  filter(location == "GREENWOOD (MS) - GREENVILLE (MS)") -> lafayetteResults

ggplot() +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead28, y = CasesIn28Days, color = "a")) +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead28, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Lafayette, IN", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

ggplot(data = covid.test.review, mapping = aes(x = Predict, y = CasesIn28Days)) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy", x = "Predicted Cases", y = "Actual Cases") +
  coord_cartesian(xlim = c(0, 15000), ylim = c(0, 15000))

ggplot(data = covid.test.review,
       mapping = aes(x = log(Predict + 1), y = log(CasesIn28Days + 1))) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy (Log Transformed)",
       x = "Predicted Cases (Log)",
       y = "Actual Cases (Log)") +
  coord_cartesian(xlim = c(0, log(15000)), ylim = c(0, log(15000)))

covid.test.review %>%
  mutate(bucket = cut(CasesIn28Days,
                      breaks = c(0, 99, 15000),
                      labels = c("<100 Cases", "100+ Cases"))) %>%
  filter(!is.na(bucket)) %>%
  mutate(residual = CasesIn28Days - Predict) %>%
  mutate(residualPercent = residual / CasesIn28Days) -> covid.test.review

ggplot(data = covid.test.review, mapping = aes(x = bucket, y = residualPercent)) +
  geom_violin() +
  labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
       y = "Percent Error") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(-5, 1))

covid.test.review %>%
  filter(CasesIn28Days > 100) -> covid.test.review.10Plus

ggplot(data = covid.test.review.10Plus, mapping = aes(x = bucket, y = residualPercent)) +
geom_violin() +
labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
     y = "Percent Error") +
  scale_y_continuous(labels = percent)

# Split Train/Test By Date ---------------------------------------------

rm(list = ls())

final_data <- read_csv("Data/Final_Merge_COVID.csv")

final_data %>%
  select(-c(TotalCivPopAmbulatoryDisability_sum,
            TotalCivPopSelfCareDisability_sum,
            TotalCivPopIndLivingDisability_sum)) -> covid

days <- unique(covid$date)
split_point <- days[floor(length(days)*.7)]

covid.train <- covid[covid$date <= split_point,]
covid.test <- covid[covid$date > split_point,]

#Save and remove geographies to add back on in the end
covid.train.geos <- covid.train$DMA
covid.test.geos <- covid.test$DMA

covid.train <- select(covid.train, -c(DMA, CasesIn14Days, DeathsIn14Days, DeathsIn28Days))
covid.test <- select(covid.test, -c(DMA, CasesIn14Days, DeathsIn14Days, DeathsIn28Days))

covid.train.x <- select(covid.train, -CasesIn28Days)
covid.train.y <- select(covid.train, CasesIn28Days)
covid.test.x <- select(covid.test, -CasesIn28Days)
covid.test.y <- select(covid.test, CasesIn28Days)

#Train Linear Model

model <- lm(CasesIn28Days ~ . - date, data = covid.train)

summary(model)

covid.train %>%
  select(-c(date, HousingMobileHome_sum, TotEmpOther_sum,
            TotalCivPopFemale_sum, Pop25UpBachelorsUp_sum,
            TotalPop85Up_sum, TotalPopUnder18_sum,
            TotalPop18Up_sum, TotalPop75Up_sum,
            TotalPop65Up_sum, TotalPop18_24_sum,
            TotalPop15_44_sum, TotalPop5_14_sum,
            TotalPop60Up_sum, `CaseLagDay-5`,
            `CaseLagDay-11`, TotalCivPopWithDisability_sum,
            TotWorkerConstruction_Maintenance_sum,
            TotWorkerSales_Office_sum, HousesInPoverty_sum,
            Pop25UpHighSchoolPlus_sum, TotalCivPopMale_sum,
            TotalPop65_69_sum, TotalPop15_17_sum,
            `CaseLagDay-13`, `CaseLagDay-3`, FREQUENTLY,
            SOMETIMES, TotEmpSelfEmployed_sum,
            TotWorkerProduction_Transportation_sum,
            HousesWithChildren_sum, `CaseLagDay-12`,
            TotalCivIncome25kUnder_sum,
            Pop5UpSpeakSpanish_sum, TotalCivIncome100kUp_sum,
            HousesWith60Plus_sum, TotalPop21Up_sum,
            `CaseLagDay-7`, RARELY, ALWAYS,
            TotalCivIncome50k_75k_sum,
            Housing2Apartments_sum, TotEmpProfessional_sum,
            `CaseLagDay-14`, `CaseLagDay-10`,
            TotalPop16Up_sum, TotalPop50_54_sum,
            TotalPopUnder5_sum, TotalPop25_29_sum,
            TotWorkerManagement_sum, TotalPop5_9_sum,
            TotalPop55_59_sum, TotalPop35_39_sum,
            `CaseLagDay-4`, `CaseLagDay-2`,
            TotalPop40_44_sum, TotalPop62Up_sum,
            TotalPop70_74_sum, mob_retail_and_rec,
            `CaseLagDay-6`, TotalCivPopAsian_sum,
            TotalPop20_24_sum, TotWorkerDroveAlone_sum,
            TotalPop80_84_sum, TotalPop45_49_sum,
            Pop25UpSomeCollege_sum, TotalPop60_64_sum,
            Housing1Detached_sum,
            TotalCivNonInstiUnInsured_sum,
            Pop5UpNonEnglishAtHome_sum,
            Pop5UpForeignBorn_sum, Housing1Attached_sum,
            `CaseLagDay-1`, `CaseLagDay-9`,
            TotalPop75_79_sum, HousesWithFoodStamps_sum,
            Pop25UpLessThanHighSchool_sum,
            Housing10PlusApartments_sum,
            TotalCivPopHawaiin_sum, TotEmpWholesale_sum,
            Housing5_9Apartments_sum, TotalCivPopWhite_sum,
            `CaseLagDay-8`, TotalCivPop2PlusRaces_sum)) -> covid.train2

cor1 <- cor(covid.train2)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(cor1, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .2,
         tl.cex = .45, addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

model <- lm(CasesIn28Days ~ ., data = covid.train2)

summary(model)

pred.covid <- predict(model, newdata = covid.test)

SSE.improved = sum((pred.covid - covid.test$CasesIn28Days)^2)
SST.original = sum((mean(covid.train$CasesIn28Days) - covid.test$CasesIn28Days)^2)
OSR2.improved = 1 - (SSE.improved/SST.original)
OSR2.improved

temp <- summary(model)
sort(temp$coefficients[,"Pr(>|t|)"], decreasing = T)

cor1 <- cor(covid.train2)
cor2 <- abs(cor1[,"new_cases"])
head(sort(cor2, decreasing = T))

#Train LASSO

#1. Ridge Regression & LASSO Train

set.seed(15071)

#1.1. Preparation of the train and test matrices

x.train=model.matrix(CasesIn28Days~. - date, data=covid.train)
y.train= covid.train$CasesIn28Days
x.test=model.matrix(CasesIn28Days~. - date, data=covid.test)
y.test=covid.test$CasesIn28Days

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
ridge.train.resid <- covid.train$CasesIn28Days - pred.ridge.train
lasso.train.resid <- covid.train$CasesIn28Days - pred.lasso.train

ridge.test.resid <- covid.test$CasesIn28Days - pred.ridge.test
lasso.test.resid <- covid.test$CasesIn28Days - pred.lasso.test

#3.3 R^2 & OSR^2

R2.ridge <- 1 - sum(ridge.train.resid^2) / sum((covid.train$CasesIn28Days - mean(covid.train$CasesIn28Days))^2)
R2.lasso <- 1 - sum(lasso.train.resid^2) / sum((covid.train$CasesIn28Days - mean(covid.train$CasesIn28Days))^2)

OSR2.ridge <- 1 - sum(ridge.test.resid^2) / sum((covid.test$CasesIn28Days - mean(covid.train$CasesIn28Days))^2)
OSR2.lasso <- 1 - sum(lasso.test.resid^2) / sum((covid.test$CasesIn28Days - mean(covid.train$CasesIn28Days))^2)

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
mat <- xgb.importance(colnames(x.train), model =xgb.final)
xgb.plot.importance(mat)

#Just the top variables
xgb.plot.importance(mat, top_n = 16, left_margin = 16, rel_to_first = T)

nodes <- xgb.dump(xgb.final, with_stats = T)
nodes[1:10]

#4. Interpretation of Results

covid.test.review <- select(covid.test, c("CasesIn28Days", "date", "hits.adj",
                                          "hits.adj", "new_cases", "new_deaths",
                                          "cases", "deaths"))

covid.test.review$Predict <- pred.lasso.test

covid.test.review$Predict[covid.test.review$Predict < 0] <- 0


#4.1 Track Prediction Accuracy

#Add back in the locations
covid.test.review$location <- covid.test.geos
covid.test.review$date <- as.Date(covid.test.review$date)
covid.test.review$dateLead28 <- covid.test.review$date + 28

covid.test.review %>%
  group_by(location) %>%
  mutate(n = n()) %>%
  filter(n > 15) -> covid.test.review.common

covid.test.review.common %>%
  filter(location == "WILMINGTON (NC)") -> wilmingtonResults

ggplot() +
  geom_line(data = wilmingtonResults,
            mapping = aes(x = dateLead28, y = Predict, color = "a")) +
  geom_line(data = wilmingtonResults,
            mapping = aes(x = dateLead28, y = CasesIn28Days, color = "b")) +
  labs(title = "Predicted Cases in Wilington, NC", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review.common %>%
  filter(location == "BOSTON (MA) - MANCHESTER (NH)") -> bostonResults

ggplot() +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead28, y = Predict, color = "a")) +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead28, y = CasesIn28Days, color = "b")) +
  labs(title = "Predicted Cases in Boston, MA", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

ggplot(data = covid.test.review, mapping = aes(x = Predict, y = CasesIn28Days)) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy", x = "Predicted Cases", y = "Actual Cases") +
  coord_cartesian(xlim = c(0, 15000), ylim = c(0, 15000))

ggplot(data = covid.test.review,
       mapping = aes(x = log(Predict + 1), y = log(CasesIn28Days + 1))) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy (Log Transformed)",
       x = "Predicted Cases (Log)",
       y = "Actual Cases (Log)") +
  coord_cartesian(xlim = c(0, log(15000)), ylim = c(0, log(15000)))

covid.test.review %>%
  mutate(bucket = cut(CasesIn28Days,
                      breaks = c(0, 99, 15000),
                      labels = c("<100 Cases", "100+ Cases"))) %>%
  filter(!is.na(bucket)) %>%
  mutate(residual = CasesIn28Days - Predict) %>%
  mutate(residualPercent = residual / CasesIn28Days) -> covid.test.review

ggplot(data = covid.test.review, mapping = aes(x = bucket, y = residualPercent)) +
  geom_violin() +
  labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
       y = "Percent Error") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(-5, 1))

covid.test.review %>%
  filter(CasesIn28Days > 100) -> covid.test.review.10Plus

ggplot(data = covid.test.review.10Plus, mapping = aes(x = bucket, y = residualPercent)) +
  geom_violin() +
  labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
       y = "Percent Error") +
  scale_y_continuous(labels = percent)

# Split Train/Test By Date - Deaths ------------------------------------------

rm(list = ls())

final_data <- read_csv("Data/Final_Merge_COVID.csv")

final_data %>%
  select(-c(TotalCivPopAmbulatoryDisability_sum,
            TotalCivPopSelfCareDisability_sum,
            TotalCivPopIndLivingDisability_sum)) -> covid

days <- unique(covid$date)
split_point <- days[floor(length(days)*.7)]

covid.train <- covid[covid$date <= split_point,]
covid.test <- covid[covid$date > split_point,]

#Save and remove geographies to add back on in the end
covid.train.geos <- covid.train$DMA
covid.test.geos <- covid.test$DMA

covid.train <- select(covid.train, -c(DMA, CasesIn14Days, CasesIn28Days, DeathsIn14Days))
covid.test <- select(covid.test, -c(DMA, CasesIn14Days, CasesIn28Days, DeathsIn14Days))

covid.train.x <- select(covid.train, -DeathsIn28Days)
covid.train.y <- select(covid.train, DeathsIn28Days)
covid.test.x <- select(covid.test, -DeathsIn28Days)
covid.test.y <- select(covid.test, DeathsIn28Days)

#Train Linear Model

model <- lm(DeathsIn28Days ~ . - date, data = covid.train)

summary(model)

pred.covid <- predict(model, newdata = covid.test)

SSE.improved = sum((pred.covid - covid.test$DeathsIn28Days)^2)
SST.original = sum((mean(covid.train$DeathsIn28Days) - covid.test$DeathsIn28Days)^2)
OSR2.improved = 1 - (SSE.improved/SST.original)
OSR2.improved

#Train LASSO

#1. Ridge Regression & LASSO Train

set.seed(15071)

#1.1. Preparation of the train and test matrices

x.train=model.matrix(DeathsIn28Days~. - date, data=covid.train)
y.train= covid.train$DeathsIn28Days
x.test=model.matrix(DeathsIn28Days~. - date, data=covid.test)
y.test=covid.test$DeathsIn28Days

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
ridge.train.resid <- covid.train$DeathsIn28Days - pred.ridge.train
lasso.train.resid <- covid.train$DeathsIn28Days - pred.lasso.train

ridge.test.resid <- covid.test$DeathsIn28Days - pred.ridge.test
lasso.test.resid <- covid.test$DeathsIn28Days - pred.lasso.test

#3.3 R^2 & OSR^2

R2.ridge <- 1 - sum(ridge.train.resid^2) / sum((covid.train$DeathsIn28Days - mean(covid.train$DeathsIn28Days))^2)
R2.lasso <- 1 - sum(lasso.train.resid^2) / sum((covid.train$DeathsIn28Days - mean(covid.train$DeathsIn28Days))^2)

OSR2.ridge <- 1 - sum(ridge.test.resid^2) / sum((covid.test$DeathsIn28Days - mean(covid.train$DeathsIn28Days))^2)
OSR2.lasso <- 1 - sum(lasso.test.resid^2) / sum((covid.test$DeathsIn28Days - mean(covid.train$DeathsIn28Days))^2)

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
mat <- xgb.importance(colnames(x.train), model =xgb.final)
xgb.plot.importance(mat)

#Just the top variables
xgb.plot.importance(mat, top_n = 16, left_margin = 16, rel_to_first = T)

nodes <- xgb.dump(xgb.final, with_stats = T)
nodes[1:10]

covid.test.review <- select(covid.test, c("DeathsIn28Days", "date", "hits.adj",
                                          "hits.adj", "new_cases", "new_deaths",
                                          "cases", "deaths"))

covid.test.review$Predict <- preds.xgb.test

covid.test.review$Predict[covid.test.review$Predict < 0] <- 0

#4. Interpretation of Results

#4.1 Track Prediction Accuracy

#Add back in the locations
covid.test.review$location <- covid.test.geos
covid.test.review$date <- as.Date(covid.test.review$date)
covid.test.review$dateLead28 <- covid.test.review$date + 28

covid.test.review %>%
  group_by(location) %>%
  mutate(n = n()) %>%
  filter(n > 15) -> covid.test.review.common

covid.test.review.common %>%
  filter(location == "ATLANTA (GA)") -> charlestonResults

ggplot() +
  geom_line(data = charlestonResults,
            mapping = aes(x = dateLead28, y = DeathsIn28Days, color = "a")) +
  geom_line(data = charlestonResults,
            mapping = aes(x = dateLead28, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Charleston, SC", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review.common %>%
  filter(location == "GREENWOOD (MS) - GREENVILLE (MS)") -> lafayetteResults

ggplot() +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead28, y = DeathsIn28Days, color = "a")) +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead28, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Lafayette, IN", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

ggplot(data = covid.test.review, mapping = aes(x = Predict, y = DeathsIn28Days)) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy", x = "Predicted Cases", y = "Actual Cases") +
  coord_cartesian(xlim = c(0, 15000), ylim = c(0, 15000))

ggplot(data = covid.test.review,
       mapping = aes(x = log(Predict + 1), y = log(DeathsIn28Days + 1))) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy (Log Transformed)",
       x = "Predicted Cases (Log)",
       y = "Actual Cases (Log)") +
  coord_cartesian(xlim = c(0, log(15000)), ylim = c(0, log(15000)))

covid.test.review %>%
  mutate(bucket = cut(DeathsIn28Days,
                      breaks = c(0, 99, 15000),
                      labels = c("<100 Cases", "100+ Cases"))) %>%
  filter(!is.na(bucket)) %>%
  mutate(residual = DeathsIn28Days - Predict) %>%
  mutate(residualPercent = residual / DeathsIn28Days) -> covid.test.review

ggplot(data = covid.test.review, mapping = aes(x = bucket, y = residualPercent)) +
  geom_violin() +
  labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
       y = "Percent Error") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(-5, 1))

covid.test.review %>%
  filter(DeathsIn28Days > 100) -> covid.test.review.10Plus

ggplot(data = covid.test.review.10Plus, mapping = aes(x = bucket, y = residualPercent)) +
  geom_violin() +
  labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
       y = "Percent Error") +
  scale_y_continuous(labels = percent)

# Split Train/Test By Date - 2 weeks -------------------------------------

rm(list = ls())

final_data <- read_csv("Data/Final_Merge_COVID.csv")

final_data %>%
  select(-c(TotalCivPopAmbulatoryDisability_sum,
            TotalCivPopSelfCareDisability_sum,
            TotalCivPopIndLivingDisability_sum)) -> covid

days <- unique(covid$date)
split_point <- days[floor(length(days)*.7)]

covid.train <- covid[covid$date <= split_point,]
covid.test <- covid[covid$date > split_point,]

#Save and remove geographies to add back on in the end
covid.train.geos <- covid.train$DMA
covid.test.geos <- covid.test$DMA

covid.train <- select(covid.train, -c(DMA, CasesIn28Days, DeathsIn14Days, DeathsIn28Days))
covid.test <- select(covid.test, -c(DMA, CasesIn28Days, DeathsIn14Days, DeathsIn28Days))

covid.train.x <- select(covid.train, -CasesIn14Days)
covid.train.y <- select(covid.train, CasesIn14Days)
covid.test.x <- select(covid.test, -CasesIn14Days)
covid.test.y <- select(covid.test, CasesIn14Days)

#Train Linear Model

model <- lm(CasesIn14Days ~ . - date, data = covid.train)

summary(model)

pred.covid <- predict(model, newdata = covid.test)

SSE.improved = sum((pred.covid - covid.test$CasesIn14Days)^2)
SST.original = sum((mean(covid.train$CasesIn14Days) - covid.test$CasesIn14Days)^2)
OSR2.improved = 1 - (SSE.improved/SST.original)
OSR2.improved

#Train LASSO

#1. Ridge Regression & LASSO Train

set.seed(15071)

#1.1. Preparation of the train and test matrices

x.train=model.matrix(CasesIn14Days~. - date, data=covid.train)
y.train= covid.train$CasesIn14Days
x.test=model.matrix(CasesIn14Days~. - date, data=covid.test)
y.test=covid.test$CasesIn14Days

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
ridge.train.resid <- covid.train$CasesIn14Days - pred.ridge.train
lasso.train.resid <- covid.train$CasesIn14Days - pred.lasso.train

ridge.test.resid <- covid.test$CasesIn14Days - pred.ridge.test
lasso.test.resid <- covid.test$CasesIn14Days - pred.lasso.test

#3.3 R^2 & OSR^2

R2.ridge <- 1 - sum(ridge.train.resid^2) / sum((covid.train$CasesIn14Days - mean(covid.train$CasesIn14Days))^2)
R2.lasso <- 1 - sum(lasso.train.resid^2) / sum((covid.train$CasesIn14Days - mean(covid.train$CasesIn14Days))^2)

OSR2.ridge <- 1 - sum(ridge.test.resid^2) / sum((covid.test$CasesIn14Days - mean(covid.train$CasesIn14Days))^2)
OSR2.lasso <- 1 - sum(lasso.test.resid^2) / sum((covid.test$CasesIn14Days - mean(covid.train$CasesIn14Days))^2)

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
mat <- xgb.importance(colnames(x.train), model =xgb.final)
xgb.plot.importance(mat)

#Just the top variables
xgb.plot.importance(mat, top_n = 16, left_margin = 16, rel_to_first = T)

nodes <- xgb.dump(xgb.final, with_stats = T)
nodes[1:10]

covid.test.review <- select(covid.test, c("CasesIn14Days", "date", "hits.adj",
                                          "hits.adj", "new_cases", "new_deaths",
                                          "cases", "deaths"))

covid.test.review$Predict <- preds.xgb.test

covid.test.review$Predict[covid.test.review$Predict < 0] <- 0

#4. Interpretation of Results

#4.1 Track Prediction Accuracy

#Add back in the locations
covid.test.review$location <- covid.test.geos
covid.test.review$date <- as.Date(covid.test.review$date)
covid.test.review$dateLead28 <- covid.test.review$date + 28

covid.test.review %>%
  group_by(location) %>%
  mutate(n = n()) %>%
  filter(n > 15) -> covid.test.review.common

covid.test.review.common %>%
  filter(location == "ATLANTA (GA)") -> charlestonResults

ggplot() +
  geom_line(data = charlestonResults,
            mapping = aes(x = dateLead28, y = CasesIn14Days, color = "a")) +
  geom_line(data = charlestonResults,
            mapping = aes(x = dateLead28, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Charleston, SC", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

covid.test.review.common %>%
  filter(location == "GREENWOOD (MS) - GREENVILLE (MS)") -> lafayetteResults

ggplot() +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead28, y = CasesIn14Days, color = "a")) +
  geom_line(data = lafayetteResults,
            mapping = aes(x = dateLead28, y = Predict, color = "b")) +
  labs(title = "Predicted Cases in Lafayette, IN", x = "Date",
       y = "Number of Cases") +
  scale_color_identity(guide = 'legend') +
  scale_colour_manual(name = 'Color',
                      values =c('b'='blue','a'='red'),
                      labels = c('Predicted','Actual'))

ggplot(data = covid.test.review, mapping = aes(x = Predict, y = CasesIn14Days)) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy", x = "Predicted Cases", y = "Actual Cases") +
  coord_cartesian(xlim = c(0, 15000), ylim = c(0, 15000))

ggplot(data = covid.test.review,
       mapping = aes(x = log(Predict + 1), y = log(CasesIn14Days + 1))) +
  geom_point(alpha = .25) +
  geom_abline(slope = 1, color = "blue") +
  labs(title = "Model Accuracy (Log Transformed)",
       x = "Predicted Cases (Log)",
       y = "Actual Cases (Log)") +
  coord_cartesian(xlim = c(0, log(15000)), ylim = c(0, log(15000)))

covid.test.review %>%
  mutate(bucket = cut(CasesIn14Days,
                      breaks = c(0, 99, 15000),
                      labels = c("<100 Cases", "100+ Cases"))) %>%
  filter(!is.na(bucket)) %>%
  mutate(residual = CasesIn14Days - Predict) %>%
  mutate(residualPercent = residual / CasesIn14Days) -> covid.test.review

ggplot(data = covid.test.review, mapping = aes(x = bucket, y = residualPercent)) +
  geom_violin() +
  labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
       y = "Percent Error") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(-5, 1))

covid.test.review %>%
  filter(CasesIn14Days > 100) -> covid.test.review.10Plus

ggplot(data = covid.test.review.10Plus, mapping = aes(x = bucket, y = residualPercent)) +
  geom_violin() +
  labs(title = "Percent Error By Number of Cases", x = "Number of Cases",
       y = "Percent Error") +
  scale_y_continuous(labels = percent)


# Next --------------------------------------------------------------------

#Predict future hotspots
# Predict the cumulative cases over the next 28 days
covid %>%
  filter(date >= max(covid$date)-ddays(27)) -> covidNext

covidNext.geos <- covidNext$DMA
covidNext.date <- covidNext$date
covidNext <- select(covidNext, -c(DMA, date, CasesIn14Days,
                                  DeathsIn14Days,DeathsIn28Days))

x.train2=model.matrix(CasesIn28Days ~ ., data = covidNext)

pred.lasso.test <- predict(lasso.fin, newx = x.train2)

covidNext$pred_cases <- pred.lasso.test[,1]
covidNext$DMA <- covidNext.geos
covidNext$date <- covidNext.date

covidNext %>%
  select(pred_cases, CasesIn28Days) %>%
  plot()

covidNext %>%
  select(date, DMA, TotPop_sum, TotalPop60Up_sum, TotalCivPopBlack_sum,
         TotalCivPopAmericanIndian_sum, TotalCivPopAsian_sum,
         TotalCivPopHawaiin_sum, TotalCivPop2PlusRaces_sum,
         HousesWithFoodStamps_sum, TotEmpRetail_sum,
         TotEmpEducation_Healthcare_sum, TotEmpEntertainment_Food_sum,
         TotWorkerService_sum, pred_cases, deaths, cases) %>%
  mutate(pred_cases = ifelse(pred_cases < 0, 0, pred_cases)) %>%
  group_by(DMA) %>%
  summarise(pred_cases_cum = sum(pred_cases),
            pred_cases = last(pred_cases),
            deaths = max(deaths),
            cases = max(cases),
            TotPop_sum = mean(TotPop_sum),
            TotalPop60Up_sum = mean(TotalPop60Up_sum),
            TotalCivPopBlack_sum = mean(TotalCivPopBlack_sum),
            TotalCivPopAmericanIndian_sum = mean(TotalCivPopAmericanIndian_sum),
            TotalCivPopAsian_sum = mean(TotalCivPopAsian_sum),
            TotalCivPopHawaiin_sum = mean(TotalCivPopHawaiin_sum),
            TotalCivPop2PlusRaces_sum = mean(TotalCivPop2PlusRaces_sum),
            HousesWithFoodStamps_sum = mean(HousesWithFoodStamps_sum),
            TotEmpRetail_sum = mean(TotEmpRetail_sum),
            TotEmpEducation_Healthcare_sum = mean(TotEmpEducation_Healthcare_sum),
            TotEmpEntertainment_Food_sum = mean(TotEmpEntertainment_Food_sum),
            TotWorkerService_sum = mean(TotWorkerService_sum)) -> covidNext

# Calculate average death rate over lst month
covid %>%
  filter(date >= max(covid$date)-days(27)-months(1),
         date <= max(covid$date)-days(27)) -> covidLast

covidLast %>%
  select(DMA, new_cases, new_deaths) %>%
  group_by(DMA) %>%
  summarise(cases = sum(new_cases),
            deaths = sum(new_deaths)) %>%
  mutate(death_rate = deaths / cases) %>%
  select(DMA, death_rate) -> death_rate

covidNext <- left_join(covidNext, death_rate)

covidNext %>%
  mutate(pred_deaths_cum = pred_cases_cum * death_rate) -> covidNext

covidNext %>%
  select(-death_rate) %>%
  mutate(TotPopMinority = TotalCivPopBlack_sum + TotalCivPopAmericanIndian_sum +
           TotalCivPopAsian_sum + TotalCivPopHawaiin_sum +
           TotalCivPop2PlusRaces_sum) %>%
  select(-c(TotalCivPopBlack_sum, TotalCivPopAmericanIndian_sum,
            TotalCivPopAsian_sum, TotalCivPopHawaiin_sum,
            TotalCivPop2PlusRaces_sum)) %>%
  rename(TotPop = TotPop_sum) %>%
  mutate(TotalPop60Up = TotalPop60Up_sum * TotPop,
         TotPopMinority = TotPopMinority * TotPop,
         FoodStamps = HousesWithFoodStamps_sum * TotPop,
         Retail = TotEmpRetail_sum * TotPop,
         Ed_Health = TotEmpEducation_Healthcare_sum * TotPop,
         Rest_Entertain = TotEmpEntertainment_Food_sum * TotPop,
         Service = TotWorkerService_sum * TotPop) %>%
  select(DMA:TotPop, pred_deaths_cum:Service) %>%
  mutate(pred_cases = if_else(pred_cases < 0, 0, pred_cases)) -> covidNext

write_csv(covidNext, "Data/FinalPredictions.csv")


# Rejoin DMA Lables -------------------------------------------------------

covidNext <- read_csv("Data/FinalPredictions.csv")

DMA_Labels <- read_csv("raw_data/dmaGeoCodeCrosswalk.csv")

DMA_Labels %>%
  select(-location) -> DMA_Labels

covidNext2 <- left_join(covidNext, DMA_Labels)

write_csv(covidNext2, "Data/FinalPredictions_geoCode.csv")

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

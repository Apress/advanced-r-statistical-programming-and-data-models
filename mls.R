## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----

 library(checkpoint)
 checkpoint("2018-09-28", R.version = "3.5.1",
   project = book_directory,
   checkpointLocation = checkpoint_directory,
   scanForPackages = FALSE,
   scan.rnw.with.knitr = TRUE, use.knitr = TRUE)

library(ggplot2)
library(cowplot)
library(data.table)
library(readxl)
library(viridis)


library(RSNNS)
library(kernlab)
library(rpart)
library(rattle)
library(DALEX)
library(caret)
library(spdep)
library(ranger)
library(e1071)
library(gbm)
library(plyr)
set.seed(1234)

options(width = 70, digits = 2)

## ------------------------------------------------------------------------
## Note: download Excel file  from publisher website first
dRaw <- read_excel("Gender_StatsData_worldbank.org_ccby40.xlsx")
dRaw <- as.data.table(dRaw) # convert data to data.table format.

dRaw[,`Indicator Name`:= NULL]

## collapse columns into a super long dataset
## with Year as a new variable
data <- melt(dRaw, measure.vars = 3:20, variable.name = "Year", variable.factor = FALSE)

## cast the data wide again
## this time with separate variables by indicator code
## keeping a country and time (Year) variable
data <- dcast(data, CountryName + Year ~ IndicatorCode)
rm(dRaw) #remove unneeded variable

#rename columns with shortened, unique names
x<-colnames(data)
x<-gsub("[[:punct:]]", "", x)
(y <- abbreviate(x, minlength = 4, method = "both.sides"))
names(data) <- y

#shorten regional names to abbreviations.
data$CntN<-abbreviate(data$CntN, minlength = 5, method = "left.kept")

## ------------------------------------------------------------------------
d <- copy(data)
sort(unique(d$CntN))

## ------------------------------------------------------------------------
str(d)
d[,Year:=as.numeric(Year)]
ddum <- dummyVars("~.", data = d)
d <- data.table(predict(ddum, newdata = d))
rm(ddum) #remove ddum as unneeded
str(d)

## ------------------------------------------------------------------------
dScaled<-scale(d[,-c(1:9)])
dScaled<-as.data.table(dScaled)
d <- cbind(d[,c(1:9)], dScaled)
rm(dScaled) #remove d2 as unneeded
str(d)

## ----mls-eda1, fig.width=12, fig.height=5, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = "Looking for significant data misshapes."----
boxplot(d[,-c(1:9)], las = 2)

## ----mls-eda2, fig.width=12, fig.height=5, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = "Looking for significant data misshapes."----
par(mfrow = c(1,2))
hist(d$ERMA, 100)
qqnorm(d$ERMA)
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
shapiro.test(d$ERMA)

## ------------------------------------------------------------------------
range(d$ERMA)
range(data$ERMA)
shapiro.test( log(data$ERMA) )

## ----mls-eda3, fig.width=4, fig.height=4, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Looking for significant data misshapes."----
par(mfrow = c(1,2))
hist(data$ERMA, 100)
hist( log (data$ERMA) , 100)
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
d2 <- copy(data[,.(SPAD, ERMA)])
d2[, Log.ERMA := log(ERMA)]
cor(d2)
rm(d2) #no longer needed

## ------------------------------------------------------------------------
lapply(data[,-c(1:2)], shapiro.test)

dlog <- copy(data)
dlog <- sapply(dlog[,-c(1:2)], log)
dlog<-as.data.table(dlog)
colnames(dlog) <- paste(colnames(dlog), "LOG", sep = ".")

dlog<-cbind(data, dlog)
View(cor(dlog[,-c(1:2)]))
rm(dlog) #remove as we will not use.

## ------------------------------------------------------------------------
set.seed(1234)
index <- createDataPartition(data$CntN, p = 0.8, list = FALSE)
trainData <- data[index, ]
validationData <- data[-index, ]

## ------------------------------------------------------------------------
#source("https://bioconductor.org/biocLite.R")
#biocLite("pcaMethods")
library(pcaMethods)

## ------------------------------------------------------------------------
#confirm structure
str(trainData[,c(3:8,10:13)])

#base R / traditional method
pc <- prcomp(trainData[,c(3:8,10:13)], center = TRUE, scale. = TRUE)
summary(pc)
pcValidationData1 <- predict(pc, newdata = validationData[,c(3:8,10:13)])

#scalable method using PcaMethods
pc<-pca(trainData[,c(1:8,10:13)], method = "svd",nPcs = 4, scale = "uv", center = TRUE)
pc
summary(pc)
pcValidationData2 <- predict(pc, newdata = validationData[,c(3:8,10:13)])

#demonstration of how to access transformed validation data
pcValidationData1[,1]
pcValidationData2$scores[,1]

## ----mls-svm1, fig.width=8, fig.height=4, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Per Capita GNP vs Adolescent Fertility Rate."----

svmDataTrain <- trainData[,.(SPAD, NYGN)]
svmDataValidate <- validationData[,.(SPAD, NYGN)]

p1 <- ggplot(data = svmDataTrain,
             aes(x = NYGN, y = SPAD))
  ## data poins colored by country
  p1 + geom_point(aes(colour = trainData$CntN)) + 
    scale_colour_viridis(discrete = TRUE)


## ------------------------------------------------------------------------
set.seed(12345)

  svm <- train(x = svmDataTrain,
             y = trainData$CntN,
             method = "svmLinear",
             preProcess = NULL,
             metric = "Accuracy",
             trControl = trainControl(method = "cv",
                                      number = 5,
                                      seeds = c(123, 234, 345, 456, 567, 678)
                                      )
             )
svm

## ------------------------------------------------------------------------
#predict the country name on our training data using our new model
predictOnTrain <- predict(svm, newdata = svmDataTrain)

mean( predictOnTrain == trainData$CntN)

## ------------------------------------------------------------------------
predictOnTest <- predict(svm, newdata = svmDataValidate)
mean(predictOnTest == validationData$CntN)

## ----mls-svm2, fig.width=8, fig.height=6, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Train vs Test Predictions."----

p1 <- ggplot(data = validationData,
             aes(x = NYGN, y = SPAD))

plot_grid(
  ## data poins colored by country
  p1 + geom_point(aes(colour = validationData$CntN, size = validationData$CntN)) + 
    scale_colour_viridis(discrete = TRUE),

  ## data poins colored by predicted country
  p1 + geom_point(aes(colour = predictOnTest, size = predictOnTest)) + 
    scale_colour_viridis(discrete = TRUE),
ncol = 1
)  

## ------------------------------------------------------------------------
rm(p1)
rm(svm)
rm(svmDataTrain)
rm(svmDataValidate)
rm(pcValidationData1)
rm(pcValidationData2)
rm(predictOnTest)
rm(predictOnTrain)
rm(pc)
rm(d)

## ------------------------------------------------------------------------
# set up training & validation data
svmDataTrain <- trainData[,-1]
svmDataTrain[,Year:=as.numeric(Year)]
svmDataValidation <- validationData[,-1]
svmDataValidation[,Year:=as.numeric(Year)]
#run linear SVM on the full data set
set.seed(12345)
svmLinear <- train(x = svmDataTrain,
             y = trainData$CntN,
             method = "svmLinear",
             preProcess = c("scale", "center", "pca"),
             metric = "Accuracy",
             trControl = trainControl(method = "cv",
                                      number = 5,
                                      seeds = c(123, 234, 345, 456, 567, 678)
                                      )
             )
svmLinear

## ------------------------------------------------------------------------
#run polynomial SVM on the full data set
set.seed(12345)
svmPoly <- train(x = svmDataTrain,
             y = trainData$CntN,
             method = "svmPoly",
             preProcess = c("scale", "center", "pca"),
             metric = "Accuracy",
             trControl = trainControl(method = "cv",
                                      number = 5
                                      )
             )

svmPoly

## ------------------------------------------------------------------------
predictOnTrainL <- predict(svmLinear, newdata = svmDataTrain)
mean( predictOnTrainL == trainData$CntN)

predictOnTrainP <- predict(svmPoly, newdata = svmDataTrain)
mean( predictOnTrainP == trainData$CntN)

## ------------------------------------------------------------------------
predictOnTestL <- predict(svmLinear, newdata = svmDataValidation)
mean(predictOnTestL == validationData$CntN)

## ------------------------------------------------------------------------
cartDataTrain <- copy(trainData[,-1])
cartDataTrain[,Year:=as.numeric(Year)]
cartDataValidation <- copy(validationData[,-1])
cartDataValidation[,Year:=as.numeric(Year)]

set.seed(12345)
cartModel <- train(x = cartDataTrain,
             y = trainData$CntN,
             method = "rpart",
             preProcess = c("scale", "center", "pca"),
             metric = "Accuracy",
             tuneLength = 10,
             trControl = trainControl(method = "cv",
                                      number = 5
                                      )
             )

cartModel

## ----mls-crt1, fig.width=9, fig.height=7, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Classification Tree Graph."----
plot(cartModel$finalModel)
text(cartModel$finalModel, cex = 0.5)

## ----mls-crt2, fig.width=6, fig.height=7, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Fancy Classification Tree Graph"----
fancyRpartPlot(cartModel$finalModel, cex = 0.4, main = "")

## ------------------------------------------------------------------------
predictOnTrainT <- predict(cartModel, newdata = cartDataTrain)
mean( predictOnTrainT == trainData$CntN)

predictOnTestT <- predict(cartModel, newdata = cartDataValidation)
mean(predictOnTestT == validationData$CntN)

## ------------------------------------------------------------------------
confusionMatrix(predictOnTestT, as.factor(validationData$CntN))

## ------------------------------------------------------------------------
rfDataTrain <- copy(trainData[,-1])
rfDataTrain[,Year:=as.numeric(Year)]
rfDataValidation <- copy(validationData[,-1])
rfDataValidation[,Year:=as.numeric(Year)]

set.seed(12345)

rfModel <- train(x = rfDataTrain,
             y = trainData$CntN,
             method = "ranger",
             preProcess = c("scale", "center", "pca"),
             metric = "Accuracy",
             num.trees = 20,
             trControl = trainControl(method = "cv",
                                      number = 5
                                      )
             )

rfModel
rfModel$finalModel$num.trees

## ------------------------------------------------------------------------
predictOnTrainR <- predict(rfModel, newdata = rfDataTrain)
mean( predictOnTrainR == trainData$CntN)

predictOnTestR <- predict(rfModel, newdata = rfDataValidation)
mean(predictOnTestR == validationData$CntN)

## ------------------------------------------------------------------------
set.seed(12345)
rfModel <- train(x = rfDataTrain,
             y = trainData$CntN,
             method = "ranger",
             preProcess = c("scale", "center", "pca"),
             metric = "Accuracy",
             num.trees = 50,
             trControl = trainControl(method = "cv",
                                      number = 5
                                      )
             )
rfModel
rfModel$finalModel$num.trees

## ------------------------------------------------------------------------
predictOnTrainR <- predict(rfModel, newdata = rfDataTrain)
mean( predictOnTrainR == trainData$CntN)

predictOnTestR <- predict(rfModel, newdata = rfDataValidation)
mean(predictOnTestR == validationData$CntN)

## ------------------------------------------------------------------------

set.seed(12345)

rfModel <- train(x = rfDataTrain,
             y = trainData$CntN,
             method = "ranger",
             preProcess = c("scale", "center", "pca"),
             metric = "Accuracy",
             num.trees = 20,
             trControl = trainControl(method = "cv",
                                      number = 5
                                      ),
             tuneGrid = expand.grid(mtry = c(1, 2, 3, 4),
                                    splitrule = "extratrees",
                                    min.node.size = c(1, 5, 10, 15))
             )

rfModel
rfModel$finalModel$num.trees
rfModel$finalModel$mtry
rfModel$finalModel$splitrule
rfModel$finalModel$min.node.size

## ------------------------------------------------------------------------
predictOnTrainR <- predict(rfModel, newdata = rfDataTrain)
mean( predictOnTrainR == trainData$CntN)

predictOnTestR <- predict(rfModel, newdata = rfDataValidation)
mean(predictOnTestR == validationData$CntN)

## ------------------------------------------------------------------------
sgbDataTrain <- copy(trainData)
sgbDataTrain[,Year:=as.numeric(Year)]
sgbDataValidation <- copy(validationData)
sgbDataValidation[,Year:=as.numeric(Year)]

ddum <- dummyVars("~.", data = sgbDataTrain)
sgbDataTrain <- data.table(predict(ddum, newdata = sgbDataTrain))
sgbDataValidation <- data.table(predict(ddum, newdata = sgbDataValidation))
rm(ddum)

## ------------------------------------------------------------------------
set.seed(12345)
sgbModel <- train(SPAD ~.,
                  data = sgbDataTrain,
             method = "gbm",
             preProcess = c("scale", "center"),
             metric = "RMSE",
             trControl = trainControl(method = "cv",
                                      number = 5
                                      ),
             tuneGrid = expand.grid(interaction.depth = 1:3,
                                    shrinkage = 0.1,
                                    n.trees = c(50, 100, 150),
                                    n.minobsinnode = 10),
             verbose = FALSE
             )
sgbModel

## ----mls-sgb1, fig.width=5, fig.height=10, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = "Relative Influnce Visualisation."----
summary(sgbModel)

## ------------------------------------------------------------------------
mean(stats::residuals(sgbModel)^2)

mean((predict(sgbModel, sgbDataValidation) - 
                  sgbDataValidation$SPAD)^2)

## ------------------------------------------------------------------------
explainSGBt <- explain(sgbModel, label = "sgbt",
               data = sgbDataTrain,
               y = sgbDataTrain$SPAD)

explainSGBv <- explain(sgbModel, label = "sgbv",
               data = sgbDataValidation,
               y = sgbDataValidation$SPAD)

## ----mls-sgb2, fig.width=8, fig.height=6, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "DALEX residual visualisations."----
performanceSGBt <- model_performance(explainSGBt)
performanceSGBv <- model_performance(explainSGBv)

plot_grid(
  plot(performanceSGBt, performanceSGBv),
  plot(performanceSGBt, performanceSGBv, geom = "boxplot"),
  ncol = 2)


## ----mls-sgb3, fig.width=6, fig.height=6, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = "DALEX Drop Out Loss."----

importanceSGBt <- variable_importance(explainSGBt)
importanceSGBv <- variable_importance(explainSGBv)
plot(importanceSGBt, importanceSGBv)

## ----mls-sgb4, fig.width=6, fig.height=6, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = "DALEX primary school non-attendance count versus teen pregnancy."----
responseSGBprmt <- variable_response(explainSGBt, variable = "SEPR", type = "pdp")
responseSGBprmv <- variable_response(explainSGBv, variable = "SEPR", type = "pdp")
plot(responseSGBprmt, responseSGBprmv)

## ----mls-sgb5, fig.width=6, fig.height=6, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = "DALEX death rate per 1000 versus teen pregnancy."----
responseSGBdynt <- variable_response(explainSGBt, variable = "SPDY", type = "pdp")
responseSGBdynv <- variable_response(explainSGBv, variable = "SPDY", type = "pdp")
plot(responseSGBdynt, responseSGBdynv)

## ------------------------------------------------------------------------
mlpDataTrain <- copy(trainData)
mlpDataTrain[,Year:=as.numeric(Year)]
mlpDataValidation <- copy(validationData)
mlpDataValidation[,Year:=as.numeric(Year)]

ddum <- dummyVars("~.", data = mlpDataTrain)
mlpDataTrain <- data.table(predict(ddum, newdata = mlpDataTrain))
mlpDataValidation <- data.table(predict(ddum, newdata = mlpDataValidation))
rm(ddum)

## ------------------------------------------------------------------------
set.seed(12345)
suppressWarnings(
  mlpModel <- train(
    SPAD ~ .,
    data = mlpDataTrain,
    method = "mlpML",
    preProcess = c("scale", "center"),
    metric = "RMSE",
    trControl = trainControl(method = "cv",
                             number = 5)
  )
)
mlpModel

## ------------------------------------------------------------------------
summary(mlpModel)

## ------------------------------------------------------------------------
mean(stats::residuals(mlpModel)^2)

mean((predict(mlpModel, mlpDataValidation) - 
                  mlpDataValidation$SPAD)^2)

## ------------------------------------------------------------------------
explainMLPt <- explain(mlpModel, label = "mlpt",
               data = mlpDataTrain,
               y = mlpDataTrain$SPAD)

explainMLPv <- explain(mlpModel, label = "mlpv",
               data = mlpDataValidation,
               y = mlpDataValidation$SPAD)

## ----mls-mlp1, fig.width=8, fig.height=6, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Model performance contrasting SGB vs MLP methods."----
performanceMLPt <- model_performance(explainMLPt)
performanceMLPv <- model_performance(explainMLPv)

plot_grid(
plot(performanceMLPt, performanceMLPv, performanceSGBt, performanceSGBv),
plot(performanceMLPt, performanceMLPv, performanceSGBt, performanceSGBv, geom = "boxplot"),
ncol = 2
)

## ----mls-mlp2, fig.width=6, fig.height=6, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Determining top key variables."----

importanceMLPt <- variable_importance(explainMLPt)
importanceMLPv <- variable_importance(explainMLPv)
plot(importanceMLPt, importanceMLPv, importanceSGBt, importanceSGBv)

## ----mls-mlp3, fig.width=6, fig.height=6, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Understanding the impact of primary school attendance on teen pregnancy."----
responseMLPprmt <- variable_response(explainMLPt, variable = "SEPR", type = "pdp")
responseMLPprmv <- variable_response(explainMLPv, variable = "SEPR", type = "pdp")
plot(responseMLPprmt, responseMLPprmv, responseSGBprmt, responseSGBprmv)

## ----mls-mlp4, fig.width=6, fig.height=6, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "DALEX death rate per 1000 versus teen pregnancy."----
responseMLPdynt <- variable_response(explainMLPt, variable = "SPDY", type = "pdp")
responseMLPdynv <- variable_response(explainMLPv, variable = "SPDY", type = "pdp")
plot(responseMLPdynt, responseMLPdynv, responseSGBdynt, responseSGBdynv)

## ------------------------------------------------------------------------
set.seed(12345)
suppressWarnings(
  mlpModelb <- train(
    SPAD ~ .,
    data = mlpDataTrain,
    method = "mlpML",
    preProcess = c("scale", "center"),
    metric = "RMSE",
    verbose = FALSE,
    trControl = trainControl(method = "cv",
                             number = 5),
    tuneGrid = expand.grid(
      layer1 = 0:10,
      layer2 = 0:10,
      layer3 = 0:10
    )
  )
)
mlpModelb

mean(stats::residuals(mlpModelb)^2)

mean((predict(mlpModelb, mlpDataValidation) -
                  mlpDataValidation$SPAD)^2)
summary(mlpModelb)


## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----

library(checkpoint)
checkpoint("2018-09-28", R.version = "3.5.1",
  project = book_directory,
  checkpointLocation = checkpoint_directory,
  scanForPackages = FALSE,
  scan.rnw.with.knitr = TRUE, use.knitr = TRUE)

library(knitr)
library(tidyverse)
library(rsample)
library(data.table)
library(boot)
library(parallel)
library(foreach)
library(doParallel)

options(width = 70, digits = 3)


## ------------------------------------------------------------------------

set.seed(5)
case_data <- initial_split(data = iris, prop = 0.8)
case_data


## ------------------------------------------------------------------------
data_train <- training(case_data)
data_test <- testing(case_data)
glimpse(data_train)

## ------------------------------------------------------------------------
unique(data_train$Species)

## ------------------------------------------------------------------------
length.lm = lm(Petal.Length ~ Sepal.Length + 
                 Sepal.Width + Petal.Width, 
               data = data_train)
length.lm
summary(length.lm)

## ------------------------------------------------------------------------
sqrt(
  sum(
    (fitted(length.lm)-data_train$Petal.Length)^2
  )/(nrow(data_train)-4)
)

## ------------------------------------------------------------------------
mse_train<- mean(length.lm$residuals^2)
mse_train

## ------------------------------------------------------------------------
sqrt(
  sum(
    (predict(length.lm, data_test)-data_test$Petal.Length)^2
  )/(nrow(data_test)-2)
)

mse_test <- mean((predict(length.lm, data_test) - 
                  data_test$Petal.Length)^2)
mse_test

## ----mli-qqplot, fig.width=7, fig.height=6, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "2D and Q-Q plots of some data"----
par(mfrow=c(2,2))
plot(data_train$Sepal.Length, data_train$Petal.Length)
plot(data_test$Sepal.Length, data_test$Petal.Length)
qqnorm(data_train$Petal.Length, 
       xlab = "Theoretical Quantiles Train")
qqnorm(data_test$Petal.Length, 
       xlab = "Theoretical Quantiles Test")

## ------------------------------------------------------------------------
summary(data_train$Sepal.Length)
summary(data_test$Sepal.Length)

## ------------------------------------------------------------------------
crossData <- iris %>% 
  sample_n(nrow(iris), replace = FALSE)
crossData <- add_column(crossData, 
  Bin = cut(1:150, breaks = 5, labels = c(1:5)))
store <- tibble(Fold=1:5, MSE=NA_integer_)

## ------------------------------------------------------------------------
for(i in 1:5){
  data_train<-crossData %>% filter(Bin != i)
  data_test<-crossData %>% filter(Bin == i)
  lengthFold.lm = lm(Petal.Length ~ Sepal.Length + 
                      Sepal.Width + Petal.Width, 
                     data = data_train)
  store[i,]$MSE <- mean((predict(lengthFold.lm, data_test) - 
                           data_test$Petal.Length)^2)
}

## ------------------------------------------------------------------------
mse_k <- mean(store$MSE)
mse_k

## ------------------------------------------------------------------------
lengthFold.lm <- lm(Petal.Length ~ Sepal.Length + 
                   Sepal.Width + Petal.Width, 
                   data = iris)
lengthFold.lm
mse_ALL <- mean(lengthFold.lm$residuals^2)
mse_ALL

## ------------------------------------------------------------------------
store

## ------------------------------------------------------------------------
mse <- function(data, i) {
  lengthBoot.lm <- lm(Petal.Length ~ Sepal.Length + 
                      Sepal.Width + Petal.Width,
                      data=data[i,])
  return(mean(lengthBoot.lm$residuals^2))
}

## ------------------------------------------------------------------------
bootResults <- boot(data=iris, statistic=mse, R=10000)
bootResults 

## ----mli-btplot1, fig.width=6, fig.height=4, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Boot Strap Results"----
plot(bootResults)

## ------------------------------------------------------------------------
boot.ci(bootResults, conf = 0.95, type="bca")

## ------------------------------------------------------------------------
detectCores()
detectCores(logical = TRUE)
detectCores(logical = FALSE)

## ------------------------------------------------------------------------
## notice 10000/2 = 5000
runP <- function(...) boot(data=iris, statistic=mse, R=5000)

## makes a cluster with 2 cores as 10000/5000 = 2
cl<-makeCluster(2) 

## passes along parts of the global environment 
## to each node / part of the cluster
## again, base is a file path variable to our book's path
## set book_directory <- "C:/YourPathHere/"
clusterExport(cl, c("runP", "mse", "book_directory", "checkpoint_directory" ))

## creates the library and some environment on 
## each of the parts of the cluster
clusterEvalQ(cl, {
  
library(checkpoint)
  checkpoint("2018-09-28", R.version = "3.5.1",
  project = book_directory,
  checkpointLocation = checkpoint_directory,
  scanForPackages = FALSE,
  scan.rnw.with.knitr = TRUE, use.knitr = TRUE)
  
    library(boot)
  })

## similar to set.seed() except for clusters
clusterSetRNGStream(cl, 5) 

## uses the parLapply() function which works on windows too
pBootResults <- do.call(c, parLapply(cl, seq_len(2), runP))
  
#stop the cluster
stopCluster(cl)

# view results
pBootResults

## get 95% confidence interval of the MSEs
## (note 0.95 is the default) 
boot.ci(pBootResults, conf = 0.95, type="bca")

## ----mli-btplot2, fig.width=6, fig.height=4, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Boot Strap Results"----
plot(pBootResults)

## ------------------------------------------------------------------------
cl <- makeCluster(2)
registerDoParallel(cl)

clusterExport(cl, c("book_directory", "checkpoint_directory"))

clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2018-09-28", R.version = "3.5.1",
  project = book_directory,
  checkpointLocation = checkpoint_directory,
  scanForPackages = FALSE,
  scan.rnw.with.knitr = TRUE, use.knitr = TRUE)
    
  library("tidyverse")
  })

## ------------------------------------------------------------------------
k <- foreach(i=1:5, .combine = c) %dopar% {
  data_train <- crossData %>% filter(Bin != i)
  data_test <- crossData %>% filter(Bin == i)
  lengthFold.lm <- lm(Petal.Length ~ Sepal.Length + 
                      Sepal.Width + Petal.Width,
                      data = data_train)
  mean((predict(lengthFold.lm, data_test) - 
          data_test$Petal.Length)^2)
}

## ------------------------------------------------------------------------
stopCluster(cl)
mse_Pk<-mean(k)
mse_Pk


## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----

library(checkpoint)
checkpoint("2018-09-28", R.version = "3.5.1",
  project = book_directory,
  checkpointLocation = checkpoint_directory,
  scanForPackages = FALSE,
  scan.rnw.with.knitr = TRUE, use.knitr = TRUE)

library(knitr)
library(ggplot2)
library(cowplot)
library(lattice)
library(viridis)
library(VIM)

library(mice)
library(micemd)
library(parallel)

library(data.table)
library(xtable)
library(JWileymisc) # has data

options(width = 70, digits = 2)


## ----cache=FALSE---------------------------------------------------------

## load example dataset
data("aces_daily")
draw <- as.data.table(aces_daily)[order(UserID)]
davg <- na.omit(draw[, .(
  Female = na.omit(Female)[1],
  Age = na.omit(Age)[1],
  SES_1 = na.omit(SES_1)[1],
  EDU = na.omit(EDU)[1],
  STRESS = mean(STRESS, na.rm = TRUE),
  SUPPORT = mean(SUPPORT, na.rm = TRUE),
  PosAff = mean(PosAff, na.rm = TRUE),
  NegAff = mean(NegAff, na.rm = TRUE)),
  by = UserID])


## ------------------------------------------------------------------------

## missing depending on support and stress
davg[, MissingProb := ifelse(
         SUPPORT < 5,
           ifelse(STRESS > 2.5, .4, .0),
           ifelse(STRESS > 2.5, 0, .4))]

set.seed(1234)
davgmiss <- copy(davg)
davgmiss[, PosAff := ifelse(rbinom(
             .N, size = 1, prob = MissingProb) == 1,
             NA, PosAff)]
davgmiss[, NegAff := ifelse(rbinom(
             .N, size = 1, prob = MissingProb) == 1,
             NA, NegAff)]
## random missingness on stress and support
davgmiss[, STRESS := ifelse(rbinom(
             .N, size = 1, prob = .1) == 1,
             NA, STRESS)]
davgmiss[, SUPPORT := ifelse(rbinom(
             .N, size = 1, prob = .1) == 1,
             NA, SUPPORT)]
davgmiss[, Age := ifelse(rbinom(
             .N, size = 1, prob = .1) == 1,
             NA, Age)]
davgmiss[, SES_1 := ifelse(rbinom(
             .N, size = 1, prob = .1) == 1,
             NA, SES_1)]
davgmiss[, Female := factor(ifelse(rbinom(
             .N, size = 1, prob = .1) == 1,
             NA, Female), levels = 0:1,
             labels = c("Male", "Female"))]
davgmiss[, EDU := factor(ifelse(rbinom(
             .N, size = 1, prob = .1) == 1,
             NA, EDU), levels = 0:1,
             labels = c("< Uni Graduate", "Uni Graduate +"))]
## drop unneeded variables to make analysis easier
davgmiss[, MissingProb := NULL]
davgmiss[, UserID := NULL]


## ----fmd-missplot, fig.width=12, fig.height=12/1.1, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Visual summary of the missingness by variable and missingness patterns."----

aggr(davgmiss, prop = TRUE,
     numbers = TRUE)


## ----fmd-missbi, fig.width=9, fig.height=9, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Bivariate plots of missing data. Central dots show the non-missing data. Margin dots show missing data. Boxplots summarize each variable by whether the other variable is missing or present."----

par(mfrow = c(2, 2))
marginplot(davgmiss[,.(STRESS, NegAff)])
marginplot(davgmiss[,.(SUPPORT, NegAff)])
marginplot(davgmiss[,.(STRESS, PosAff)])
marginplot(davgmiss[,.(SUPPORT, PosAff)])


## ------------------------------------------------------------------------

## does age differ by missing on negative affect?
t.test(Age ~ is.na(NegAff), data = davgmiss)$p.value

## does age differ by missing on positive affect?
t.test(Age ~ is.na(PosAff), data = davgmiss)$p.value

## does stress differ by missing on negative affect?
t.test(STRESS ~ is.na(NegAff), data = davgmiss)$p.value

## does stress differ by missing on positive affect?
t.test(STRESS ~ is.na(PosAff), data = davgmiss)$p.value

## does social support differ by missing on negative affect?
t.test(SUPPORT ~ is.na(NegAff), data = davgmiss)$p.value

## does social support differ by missing on positive affect?
t.test(SUPPORT ~ is.na(PosAff), data = davgmiss)$p.value


## ------------------------------------------------------------------------

system.time(mi.1 <- mice(
  davgmiss,
  m = 6,   maxit = 10,
  defaultMethod = c("norm", "logreg", "polyreg", "polr"),
  seed = 1234, printFlag = FALSE)
)


## ----fmd-micediag, fig.width=7, fig.height=9, out.width='1\\linewidth', fig.pos="!h", fig.cap = c("mice diagnostics for convergence", "mice diagnostics for convergence after more iterations")----

## plot convergence diagnostics
plot(mi.1, PosAff + NegAff + SUPPORT ~ .it | .ms)

## run an additional iterations
system.time(mi.1 <- mice.mids(
  mi.1, maxit = 10,
  printFlag = FALSE)
)

## plot convergence diagnostics
plot(mi.1, PosAff + NegAff + SUPPORT ~ .it | .ms)


## ----fmd-micediag2, fig.width=7, fig.height=7, out.width='1\\linewidth', fig.pos="!h", fig.cap = c("Univariate density plots for observed and imputed data, separated by imputation.", "Bivariate scatter plots with imputed data colored separately by observed and imputed data.")----

densityplot(mi.1, ~ PosAff + NegAff + SUPPORT + STRESS)

xyplot(mi.1, NegAff + PosAff ~ STRESS + SUPPORT)


## ------------------------------------------------------------------------

lm.1 <- with(mi.1, lm(PosAff ~ STRESS + Age + EDU + Female))

lm.1


## ----fmd-modeldiag, fig.width=6, fig.height=6, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Linear regression model diagnostics from first imputed dataset.")----

par(mfcol = c(2,2 ))
plot(lm.1$analyses[[1]])
par(mfcol = c(1,1))


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(summary(pool(lm.1), conf.int=TRUE),
##   digits = 2,
##   caption = "Regression results pooled across multiply imputed data",
##   label = "tmd-pooledres1")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(summary(pool(lm.1), conf.int=TRUE),
  digits = 2,
  caption = "Regression results pooled across multiply imputed data",
  label = "tmd-pooledres1")

## ------------------------------------------------------------------------

pool.r.squared(lm.1)


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(summary(pool(lm.1), type = "all", conf.int=TRUE),
##   digits = 2,
##   caption = "Regression results pooled across multiply imputed data with additional information",
##   label = "tmd-pooledres1alt")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(summary(pool(lm.1), type = "all", conf.int=TRUE),
  digits = 2,
  caption = "Regression results pooled across multiply imputed data with additional information",
  label = "tmd-pooledres1alt")

## ----fmd-predhat, fig.width=5, fig.height=4, out.width='.5\\linewidth', fig.pos="!h", fig.cap = c("Pooled predictions from linear regression models of the association between stress and positive affect.")----

newdat <- data.frame(
  STRESS = seq(from = 0, to = 6, length.out = 100),
  Age = mean(davg$Age),
  EDU = factor("< Uni Graduate", levels = levels(davgmiss$EDU)),
  Female = factor("Female", levels = levels(davgmiss$Female)))

newdat$PosAff <- rowMeans(sapply(1:6, function(i) {
  predict(lm.1$analyses[[i]], newdata = newdat)
}))

ggplot(newdat, aes(STRESS, PosAff)) +
  geom_line()


## ------------------------------------------------------------------------

cl <- makeCluster(2)
clusterExport(cl, c("book_directory", "checkpoint_directory" ))

clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2018-09-28", R.version = "3.5.1",
  project = book_directory,
  checkpointLocation = checkpoint_directory,
  scanForPackages = FALSE,
  scan.rnw.with.knitr = TRUE, use.knitr = TRUE)
  library(mice)
  library(randomForest)
  library(data.table)
})

imputation_seeds <- c(
  403L, 2L, 2118700268L, 1567504751L,
  -161759579L, -1822093220L)

clusterExport(cl, c("davgmiss", "imputation_seeds"))


## ------------------------------------------------------------------------

system.time(mi.par <- parLapplyLB(cl, 1:6, function(i) {
mice(
  davgmiss,
  m = 1,   maxit = 20,
  defaultMethod = c("norm", "logreg", "polyreg", "polr"),
  seed = imputation_seeds[i])
}))


## ------------------------------------------------------------------------

## combine the separate imputations into a single object
mi.par2 <- ibind(mi.par[[1]], mi.par[[2]])
for (i in 3:6) {
  mi.par2 <- ibind(mi.par2, mi.par[[i]])
}

mi.par2


## ----fmd-rfdiag, fig.width=7, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Convergence diagnostics for random forest imputation model.", "Density plots of observed and imputed values from random forest model.", "Scatter plots of affect versus stress and social support for observed and imputed values.")----

system.time(mi.rfpar <- parLapplyLB(cl, 1:6, function(i) {
  mice(
    davgmiss,
    m = 1, maxit = 30,
    method = "rf",
    seed = imputation_seeds[i],
    ntree = 500, nodesize = 10)
}))

## combine into a single object
mi.rf <- ibind(mi.rfpar[[1]], mi.rfpar[[2]])
for (i in 3:6) {
  mi.rf <- ibind(mi.rf, mi.rfpar[[i]])
}

## plot convergence diagnostics
plot(mi.rf, PosAff + NegAff + SUPPORT ~ .it | .ms)

## model diagnostics
densityplot(mi.rf, ~ PosAff + NegAff + SUPPORT + STRESS)

xyplot(mi.rf, NegAff + PosAff ~ STRESS + SUPPORT)


## ----fmd-micompare, fig.width=7, fig.height=5, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Pooled predictions from linear regression models of the association between stress and positive affect.")----

m.true <- lm(PosAff ~ STRESS + Age + EDU + Female, data = davg)
m.cc <- lm(PosAff ~ STRESS + Age + EDU + Female, data = davgmiss)
m.mireg <- summary(pool(with(mi.1,
  lm(PosAff ~ STRESS + Age + EDU + Female))),
  conf.int = TRUE)
m.mirf <- summary(pool(with(mi.rf,
  lm(PosAff ~ STRESS + Age + EDU + Female))),
  conf.int = TRUE)

res.true <- as.data.table(cbind(coef(m.true), confint(m.true)))
res.cc <- as.data.table(cbind(coef(m.cc), confint(m.cc)))
res.mireg <- as.data.table(m.mireg[, c("estimate", "2.5 %", "97.5 %")])
res.mirf <- as.data.table(m.mirf[, c("estimate", "2.5 %", "97.5 %")])
setnames(res.true, c("B", "LL", "UL"))
setnames(res.cc, c("B", "LL", "UL"))
setnames(res.mireg, c("B", "LL", "UL"))
setnames(res.mirf, c("B", "LL", "UL"))

res.compare <- rbind(
  cbind(Type = "Truth", Param = names(coef(m.true)), res.true),
  cbind(Type = "CC", Param = names(coef(m.true)), res.cc),
  cbind(Type = "MI Reg", Param = names(coef(m.true)), res.mireg),
  cbind(Type = "MI RF", Param = names(coef(m.true)), res.mirf))

ggplot(res.compare, aes(factor(""),
   y = B, ymin = LL, ymax = UL, colour = Type)) +
  geom_pointrange(position = position_dodge(.4)) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Param, scales = "free") +
  theme(
    legend.position = c(1, 0),
    legend.justification = c("right", "bottom"))

## clean up cluster
stopCluster(cl)
rm(cl)


## ------------------------------------------------------------------------

cl <- makeCluster(2)
clusterExport(cl, c("book_directory", "checkpoint_directory" ))

clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2018-09-28", R.version = "3.5.1",
  project = book_directory,
  checkpointLocation = checkpoint_directory,
  scanForPackages = FALSE,
  scan.rnw.with.knitr = TRUE, use.knitr = TRUE)
  library(mice)
  library(randomForest)
  library(data.table)
})


## ------------------------------------------------------------------------

## example of how to have R return some seed values
dput(.Random.seed[1:5])

## random seeds
imputation_seeds <- c(403L, 148L, -1767993668L,
  1417792552L, 298386660L, 1360311820L,
1356573822L, -1472988872L, 1215046494L, 759520201L,
1399305648L, -455288776L, 969619279L, 518793662L,
-383967014L, -1983801345L, -698559309L, 1957301883L,
-1457959076L, 1321574932L, -537238757L,
11573466L, 1466816383L, -2113923363L, 1663041018L)

clusterExport(cl, c("davgmiss", "imputation_seeds"))


## ----fmd-rfcdx, fig.width=9, fig.height=6, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Density plots for continuous variables to be included in the imputation model"----

ggplot(melt(davgmiss[, sapply(davgmiss, is.numeric),
              with = FALSE], measure.vars = 1:6), aes(value)) +
         geom_density() + geom_rug() +
         facet_wrap(~variable, scales = "free")


## ------------------------------------------------------------------------

start.time <- proc.time()
mi.rfpar1 <- parLapplyLB(cl, 1:4, function(i) {
  mice(
    davgmiss,
    m = 1, maxit = 5,
    method = "rf",
    seed = imputation_seeds[i],
    ntree = 100, nodesize = 10)
})
stop.time <- proc.time()

## estimate of how long it took
stop.time - start.time

## combine into a single object
mi.rf1 <- ibind(mi.rfpar1[[1]], mi.rfpar1[[2]])
for (i in 3:4) {
  mi.rf1 <- ibind(mi.rf1, mi.rfpar1[[i]])
}


## ----fmd-rfcase1, fig.width=7, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Convergence diagnostics for random forest imputation model.")----

## plot convergence diagnostics
plot(mi.rf1, NegAff + STRESS + Age ~ .it | .ms)


## ----fmd-rfcase2, fig.width=7, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Density plots of observed and imputed values from random forest model.")----

## model diagnostics for continuous study variables
densityplot(mi.rf1, ~ NegAff + STRESS + Age)


## ----fmd-rfcase3, fig.width=7, fig.height=6, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Distribution plot (density and QQ Deviates) for model residuals.")----

## fit the models
fit.mirf1 <- with(mi.rf1,
  lm(NegAff ~ STRESS + Age + EDU + Female + SES_1))

testdistr(unlist(lapply(fit.mirf1$analyses, rstandard)))


## ------------------------------------------------------------------------

## pool results and summarize
m.mirf1 <- summary(pool(fit.mirf1), conf.int = TRUE)


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(m.mirf1,
##   digits = 2,
##   caption = "Regression results pooled across multiply imputed data test run",
##   label = "tmd-pooledres2")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(m.mirf1,
  digits = 2,
  caption = "Regression results pooled across multiply imputed data test run",
  label = "tmd-pooledres2")

## ----fmd-rfcase4, fig.width=7, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Convergence diagnostics for random forest imputation model.", "Density plots of observed and imputed values from random forest model.")----

start.time2 <- proc.time()
mi.rfpar2 <- parLapplyLB(cl, 1:10, function(i) {
  mice(
    davgmiss,
    m = 1, maxit = 30,
    method = "rf",
    seed = imputation_seeds[i],
    ntree = 100, nodesize = 10)
})
stop.time2 <- proc.time()

## time taken
stop.time2 - start.time2

## combine into a single object
mi.rf2 <- ibind(mi.rfpar2[[1]], mi.rfpar2[[2]])
for (i in 3:10) {
  mi.rf2 <- ibind(mi.rf2, mi.rfpar2[[i]])
}


## ----fmd-rfcase5, fig.width=7, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Convergence diagnostics for random forest imputation model.")----

## plot convergence diagnostics
plot(mi.rf2, NegAff + STRESS + Age ~ .it | .ms)


## ----fmd-rfcase6, fig.width=7, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Density plots of observed and imputed values from random forest model.")----

## model diagnostics for continuous study variables
densityplot(mi.rf2, ~ NegAff + STRESS + Age)


## ----fmd-rfcase7, fig.width=7, fig.height=6, out.width='.8\\linewidth', fig.pos="!h", fig.cap = c("Distribution plot (density and QQ Deviates) for model residuals.")----

## fit the models
fit.mirf2 <- with(mi.rf2,
  lm(NegAff ~ STRESS + Age + EDU + Female + SES_1))

testdistr(unlist(lapply(fit.mirf2$analyses, rstandard)))


## ------------------------------------------------------------------------

## pool results and summarize
m.mirf2 <- summary(pool(fit.mirf2), conf.int = TRUE)


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(m.mirf2,
##   digits = 2,
##   caption = "Regression results pooled across multiply imputed data final run",
##   label = "tmd-pooledres3")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(m.mirf2,
  digits = 2,
  caption = "Regression results pooled across multiply imputed data final run",
  label = "tmd-pooledres3")


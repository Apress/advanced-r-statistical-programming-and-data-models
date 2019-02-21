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
library(viridis)
library(JWileymisc)
library(data.table)
library(lme4)
library(lmerTest)
library(chron)
library(zoo)
library(pander)
library(texreg)
library(xtable)
library(splines)
library(parallel)
library(boot)
library(optimx)
library(dfoptim)

options(width = 70, digits = 2)


## ------------------------------------------------------------------------

data(aces_daily)
draw <- as.data.table(aces_daily)
d <- readRDS("aces_daily_sim_processed.RDS")


## ------------------------------------------------------------------------

d[, SOLs30 := as.integer(SOLs >= 30)]


## ------------------------------------------------------------------------

m1.glmm <- glmer(SOLs30 ~ BCOPEDis + BWASONs + (1 | UserID),
                 family = binomial(link = logit),
                 data = d, nAGQ = 9)
summary(m1.glmm)


## ------------------------------------------------------------------------

plogis(fixef(m1.glmm)[["(Intercept)"]])


## ------------------------------------------------------------------------

exp(cbind(
  B = fixef(m1.glmm),
  confint(m1.glmm, parm = "beta_", method = "Wald")))


## ------------------------------------------------------------------------

preddat <- as.data.table(expand.grid(
  BCOPEDis = seq(
    from = min(d$BCOPEDis, na.rm=TRUE),
    to = max(d$BCOPEDis, na.rm = TRUE),
    length.out = 1000),
  BWASONs = quantile(d$BWASONs, probs = c(.2, .8),
                     na.rm = TRUE)))

## predictions based on average random effects
preddat$yhat <- predict(m1.glmm,
  newdata = preddat,
  type = "response",
  re.form = ~ 0)


## ------------------------------------------------------------------------

preddat2 <- as.data.table(expand.grid(
  UserID = unique(d$UserID),
  BCOPEDis = seq(
    from = min(d$BCOPEDis, na.rm=TRUE),
    to = max(d$BCOPEDis, na.rm = TRUE),
    length.out = 1000),
  BWASONs = quantile(d$BWASONs, probs = c(.2, .8),
                     na.rm = TRUE)))

## predictions based on average random effects
preddat2$yhat <- predict(m1.glmm,
  newdata = preddat2,
  type = "response",
  re.form = NULL)


## ------------------------------------------------------------------------

## calculate predicted probabilities
## averaging across participants
preddat3 <- preddat2[, .(yhat = mean(yhat)),
         by = .(BCOPEDis, BWASONs)]


## ----fglmma-probs1, fig.width=10, fig.height=5, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Graph of the predicted probabilities setting random effects to zero (average on the logit scale) and averaging all of them."----

ggplot(rbind(
  cbind(preddat, Type = "Zero"),
  cbind(preddat3, Type = "Average")),
  aes(BCOPEDis, yhat, colour = Type)) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~ round(BWASONs, 1)) +
  theme(
    legend.key.width = unit(1, "cm"),
    legend.position = c(.1, .9)) +
  xlab("Average disengagement coping") +
  ylab("Probability of sleep onset latency 30+ min") +
  coord_cartesian(
    xlim = c(1, 4),
    ylim = c(0, .6),
    expand = FALSE)



## ----fglmma-probs2, fig.width=10, fig.height=5, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Graph of the individual predicted probabilities across levels of disengagement coping and average number of awakenings."----

ggplot(preddat2,
  aes(BCOPEDis, yhat, group = UserID)) +
  geom_line(alpha = .2) +
  facet_wrap(~ round(BWASONs, 1))+
  xlab("Average disengagement coping") +
  ylab("Probability of sleep onset latency 30+ min") +
  coord_cartesian(
    xlim = c(1, 4),
    ylim = c(0, 1),
    expand = FALSE)



## ------------------------------------------------------------------------

m2.glmm <- glmer(SOLs30 ~ BPosAff + WPosAff +
  (1 + WPosAff | UserID),
  family = binomial(link = logit),
  data = d, nAGQ = 1)

summary(m2.glmm)


## ------------------------------------------------------------------------

exp(cbind(
  B = fixef(m2.glmm),
  confint(m2.glmm, parm = "beta_", method = "Wald")))


## ------------------------------------------------------------------------

bpa.low <- quantile(d$BPosAff, probs = .2, na.rm=TRUE)
bpa.high <- quantile(d$BPosAff, probs = .8, na.rm=TRUE)

preddat4.low <- as.data.table(expand.grid(
  UserID = unique(d$UserID),
  WPosAff = seq(
    from = min(d[BPosAff <= bpa.low]$WPosAff,
               na.rm = TRUE),
    to = max(d[BPosAff <= bpa.low]$WPosAff,
             na.rm = TRUE),
    length.out = 1000),
  BPosAff = bpa.low))

preddat4.high <- as.data.table(expand.grid(
  UserID = unique(d$UserID),
  WPosAff = seq(
    from = min(d[BPosAff >= bpa.high]$WPosAff,
               na.rm = TRUE),
    to = max(d[BPosAff >= bpa.high]$WPosAff,
             na.rm = TRUE),
    length.out = 1000),
  BPosAff = bpa.high))

preddat4 <- rbind(
  preddat4.low,
  preddat4.high)

## predictions including random effects
preddat4$yhat <- predict(m2.glmm,
  newdata = preddat4,
  type = "response",
  re.form = NULL)

## calculate predicted probabilities
## averaging across participants
preddat4b <- preddat4[, .(yhat = mean(yhat)),
         by = .(WPosAff, BPosAff)]


## ----fglmma-probs3, fig.width=7, fig.height=6, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Graph of the predicted probabilities averaging across individuals accounting for random intercept and slope of within person positive affect."----

ggplot(preddat4b,
  aes(WPosAff, yhat, colour = factor(round(BPosAff, 1)))) +
  geom_line(size = 1) +
  scale_color_viridis("Average\nPositive Affect",
                      discrete = TRUE) +
  theme(
    legend.key.width = unit(1.5, "cm"),
    legend.position = c(.7, .9)) +
  coord_cartesian(
    xlim = c(-4, 4),
    ylim = c(0, .45),
    expand = FALSE) +
  xlab(paste0("Within person positive affect\n",
              "(deviations from own mean)")) +
  ylab("Probability of sleep onset latency 30+ min")


## ------------------------------------------------------------------------

m3.glmm <- glmer(WASONs ~ Age + BornAUS +
  (1 | UserID),
  family = poisson(link = log),
  data = d, nAGQ = 9)

summary(m3.glmm)


## ------------------------------------------------------------------------

exp(cbind(
  B = fixef(m3.glmm),
  confint(m3.glmm, parm = "beta_", method = "Wald")))

## ------------------------------------------------------------------------

preddat5 <- as.data.table(expand.grid(
  UserID = unique(d[!is.na(BornAUS) & !is.na(Age)]$UserID),
  Age = seq(
    from = min(d$Age, na.rm=TRUE),
    to = max(d$Age, na.rm = TRUE),
    length.out = 1000),
  BornAUS = 0:1))

## predictions based on average random effects
preddat5$yhat <- predict(m3.glmm,
  newdata = preddat5,
  type = "response",
  re.form = NULL)

## calculate predicted counts
## averaging across participants
preddat5 <- preddat5[, .(yhat = mean(yhat)),
         by = .(Age, BornAUS)]


## ----fglmma-probs5, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Graph of the average predicted counts of wakenings after sleep onset by age and whether people were born in Australia or not."----

ggplot(preddat5,
  aes(Age, yhat, colour = factor(BornAUS))) +
  geom_line(size = 2) +
  scale_colour_viridis("Born in Australia", discrete = TRUE) +
  xlab("Age (years)") +
  ylab("Predicted # wakenings after sleep onset") +
  theme(
    legend.key.width = unit(1.5, "cm"),
    legend.position = c(.1, .9)) +
  coord_cartesian(
    xlim = c(18, 26.5),
    ylim = c(0, 2),
    expand = FALSE)


## ------------------------------------------------------------------------

m3.glmm.nb <- glmer.nb(formula(m3.glmm),
  data = d)


## ------------------------------------------------------------------------

## load R code shipped with lme4 to provide the allFit()
source(system.file("utils", "allFit.R", package="lme4"))
m3.all <- allFit(m3.glmm.nb)


## ------------------------------------------------------------------------

m3.all.sum <- summary(m3.all)

m3.all.sum$fixef
m3.all.sum$llik
m3.all.sum$theta


## ------------------------------------------------------------------------

screenreg(
  list(Poisson = m3.glmm,
       NegBin = m3.glmm.nb))


## ------------------------------------------------------------------------

exp(cbind(
  fixef(m3.glmm),
  confint(m3.glmm, parm = "beta_", method = "Wald"),
  fixef(m3.glmm.nb),
  confint(m3.glmm.nb, parm = "beta_", method = "Wald")))


## ------------------------------------------------------------------------

theta <- getME(m3.glmm.nb, "glmer.nb.theta")

density <- data.table(
  X = as.integer(names(table(d$WASONs))),
  Observed = as.vector(prop.table(table(d$WASONs))))

density$NegBin <- colMeans(do.call(rbind, lapply(fitted(m3.glmm.nb), function(mu) {
  dnbinom(density$X, size = theta, mu = mu)
  })))

density$Poisson <- colMeans(do.call(rbind, lapply(fitted(m3.glmm), function(mu) {
  dpois(density$X, lambda = mu)
  })))


## ----fglmma-pnbdens, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Observed and expected average densities of number of awakenings at night based on a poisson and negative binomial GLMM."----

ggplot(melt(density, id.vars = "X"),
  aes(X, value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_fill_viridis("Type", discrete = TRUE) +
  theme(legend.position = c(.8, .8)) +
  xlab("Number of awakenings") +
  ylab("Density") +
  coord_cartesian(
    xlim = c(-.5, 4.5),
    ylim = c(0, .5),
    expand = FALSE)


## ------------------------------------------------------------------------

getME(m3.glmm.nb, "glmer.nb.theta")


## ----fglmma-wasodist, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Distribution of within person deviations from typical number of awakenings after sleep onset lagged to the previous day."----

testdistr(d[, WWASONsLag1],
          varlab = "Within WASONs lag 1")


## ------------------------------------------------------------------------

m4.glmm <- glmer(WASONs ~ Age + BornAUS +
   WWASONsLag1 +
  (1 + WWASONsLag1  | UserID),
  family = poisson(link = log),
  data = d, nAGQ = 1)

summary(m4.glmm)


## ------------------------------------------------------------------------

exp(cbind(
  B = fixef(m4.glmm),
  confint(m4.glmm, parm = "beta_", method = "Wald")))


## ----fglmma-wasodist2, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "For nearly all people, when they have more than usual awakenings one night, they tend to have fewer awakenings the following night."----

testdistr(exp(coef(m4.glmm)$UserID$WWASONsLag1))


## ------------------------------------------------------------------------

preddat.boot <- as.data.table(expand.grid(
  UserID = unique(model.frame(m4.glmm)$UserID),
  WWASONsLag1 = seq(
    from = min(d$WWASONsLag1, na.rm = TRUE),
    to = max(d$WWASONsLag1, na.rm = TRUE),
    length.out = 100),
  Age = quantile(d[!duplicated(UserID)]$Age,
                 probs = c(.2, .8), na.rm = TRUE),
  BornAUS = 0:1))

preddat.boot$yhat <- predict(m4.glmm,
  newdata = preddat.boot)



## ------------------------------------------------------------------------

genPred <- function(m) {
  predict(m,
    newdata = preddat.boot)
}

cl <- makeCluster(4)
clusterExport(cl, c("book_directory",
                    "checkpoint_directory",
                    "preddat.boot", "d", "genPred"))

clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2018-09-28", R.version = "3.5.1",
    project = book_directory,
    checkpointLocation = checkpoint_directory,
    scanForPackages = FALSE,
    scan.rnw.with.knitr = TRUE, use.knitr = TRUE)

  library(data.table)
  library(lme4)
  library(lmerTest)
})



## ------------------------------------------------------------------------

system.time(bootres <- bootMer(m4.glmm,
    FUN = genPred,
    nsim = 100,
    seed = 12345,
    use.u = FALSE,
    type = "parametric",
    parallel = "snow",
    ncpus = 4,
    cl = cl))


## ------------------------------------------------------------------------

preddat.boot[, Index := rep(1L:400L,
  each = length(unique(UserID)))]


## ------------------------------------------------------------------------

preddat.boot.avg <- preddat.boot[, .(yhat = mean(exp(yhat))),
  by = .(WWASONsLag1, Age, BornAUS)]


## ------------------------------------------------------------------------

dim(bootres$t)

for (i in 1:400) {
  ## find which indices to use
  ok <- which(preddat.boot$Index == i)

  ## now average across people
  tmp_avg <- rowMeans(exp(bootres$t[, ok]))

  ## lower confidence interval
  preddat.boot.avg[i,
    LL := quantile(tmp_avg, probs = .025, na.rm = TRUE)]
  preddat.boot.avg[i,
    UL := quantile(tmp_avg, probs = .975, na.rm = TRUE)]
}


## ----fglmml-poispredboot, fig.width = 10, fig.height = 6, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Plot of predicted number of awakenings by previous night awakenings (relative to own average), separated by age (years) at the 20th and 80th percentiles (19.4y and 25y, respectively) and born in Australia (0 = no, 1 = yes). Bootstrap confidence intervals around the mean predicted count are show through shading."----

ggplot(preddat.boot.avg, aes(WWASONsLag1, yhat,
  colour = factor(BornAUS), fill = factor(BornAUS))) +
  geom_ribbon(aes(ymin = LL, ymax = UL),
              alpha = .25, colour = NA) +
  geom_line(size = 1) +
  ylab("Predicted Awakenings") +
  xlab("Within person awakenings lag 1") +
  scale_color_viridis("Born in Australia", discrete = TRUE) +
  scale_fill_viridis("Born in Australia", discrete = TRUE) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "cm")) +
  facet_wrap(~ Age) +
  coord_cartesian(
   xlim = c(-3, 3),
   ylim = c(0, 2.5),
   expand = FALSE)



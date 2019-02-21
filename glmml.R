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

options(width = 70, digits = 2)


## ----fglmml-diagmat, fig.width=4, fig.height=4, out.width='.4\\linewidth', fig.pos="!h", fig.cap = "Graph of block diagonal dummy code matrix of UserID.  Black values indicate rows of the data that belong to a particular participant. Different columns represent different participants."----

data(aces_daily)
draw <- as.data.table(aces_daily)
d <- readRDS("aces_daily_sim_processed.RDS")

mat <- model.matrix(~ 0 + factor(UserID), data = d)

image(t(mat[1:300, 1:10]), col = c("white", "black"),
      xlab = "Participants", ylab = "Observation",
      xaxt = "n", yaxt = "n")


## ----echo=FALSE, results='asis', listings=FALSE--------------------------

m.glm <- lm(PosAff ~ 1, data = d)
m.glmm <- lmer(PosAff ~ 1 + (1 | UserID), data = d)

m.glmm.sigma <- VarCorr(m.glmm)$UserID[1,1]
m.glmm.lower <- sprintf("%0.2f - %0.2f = %0.2f",
   fixef(m.glmm)[[1]],
   m.glmm.sigma,
   fixef(m.glmm)[[1]] - m.glmm.sigma)

m.glmm.upper <- sprintf("%0.2f + %0.2f = %0.2f",
   fixef(m.glmm)[[1]],
   m.glmm.sigma,
   fixef(m.glmm)[[1]] + m.glmm.sigma)

texreg(list(
  GLM = extract(m.glm),
  GLMM = extract(m.glmm)),
  label = "tglmml-glmminttex", float.pos = "!hb")


## ----echo=FALSE----------------------------------------------------------
cat(m.glmm.lower)
cat(m.glmm.upper)

## ------------------------------------------------------------------------

m <- lmer(NegAff ~ 1 + (1 | UserID), data = d)
cor(na.omit(d$NegAff), fitted(m))^2


## ----message=FALSE, warning=FALSE, error=FALSE---------------------------

m <- lmer(NegAff ~ 1 + (1 | UserID), data = d)
R2LMER(m, summary(m))

iccMixed("NegAff", "UserID", d)


## ------------------------------------------------------------------------

## data setup
d[,
  SurveyDayCount := as.integer(SurveyDay - min(SurveyDay)),
  by = UserID]

## setup mini dataset
tmpd <- d[!is.na(SOLs) & !is.na(SurveyDayCount),
  .(SOLs, SurveyDayCount, UserID)]

## fixed effects, all people
mreg <- lm(SOLs ~ 1 + SurveyDayCount, data = tmpd)
## add predictions to the dataset
tmpd[, Fixed := predict(mreg, newdata = tmpd)]


## ------------------------------------------------------------------------

## fixed effects, individual models
tmpd[, Individual := fitted(lm(SOLs ~ 1 +
  offset(coef(mreg)[2] * SurveyDayCount))),
  by = UserID]


## ------------------------------------------------------------------------

## random intercept model, all people
m <- lmer(SOLs ~ 1 + + SurveyDayCount + (1 | UserID), data = tmpd)
## add predictions to the dataset
tmpd[, Random := predict(m, newdata = tmpd)]


## ----fglmml-intvis1, fig.width=7, fig.height=4, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plot of estimated trajectory from indiviudal regresion models and a random intercept model. Population average shown in blue. While all lines have the same slope, the intercepts of each line are closer to the population average for the Random model than the Individual models, showing the effect of shrinkage."----

## select a few example IDs to plot
tmpdselect <- melt(tmpd[UserID %in% unique(UserID)[107:115]],
     id.vars = c("UserID", "SurveyDayCount", "SOLs"))

ggplot(tmpdselect[variable != "Fixed"],
       aes(SurveyDayCount, value, group = UserID)) +
  geom_abline(intercept = coef(mreg)[1], slope = coef(mreg)[2],
              size = 2, colour = "blue") +
  geom_line() +
  facet_wrap(~ variable)


## ----fglmml-intvis2, fig.width=8, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plots of different model estimated lines against raw data values for 9 participants."----

## plots against individual data
ggplot(tmpdselect, aes(SurveyDayCount)) +
  geom_point(aes(y = SOLs), size = 1) +
  geom_line(aes(y = value,
                colour = variable,
                linetype = variable), size = 1.5) +
  facet_wrap(~UserID, scales = "free_y") +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(2, "cm"))


## ----fglmml-intvis3, fig.width=8, fig.height=6.5, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plot of estimated intercept in the individual models and random effects model for each participant, with arrows showing the shrinkage towards the population mean intercept."----

tmpd <- tmpd[SurveyDayCount==0][order(Individual)]
tmpd[, UserID := factor(UserID, levels = UserID)]

ggplot(tmpd, aes(x = Individual, xend = Random,
                 y = UserID, yend = UserID)) +
  geom_segment(
    arrow = arrow(length = unit(0.01, "npc"))) +
  geom_vline(xintercept = tmpd[SurveyDayCount==0][1, Fixed]) +
  xlab("Estimated Intercept") +
  theme(axis.text.y = element_blank())


## ----fglmml-glmmdiag1, fig.width=10, fig.height=8, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random intercept (bottom left), and a simple single level generalized additive model smooth of the association between survey day and sleep onset latency to assess for non-linearity (bottom right).", warning = FALSE----

assumptiontests <- plotDiagnosticsLMER(m, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot,
  list(ggplot(d, aes(SurveyDayCount, SOLs)) +
       stat_smooth()),
  ncol = 2))


## ----fglmml-glmmdiag2, fig.width=10, fig.height=8, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random intercept (bottom left), and a simple single level generalized additive model smooth of the association between survey day and sleep onset latency to assess for non-linearity (bottom right).", warning = FALSE----

d[, sqrtSOLs := sqrt(SOLs)]
m2 <- lmer(sqrtSOLs ~ SurveyDayCount + (1 | UserID),
           data = d)

assumptiontests <- plotDiagnosticsLMER(m2, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot,
  list(ggplot(d, aes(SurveyDayCount, sqrtSOLs)) +
       stat_smooth()),
  ncol = 2))


## ------------------------------------------------------------------------

summary(m2)


## ------------------------------------------------------------------------

system.time(
  ci.wald <- confint(m2,
   method = "Wald", oldNames = FALSE))
system.time(
  ci.profile <- confint(m2,
   method = "profile", oldNames = FALSE))
system.time(
  ci.boot <- confint(m2,
   method = "boot", oldNames = FALSE,
   nsim = 200, seed = 1234))

ci.compare <- data.table(
  Param = rownames(ci.wald),
  Wald = sprintf("%0.2f, %0.2f",
    ci.wald[,1], ci.wald[,2]),
  Profile = sprintf("%0.2f, %0.2f",
    ci.profile[,1], ci.profile[,2]),
  Boot = sprintf("%0.2f, %0.2f",
    ci.boot[,1], ci.boot[,2]))

print(ci.compare)


## ------------------------------------------------------------------------

testm2 <- detailedTests(m2, method = "Wald")
formatLMER(list(testm2))


## ------------------------------------------------------------------------

testm2b <- detailedTests(m2, method = "profile")
formatLMER(list(testm2b))


## ------------------------------------------------------------------------

## setup dataset
tmpd <- d[!is.na(sqrtSOLs) & !is.na(SurveyDayCount),
  .(sqrtSOLs, SurveyDayCount, UserID)]

## fixed effects, all people
mreg <- lm(sqrtSOLs ~ 1 + SurveyDayCount, data = tmpd)
## add predictions to the dataset
tmpd[, Fixed := predict(mreg, newdata = tmpd)]


## ------------------------------------------------------------------------

## fixed effects, individual models
tmpd[, Individual := fitted(lm(sqrtSOLs ~ 1 + SurveyDayCount)),
  by = UserID]


## ------------------------------------------------------------------------

## random intercept model, all people
m <- lmer(sqrtSOLs ~ 1 + SurveyDayCount +
          (1 + SurveyDayCount | UserID), data = tmpd)
## add predictions to the dataset
tmpd[, Random := predict(m, newdata = tmpd)]


## ----fglmml-slopevis1, fig.width=7, fig.height=4, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plot of estimated trajectory from indiviudal regresion models and a random intercept model. Population average shown in blue. The random model pulls both the intercepts and slopes closer to the population average intercept and slope, showing the effect of shrinkage."----

## select a few example IDs to plot
tmpdselect <- melt(tmpd[UserID %in% unique(UserID)[107:115]],
     id.vars = c("UserID", "SurveyDayCount", "sqrtSOLs"))

ggplot(tmpdselect[variable != "Fixed"],
       aes(SurveyDayCount, value, group = UserID)) +
  geom_abline(intercept = coef(mreg)[1], slope = coef(mreg)[2],
              size = 2, colour = "blue") +
  geom_line() +
  facet_wrap(~ variable)


## ----fglmml-slopevis2, fig.width=8, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plots of different model estimated lines against raw data values for 9 participants."----

## plots against individual data
ggplot(tmpdselect, aes(SurveyDayCount)) +
  geom_point(aes(y = sqrtSOLs), size = 1) +
  geom_line(aes(y = value,
                colour = variable,
                linetype = variable), size = 1.5) +
  facet_wrap(~UserID, scales = "free_y") +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(2, "cm"))


## ----fglmml-slopevis3, fig.width=8, fig.height=6.5, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plot of estimated slope in the individual models and random effects model for each participant, with arrows showing the shrinkage towards the population mean slope."----

tmpd <- d[, .(
  Individual = coef(lm(
    sqrtSOLs ~ 1 + SurveyDayCount))[2]),
  by = UserID]

## estimated random slope is deviation + average
tmpd$Random <- ranef(m)$UserID[, "SurveyDayCount"] + fixef(m)[2]
tmpd <- tmpd[order(Individual)]
tmpd[, UserID := factor(UserID, levels = UserID)]

ggplot(tmpd, aes(x = Individual, xend = Random,
                 y = UserID, yend = UserID)) +
  geom_segment(
    arrow = arrow(length = unit(0.01, "npc"))) +
  geom_vline(xintercept = coef(mreg)[2]) +
  xlab("Estimated Slope") +
  theme(axis.text.y = element_blank())


## ------------------------------------------------------------------------

between_data <- cbind(
  d[, .(
  BWASONs = na.omit(BWASONs)[1]),
  by = UserID][order(UserID)],
  coef(m)$UserID)


## ------------------------------------------------------------------------

between.int <- lm(`(Intercept)` ~ BWASONs,
                 data = between_data)
between.slope <- lm(SurveyDayCount ~ BWASONs,
             data = between_data)


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## texreg(list(
##   Intercept = between.int,
##   Slope = between.slope),
##   digits = 3,
##   label = "tglmml-blups",
##   float.pos = "!hb")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
texreg(list(
  Intercept = between.int,
  Slope = between.slope),
  digits = 3,
  label = "tglmml-blups",
  float.pos = "!hb")

## ----fglmml-caterpillar, fig.width=10, fig.height=6, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Dot plots of estimated random effects with approximate confidence intervals.."----

ggplot(as.data.frame(ranef(m, condVar = TRUE)),
 aes(grp, condval,
     ymin = condval - 2 * condsd,
     ymax = condval + 2 * condsd)) +
 geom_pointrange(size = .2) +
 facet_wrap(~ term, scales = "free_x") +
 coord_flip() +
 theme(axis.text.y = element_blank(),
       axis.ticks.y = element_blank()) +
  ylab("Random effect + uncertainty") +
  xlab("Participant ID")


## ------------------------------------------------------------------------

me.prediction <- lmer(sqrtSOLs ~
   SurveyDayCount + BWASONs +
   SurveyDayCount:BWASONs +
   (1 + SurveyDayCount | UserID),
  data = d)


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## texreg(list(
##   Intercept = extract(between.int),
##   Slope = extract(between.slope),
##   Random = extract(me.prediction)),
##   digits = 3,
##   label = "tglmml-blupsme",
##   float.pos = "!hb")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
texreg(list(
  Intercept = extract(between.int),
  Slope = extract(between.slope),
  Random = extract(me.prediction)),
  digits = 3,
  label = "tglmml-blupsme",
  float.pos = "!hb")

## ------------------------------------------------------------------------

## mixed effects, with random intercept by ID
m.lmm <- lmer(PosAff ~ 1 + (1 | UserID), data = d)
summary(m.lmm)


## ------------------------------------------------------------------------

## fixed effects only, GLM
m.lm <- lm(PosAff ~ 1, data = d)
summary(m.lm)

## nice side by side comparison
screenreg(list(
  GLM = extract(m.lm),
  GLMM = extract(m.lmm)))


## ------------------------------------------------------------------------

## mixed effects, with random intercept by ID
m2.lmm <- lmer(PosAff ~ 1 + BSTRESS + (1 | UserID), data = d)
summary(m2.lmm)


## ----fglmml-diagex1, fig.width=10, fig.height=8, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random intercept (bottom left), and a simple, single level generalized additive model smooth of the association between average stress and positive to assess for non-linearity (bottom right).", warning = FALSE----

assumptiontests <- plotDiagnosticsLMER(m2.lmm, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot,
  list(ggplot(d, aes(BSTRESS, PosAff)) +
       stat_smooth()),
  ncol = 2))


## ----fglmml-splines1, fig.width = 7, fig.height = 7, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Plot of average stress versus positive affect using B-splines with 3 degrees of freedom, 10 degrees of freedom, and a generalized additive model."----

ggplot(d, aes(BSTRESS, PosAff)) +
  stat_smooth(method = "lm",
              formula = y ~ bs(x, df = 3),
              colour = viridis(3)[1]) +
  stat_smooth(method = "lm",
              formula = y ~ bs(x, df = 10),
              colour = viridis(3)[2]) +
  stat_smooth(colour = viridis(3)[3])


## ------------------------------------------------------------------------

## mixed effects model
m3.lmm <- lmer(PosAff ~ 1 + bs(BSTRESS, df = 3) +
                 (1 | UserID), data = d)
summary(m3.lmm)

## compare the linear and B-spline models
AIC(refitML(m3.lmm), refitML(m2.lmm))


## ------------------------------------------------------------------------

## create the new variable in the dataset
d[, Weekend := factor(as.integer(
      weekdays(SurveyDay) %in% c("Saturday", "Sunday")))]

## update the model adding weekend
m4.lmm <- update(m3.lmm, . ~ . + Weekend)

## screenreg summary
screenreg(m4.lmm)


## ------------------------------------------------------------------------

preddat <- as.data.table(expand.grid(
  BSTRESS = seq(
    from = min(d$BSTRESS, na.rm=TRUE),
    to = max(d$BSTRESS, na.rm=TRUE),
    length.out = 1000),
  Weekend = levels(d$Weekend)))

preddat$yhat <- predict(m4.lmm,
  newdata = preddat,
  re.form = ~ 0)


## ----fglmml-rpred1, fig.width = 7, fig.height = 6, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Plot of predicted positive affect from a model of average stress and weekday vs weekend."----

ggplot(preddat, aes(BSTRESS, yhat, colour = Weekend)) +
  geom_line(size = 1) +
  ylab("Positive Affect") +
  xlab("Average Stress") +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position = c(.75, .8),
    legend.key.width = unit(1, "cm"))


## ------------------------------------------------------------------------

cl <- makeCluster(2)
clusterExport(cl, c("book_directory",
                    "checkpoint_directory",
                    "preddat", "d"))

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
  library(splines)
})

genPred <- function(m) {
  predict(m,
    newdata = preddat,
    re.form = ~0)
}

## ------------------------------------------------------------------------

system.time(
  bootres <- bootMer(m4.lmm,
    FUN = genPred,
    nsim = 1000,
    seed = 12345,
    use.u = FALSE,
    type = "parametric",
    parallel = "snow",
    cl = cl)
)

## calculate percentile bootstrap confidence intervals
## and add to the dataset for plotting
preddat$LL <- apply(bootres$t, 2, quantile, probs = .025)
preddat$UL <- apply(bootres$t, 2, quantile, probs = .975)


## ----fglmml-rpred2, fig.width = 7, fig.height = 6, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Plot of predicted positive affect from a model of average stress and weekday vs weekend with bootstrapped confidence intervals."----

ggplot(preddat, aes(BSTRESS, yhat, colour = Weekend,
                    fill = Weekend)) +
  geom_ribbon(aes(ymin = LL, ymax = UL),
              alpha = .25, colour = NA) +
  geom_line(size = 1) +
  ylab("Positive Affect") +
  xlab("Average Stress") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position = c(.75, .8),
    legend.key.width = unit(1, "cm")) +
  coord_cartesian(xlim = c(0, 8), ylim = c(1, 4),
                  expand = FALSE)


## ------------------------------------------------------------------------

## update the model adding within person stress
m5.lmm <- update(m4.lmm, . ~ . + WSTRESS)

## screenreg summary
screenreg(m5.lmm)


## ------------------------------------------------------------------------

bstress.low <- round(quantile(d[!duplicated(UserID)]$BSTRESS,
                        probs = .25), 1)
bstress.high <- round(quantile(d[!duplicated(UserID)]$BSTRESS,
                         probs = .75), 1)

preddat.low <- as.data.table(expand.grid(
  BSTRESS = bstress.low,
  WSTRESS = seq(
    from = quantile(d[BSTRESS <= bstress.low]$WSTRESS,
               probs = .02, na.rm = TRUE),
    to = quantile(d[BSTRESS <= bstress.low]$WSTRESS,
               probs = .98, na.rm = TRUE),
    length.out = 1000),
  Weekend = factor("1", levels = levels(d$Weekend))))

preddat.high <- as.data.table(expand.grid(
  BSTRESS = bstress.high,
  WSTRESS = seq(
    from = quantile(d[BSTRESS >= bstress.high]$WSTRESS,
               probs = .02, na.rm = TRUE),
    to = quantile(d[BSTRESS >= bstress.high]$WSTRESS,
               probs = .98, na.rm = TRUE),
    length.out = 1000),
  Weekend = factor("1", levels = levels(d$Weekend))))

preddat <- rbind(
  preddat.low,
  preddat.high)

preddat$yhat <- predict(m5.lmm,
  newdata = preddat,
  re.form = ~ 0)

## convert BSTRESS to factor for plotting
preddat$BSTRESS <- factor(preddat$BSTRESS)


## ----fglmml-rpred2b, fig.width = 7, fig.height = 6, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Plot of predicted positive affect from a model of average stress and weekday vs weekend."----

ggplot(preddat, aes(WSTRESS, yhat, colour = BSTRESS)) +
  geom_line(size = 1) +
  ylab("Positive Affect") +
  xlab("Within Stress") +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position = c(.05, .2),
    legend.key.width = unit(1, "cm"))


## ------------------------------------------------------------------------

bstress.low <- round(quantile(d[!duplicated(UserID)]$BSTRESS,
                        probs = .25), 1)
bstress.high <- round(quantile(d[!duplicated(UserID)]$BSTRESS,
                         probs = .75), 1)

preddat.low <- as.data.table(expand.grid(
  UserID = unique(d$UserID),
  BSTRESS = bstress.low,
  WSTRESS = seq(
    from = quantile(d[BSTRESS <= bstress.low]$WSTRESS,
               probs = .02, na.rm = TRUE),
    to = quantile(d[BSTRESS <= bstress.low]$WSTRESS,
               probs = .98, na.rm = TRUE),
    length.out = 1000),
  Weekend = factor("1", levels = levels(d$Weekend))))

preddat.high <- as.data.table(expand.grid(
  UserID = unique(d$UserID),
  BSTRESS = bstress.high,
  WSTRESS = seq(
    from = quantile(d[BSTRESS >= bstress.high]$WSTRESS,
               probs = .02, na.rm = TRUE),
    to = quantile(d[BSTRESS >= bstress.high]$WSTRESS,
               probs = .98, na.rm = TRUE),
    length.out = 1000),
  Weekend = factor("1", levels = levels(d$Weekend))))

preddat <- rbind(
  preddat.low,
  preddat.high)

preddat$yhat <- predict(m5.lmm,
  newdata = preddat,
  re.form = NULL)

## convert BSTRESS to factor for plotting
preddat$BSTRESS <- factor(preddat$BSTRESS)


## ----fglmml-rpred3, fig.width = 10, fig.height = 6, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Plot of predicted positive affect from a model of average stress and weekday vs weekend."----

ggplot(preddat, aes(WSTRESS, yhat, group = UserID)) +
  geom_line(alpha = .2) +
  ylab("Positive Affect") +
  xlab("Within Stress") +
  facet_wrap(~ BSTRESS, ncol = 2) +
  coord_cartesian(
    xlim = c(-4, 5),
    ylim = c(1, 5),
    expand = FALSE)


## ------------------------------------------------------------------------

m6.lmm <- update(m5.lmm, . ~ . - (1 | UserID) +
  (1 + WSTRESS | UserID))

screenreg(m6.lmm)


## ------------------------------------------------------------------------

## convert BSTRESS from factor to numeric for prediction
preddat$BSTRESS <- as.numeric(as.character(
  preddat$BSTRESS))

preddat$yhat2 <- predict(m6.lmm,
  newdata = preddat,
  re.form = NULL)

## convert BSTRESS to factor for plotting
preddat$BSTRESS <- factor(preddat$BSTRESS)


## ----fglmml-rpred4, fig.width = 10, fig.height = 6, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Plot of predicted positive affect from a model of average stress and weekday vs weekend."----

ggplot(preddat, aes(WSTRESS, yhat2, group = UserID)) +
  geom_line(alpha = .2) +
  ylab("Positive Affect") +
  xlab("Within Stress") +
  facet_wrap(~ BSTRESS, ncol = 2) +
  coord_cartesian(
    xlim = c(-4, 5),
    ylim = c(1, 5),
    expand = FALSE)


## ----fglmml-diagex6, fig.width=10, fig.height=15, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random intercept (bottom left), and a simple, single level generalized additive model smooth of the association between within stress and positive affect to assess for non-linearity (bottom right).", warning = FALSE----

assumptiontests <- plotDiagnosticsLMER(m6.lmm, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot,
  list(ggplot(d, aes(WSTRESS, PosAff)) +
       stat_smooth()),
  ncol = 2))


## ------------------------------------------------------------------------

assumptiontests$ExtremeValues[
  EffectType == "Multivariate Random Effect UserID"]


## ------------------------------------------------------------------------

m7.lmm <- update(m6.lmm,
  data = d[UserID != 123],
  REML = FALSE)

screenreg(list(m6.lmm, m7.lmm))


## ----fglmml-diagex7, fig.width=10, fig.height=15, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random intercept (bottom left), and a simple, single level generalized additive model smooth of the association between within stress and positive affect to assess for non-linearity (bottom right).", warning = FALSE----

assumptiontests <- plotDiagnosticsLMER(m7.lmm, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot,
  list(ggplot(d[UserID != 123], aes(WSTRESS, PosAff)) +
       stat_smooth()),
  ncol = 2))


## ------------------------------------------------------------------------

m7.lmmb <- update(m7.lmm, . ~ . - (1 + WSTRESS | UserID) +
  WSTRESS + I(WSTRESS^2) +
  (1 + WSTRESS + I(WSTRESS^2) | UserID))

m7.lmmc <- update(m7.lmm, . ~ . - (1 + WSTRESS | UserID)
  - WSTRESS +
  pmin(WSTRESS, 0) + pmax(WSTRESS, 0) +
  (1 + pmin(WSTRESS, 0) + pmax(WSTRESS, 0) | UserID))

m7.lmmd <- update(update(m7.lmm, . ~ . - WSTRESS), . ~ .
  - (1 + WSTRESS | UserID)  +
  bs(WSTRESS, df = 3) + (1 + bs(WSTRESS, df = 3) | UserID))


## ------------------------------------------------------------------------

AIC(
  m7.lmm,
  m7.lmmb,
  m7.lmmc,
  m7.lmmd)


## ----fglmml-diagex7c, fig.width=10, fig.height=15, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random effects (middle row and bottom left), and a test for multivariate normality of the random effects (bottom right).", warning = FALSE----

assumptiontests <- plotDiagnosticsLMER(m7.lmmc, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot,
  ncol = 2))


## ------------------------------------------------------------------------

## hack
model.frame <- function(obj) {
  d[UserID != 123][
    !is.na(PosAff) & !is.na(BSTRESS) &
    !is.na(WSTRESS) & !is.na(Weekend)]
}
detailedTests <- detailedTests
environment(detailedTests) <- environment()
.detailedTestsLMER <- .detailedTestsLMER
environment(.detailedTestsLMER) <- environment()

## calculate the detailed tests
test.m7.lmmc <- detailedTests(m7.lmmc,
  method = "Wald")

## remove our hack
rm(model.frame, detailedTests,
   .detailedTestsLMER)


## ------------------------------------------------------------------------

effecttable <- formatLMER(list(test.m7.lmmc))


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(effecttable,
##   caption = paste("Final random intercept and slope model",
##     "with a B-spline for average stress and linear piecewise",
##     "model for within person stress."),
##   label = "tglmml-effecttable")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(effecttable,
  caption = paste("Final random intercept and slope model",
    "with a B-spline for average stress and linear piecewise",
    "model for within person stress."),
  label = "tglmml-effecttable")

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(
## formatLMER(
##   list(test.m7.lmmc),
##   format = list(
##     FixedEffects = c("b = %s, %s, CI = (%s; %s)"),
##     RandomEffects = c("%s", "%s (%s; %s)"),
##     EffectSizes = c("Marg f2 = %s; Cond f2 = %s, %s")),
##   pcontrol = list(
##     digits = 3,
##     stars = FALSE,
##     includeP = TRUE,
##     includeSign = TRUE,
##     dropLeadingZero = TRUE)),
##   caption = paste("Different formatting for the final",
##     "random intercept and slope model."),
##   label = "tglmml-effecttablealt")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(
formatLMER(
  list(test.m7.lmmc),
  format = list(
    FixedEffects = c("b = %s, %s, CI = (%s; %s)"),
    RandomEffects = c("%s", "%s (%s; %s)"),
    EffectSizes = c("Marg f2 = %s; Cond f2 = %s, %s")),
  pcontrol = list(
    digits = 3,
    stars = FALSE,
    includeP = TRUE,
    includeSign = TRUE,
    dropLeadingZero = TRUE)),
  caption = paste("Different formatting for the final",
    "random intercept and slope model."),
  label = "tglmml-effecttablealt")


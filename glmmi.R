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

options(width = 70, digits = 2)


## ------------------------------------------------------------------------

ex.wide <- data.table(
  ID = c(1, 2, 3),
  SBPT1 = c(135, 120, 121),
  SBPT2 = c(130, 125, 125),
  SBPT3 = c(125, 121, NA))

print(ex.wide)


reshape(
  data = ex.wide,
  varying = list(paste0("SBPT", 1:3)),
  v.names = c("SBP"),
  idvar = "ID",
  direction = "long")


## ------------------------------------------------------------------------

ex.long <- data.table(
  ID = c(1, 1, 1, 2, 2, 2, 3, 3),
  SBP = c(135, 130, 125, 120, 125, 121, 121, 125),
  Time = c(1, 2, 3, 1, 2, 3, 1, 2))

print(ex.long)

reshape(
  data = ex.long,
  v.names = "SBP",
  timevar = "Time",
  sep = "T",
  idvar = "ID",
  direction = "wide")


## ------------------------------------------------------------------------

data(aces_daily)
str(aces_daily)


## ----cache=FALSE---------------------------------------------------------

draw <- as.data.table(aces_daily)
draw <- draw[order(UserID, SurveyDay, SurveyInteger)]
draw[, UserID := factor(UserID)]

tmpdata <- draw[!is.na(SurveyDay) & !is.na(SurveyInteger)][, .(
  MinD = min(SurveyDay),
  MinS = min(SurveyInteger[SurveyDay == min(SurveyDay)]),
  MaxD = max(SurveyDay),
  MaxS = max(SurveyInteger[SurveyDay == max(SurveyDay)])),
  by = UserID]

tmpdata <- tmpdata[, .(
  SurveyInteger = c(
    MinS:3L, #first day
    rep(1L:3L, times = MaxD - MinD - 1), #all days between first/last
    1L:MaxS), #last day
  SurveyDay = as.Date(rep(MinD:MaxD, c(
      4L - MinS, #first day
      rep(3, MaxD - MinD - 1), #all days between first/last
      MaxS)), origin = "1970-01-01")), #lastday
  by = UserID]

d <- merge(draw, tmpdata, by = c("UserID", "SurveyDay", "SurveyInteger"),
           all = TRUE)

nrow(draw)
nrow(d)

nrow(draw)/nrow(d)


## ----fglmmi-varplot, fig.width=4, fig.height=6, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plot showing hypothetical data with high between variance and low between variance. In the high between variance, observations within a person vary little, but there are large individual differences. In low between variance, there are not many individual differences, but large variability within each person"----

set.seed(1234)
ex.data.1 <- data.table(
  ID = factor(rep(1:4, each = 10)),
  time = rep(1:10, times = 4),
  y = rnorm(40, rep(1:4, each = 10), .2))

ex.data.2 <- data.table(
  ID = factor(rep(1:4, each = 10)),
  time = rep(1:10, times = 4),
  y = rnorm(40, 2.5, 1))

plot_grid(
 ggplot(ex.data.1,
        aes(time, y, colour = ID, shape = ID)) +
  stat_smooth(method = "lm", formula = y ~ 1, se=FALSE) +
  geom_point() +
  scale_color_viridis(discrete = TRUE),
 ggplot(ex.data.2,
        aes(time, y, colour = ID, shape = ID)) +
  stat_smooth(method = "lm", formula = y ~ 1, se=FALSE) +
  geom_point() +
  scale_color_viridis(discrete = TRUE),
 ncol = 1,
 labels = c(
   "High Between Variance",
   "Low Between Variance"),
 align = "hv")


## ------------------------------------------------------------------------

## mean and SD on all observations
egltable("PosAff", data = d)

## mean and SD first averaging within ID
egltable("PosAff",
  data = d[, .(
    PosAff = mean(PosAff, na.rm = TRUE)),
    by = UserID])

## mean and SD on first observations
egltable("PosAff", data = d[
  order(UserID, SurveyDay, SurveyInteger)][,
    .(PosAff = PosAff[1]), by = UserID])


## ------------------------------------------------------------------------

tab <- egltable(c("Female", "Age", "BornAUS", "SES_1", "EDU"),
                data = d[!duplicated(UserID)],
                strict = FALSE)
tab


## ----fglmmi-likplot, fig.width=7, fig.height=4, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plot showing average coping ratings for women and men."----

## create a dataset of the means and labels by gender
copeplotdata <- d[!is.na(Female), .(
  M = c(
    mean(COPEPrb, na.rm = TRUE),
    mean(COPEPrc, na.rm = TRUE),
    mean(COPEExp, na.rm = TRUE),
    mean(COPEDis, na.rm = TRUE)),
  Var = 1:4,
  Low = sprintf("I usually don't do this at all\n[%s]",
                c("Problem Focused", "Emotional Processing",
                  "Emotional Expression", "Disengagement")),
  High = sprintf("I usually do this a lot\n[%s]",
                 c("Problem Focused", "Emotional Processing",
                   "Emotional Expression", "Disengagement"))),
  by = Female]

## coded 0/1 but for plotting, R needs to know
## it is discrete not a continuous number
copeplotdata[, Female := factor(Female)]

## create a plot
gglikert(x = "M", y = "Var", leftLab = "Low", rightLab = "High",
         data = copeplotdata, colour = "Female",
  xlim = c(1, 4), title = "Average Coping") +
  scale_colour_manual(values =
    c("1" = "grey70", "0" = "grey30"))


## ----fglmmi-likplotaf, fig.width=7, fig.height=3, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Plot showing average coping ratings for women and men."----

## create a dataset of the means and labels by stress
afplotdata <- d[!is.na(STRESS), .(
  M = c(
    mean(PosAff, na.rm = TRUE),
    mean(NegAff, na.rm = TRUE)),
  Var = 1:2,
  Low = sprintf("Very Slightly or\nNot at all\n[%s]",
                c("Positive Affect", "Negative Affect")),
  High = sprintf("Extremely\n\n[%s]",
                c("Positive Affect", "Negative Affect"))),
  by = .(Stress = STRESS > 5)]

## add labels to understand stress
afplotdata[, Stress := factor(Stress, levels = c(FALSE, TRUE),
                              labels = c("<= 5", "> 5"))]

## create a plot
gglikert(x = "M", y = "Var", leftLab = "Low", rightLab = "High",
         data = afplotdata, colour = "Stress",
  xlim = c(1, 5), title = "Affect by Stress") +
  scale_colour_manual(values =
    c("<= 5" = "grey70", "> 5" = "grey30"))


## ------------------------------------------------------------------------

d[, Survey := factor(SurveyInteger, levels = 1:3,
    labels = c("Morning", "Afternoon", "Evening"))]

egltable(c("PosAff", "NegAff", "STRESS"), g = "Survey",
  data = d[, .(
    PosAff = mean(PosAff, na.rm = TRUE),
    NegAff = mean(NegAff, na.rm = TRUE),
    STRESS = mean(STRESS, na.rm = TRUE)
    ), by = .(UserID, Survey)])


## ------------------------------------------------------------------------

d[, BPosAff := mean(PosAff, na.rm = TRUE), by = UserID]
d[, WPosAff := PosAff - BPosAff]

egltable("BPosAff", data = d[!duplicated(UserID)])
egltable("WPosAff", data = d)


## ------------------------------------------------------------------------

## define a new function
bwmean <- function(x, na.rm = TRUE) {
  m <- mean(x, na.rm = na.rm)
  list(m, x - m)
}

## apply it to affect, support, and stress, by ID
d[, c("BNegAff", "WNegAff") := bwmean(NegAff), by = UserID]
d[, c("BSUPPORT", "WSUPPORT") := bwmean(SUPPORT), by = UserID]
d[, c("BSTRESS", "WSTRESS") := bwmean(STRESS), by = UserID]


## ------------------------------------------------------------------------

d[, .(
  NCope = sum(!is.na(COPEPrb)),
  NSOLs = sum(!is.na(SOLs))),
  by = Survey]


## ------------------------------------------------------------------------

d[, BCOPEPrb := mean(COPEPrb, na.rm = TRUE), by = UserID]
d[, WCOPEPrb := na.omit(COPEPrb) - BCOPEPrb,
  by = .(UserID, SurveyDay)]
d[, BCOPEPrc := mean(COPEPrc, na.rm = TRUE), by = UserID]
d[, WCOPEPrc := na.omit(COPEPrc) - BCOPEPrc,
  by = .(UserID, SurveyDay)]
d[, BCOPEExp := mean(COPEExp, na.rm = TRUE), by = UserID]
d[, WCOPEExp := na.omit(COPEExp) - BCOPEExp,
  by = .(UserID, SurveyDay)]
d[, BCOPEDis := mean(COPEDis, na.rm = TRUE), by = UserID]
d[, WCOPEDis := na.omit(COPEDis) - BCOPEDis,
  by = .(UserID, SurveyDay)]

d[, BSOLs := mean(SOLs, na.rm = TRUE), by = UserID]
d[, WSOLs := na.omit(SOLs) - BSOLs,
  by = .(UserID, SurveyDay)]
d[, BWASONs := mean(WASONs, na.rm = TRUE), by = UserID]
d[, WWASONs := na.omit(WASONs) - BWASONs,
  by = .(UserID, SurveyDay)]


## ------------------------------------------------------------------------

iccMixed("NegAff", "UserID", d)

iccMixed("PosAff", "UserID", d)


## ------------------------------------------------------------------------

## number of units
n <- length(unique(d$UserID))

## average observations per unit
k <- nrow(d[!is.na(NegAff)])/n

## effective sample size
nEffective(n, k, dv = "NegAff", id = "UserID", data = d)

k <- nrow(d[!is.na(PosAff)])/n
nEffective(n, k, dv = "PosAff", id = "UserID", data = d)


## ------------------------------------------------------------------------

tmp <- meanDecompose(PosAff ~ UserID, data = d)
str(tmp, max.level = 1)


## ----fglmmi-bdistpa, fig.width=4, fig.height=5, out.width='.4\\linewidth', fig.pos="!h", fig.cap = "Between person positive affect against a normal distribution."----

testdistr(tmp[[1]]$X, varlab = names(tmp)[1],
          extremevalues = "theoretical", robust=TRUE)


## ----fglmmi-mdistpa, fig.width=8, fig.height=8, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Between and within person positive affect against a normal distribution."----

plots <- lapply(names(tmp), function(x) {
  testdistr(tmp[[x]]$X, plot = FALSE, varlab = x,
            extremevalues = "theoretical", robust=TRUE)[1:2]
})

do.call(plot_grid, c(unlist(plots, FALSE), ncol = 2))


## ----fglmmi-mdistna, fig.width=8, fig.height=12, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Different levels of negative affect against a normal distribution."----

tmp <- meanDecompose(NegAff ~ UserID + SurveyDay, data = d)
do.call(plot_grid, c(unlist(lapply(names(tmp), function(x) {
  testdistr(tmp[[x]]$X, plot = FALSE, varlab = x,
            extremevalues = "theoretical", robust=TRUE)[1:2]
}), FALSE), ncol = 2))


## ----fglmmi-mdistlogna, fig.width=8, fig.height=12, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Different levels of the natural logarithm of negative affect against a normal distribution."----

d[, logNegAff := log(NegAff)]
tmp <- meanDecompose(logNegAff ~ UserID + SurveyDay, data = d)
do.call(plot_grid, c(unlist(lapply(names(tmp), function(x) {
  testdistr(tmp[[x]]$X, plot = FALSE, varlab = x,
            extremevalues = "theoretical", robust=TRUE)[1:2]
}), FALSE), ncol = 2))


## ----fglmmi-timetrends, fig.width=8, fig.height=6, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Trends in variables over time with a gam smooth.", warning=FALSE----

dt <- d[, .(
  WPosAff = mean(WPosAff, na.rm = TRUE),
  WNegAff = mean(WNegAff, na.rm = TRUE),
  WSTRESS = mean(WSTRESS, na.rm = TRUE),
  WSUPPORT = mean(WSUPPORT, na.rm = TRUE),
  WSOLs = mean(WSOLs, na.rm = TRUE),
  WWASONs = mean(WWASONs, na.rm = TRUE)) , by = SurveyDay]
dt <- melt(dt, id.var = "SurveyDay")

ggplot(dt, aes(SurveyDay, value)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 10)) +
  facet_wrap(~ variable, scales = "free")


## ----fglmmi-weekend, fig.width=8, fig.height=6, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Trends in variables over time with a gam smooth."----

dt[, Weekend := weekdays(SurveyDay) %in% c("Saturday", "Sunday")]

ggplot(dt, aes(Weekend, value)) +
  stat_summary(fun.data = mean_cl_boot) +
  facet_wrap(~ variable, scales = "free")


## ------------------------------------------------------------------------

d[, StartTimec11Alt := ifelse(is.na(SurveyStartTimec11),
                              mean(SurveyStartTimec11, na.rm = TRUE),
                              SurveyStartTimec11),
  by = .(UserID, Survey)]
d[, StartDayTimec11Alt := chron(
      dates. = format(SurveyDay, "%m/%d/%Y"),
      times. = StartTimec11Alt)]


## ----fglmmi-acf1, fig.width=5, fig.height=4, out.width='.4\\linewidth', fig.pos="!h", fig.cap = "Autocorrelation for one participant.", warning=FALSE----

tmpd <- d[UserID == 1]
acf(na.approx(zoo(tmpd$PosAff,
    order.by = tmpd$StartDayTimec11Alt)),
    lag.max = 10)


## ----fglmmi-acfall, fig.width=4, fig.height=8, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Autocorrelation for all participant for positive and negative affect and stress.", message = FALSE, warning = FALSE----

acf.posaff <- acfByID("PosAff", "StartDayTimec11Alt",
                      "UserID", d)

print(acf.posaff)

## make for other measures
acf.negaff <- acfByID("NegAff", "StartDayTimec11Alt",
                      "UserID", d)
acf.stress <- acfByID("STRESS", "StartDayTimec11Alt",
                      "UserID", d)

## put into one dataset for plotting a panel
acf.all <- rbind(
  acf.posaff, acf.negaff,
  acf.stress)

ggplot(acf.all,
    aes(factor(Lag), y = AutoCorrelation)) +
  geom_hline(yintercept = 0, colour = "grey50", size = 1) +
  geom_hline(yintercept = c(-.5, .5),
             linetype = 2, colour = "grey50", size = 1) +
  geom_boxplot() + ylab("Auto Correlation") +
  facet_wrap(~ Variable, ncol = 1)


## ------------------------------------------------------------------------

## ensure data ordered by ID, date, and time
d <- d[order(UserID, SurveyDay, SurveyInteger)]
## calculate a number for the survey from 1 to total
d[, USURVEYID := 1:.N, by = .(UserID)]

d[,
  c("NegAffLag1", "WNegAffLag1",
    "PosAffLag1", "WPosAffLag1",
    "STRESSLag1", "WSTRESSLag1") :=
    .SD[.(UserID = UserID, USURVEYID = USURVEYID - 1),
    .(NegAff, WNegAff,
      PosAff, WPosAff,
      STRESS, WSTRESS),
      on = c("UserID", "USURVEYID")]]

d[,
  c("WCOPEPrbLag1", "WCOPEPrcLag1",
    "WCOPEExpLag1", "WCOPEDisLag1",
    "WSOLsLag1", "WWASONsLag1") :=
  .SD[.(UserID = UserID, Survey = Survey, SurveyDay = SurveyDay - 1),
      .(WCOPEPrb, WCOPEPrc, WCOPEExp, WCOPEDis,
        WSOLs, WWASONs),
      on = c("UserID", "Survey", "SurveyDay")]]

## save data after processing, with compression
## for use in subsequent chapters
saveRDS(d, file = "aces_daily_sim_processed.RDS",
        compress = "xz")


## ----fglmmi-glmmdiagnegaff, fig.width=8, fig.height=10, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Negative affect mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random intercept (middle left), the distribution of the random slope (middle right), and whether the random effects are multivariate normal (bottom left).", warning = FALSE----

m.negaff <- lmer(NegAff ~ 1 + BSTRESS + WSTRESS +
            (1 + WSTRESS | UserID), data = d)
assumptiontests <- plotDiagnosticsLMER(m.negaff, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot, ncol = 2))


## ----fglmmi-glmmdiagposaff, fig.width=8, fig.height=10, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Positive affect mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random intercept (middle left), the distribution of the random slope (middle right), and whether the random effects are multivariate normal (bottom left).", warning = FALSE----

m.posaff <- lmer(PosAff ~ 1 + BSTRESS + WSTRESS +
            (1 + WSTRESS | UserID), data = d)

assumptiontests <- plotDiagnosticsLMER(m.posaff, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot, ncol = 2))


## ----fglmmi-glmmdiagposaff2, fig.width=8, fig.height=10, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Positive affect mixed effects model diagnostic plots showing the distribution of residuals (top left), the residuals versus fitted values to assess homogeneity of variance (top right), the distribution of the random intercept (middle left), the distribution of the random slope (middle right), and whether the random effects are multivariate normal (bottom left). Results after removing two multivariate outliers, IDs 57 and 123.", warning = FALSE----

assumptiontests$ExtremeValues[
  EffectType == "Multivariate Random Effect UserID"]

m.posaff <- lmer(PosAff ~ 1 + BSTRESS + WSTRESS +
            (1 + WSTRESS | UserID),
            data = d[!UserID %in% c(57, 123)])

assumptiontests <- plotDiagnosticsLMER(m.posaff, plot = FALSE)
do.call(plot_grid, c(
  assumptiontests[c("ResPlot", "ResFittedPlot")],
  assumptiontests$RanefPlot, ncol = 2))




## ----fglmmi-posaffext, fig.width=5, fig.height=5, out.width='.5\\linewidth', fig.pos="!h", fig.cap = "Positive affect and stress associations, highlight extreme cases", warning = FALSE----

ggplot() +
  stat_smooth(aes(WSTRESS, PosAff, group = UserID),
    data = d[!UserID %in% c(123)], method = "lm",
   se = FALSE, colour = "grey50") +
  stat_smooth(aes(WSTRESS, PosAff, group = UserID),
    data = d[UserID %in% c(123)], method = "lm",
   se = FALSE, colour = "blue", size = 2) +
  geom_point(aes(WSTRESS, PosAff),
    data = d[UserID %in% c(123)], colour = "blue", size = 2) +
  stat_smooth(aes(WSTRESS, PosAff, group = UserID),
    data = d[UserID %in% c(57)], method = "lm",
   se = FALSE, colour = "orange", size = 2) +
  geom_point(aes(WSTRESS, PosAff),
    data = d[UserID %in% c(57)], colour = "orange", size = 2)



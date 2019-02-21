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
library(data.table)
library(JWileymisc)
library(varian)
library(mice)
library(parallel)

options(width = 70, digits = 2)


## ------------------------------------------------------------------------

## ordered
sd(c(1, 3, 5, 7, 9))
rmssd(c(1, 3, 5, 7, 9))

## randomized
sd(c(3, 1, 9, 5, 7))
rmssd(c(3, 1, 9, 5, 7))


## ------------------------------------------------------------------------

data(aces_daily)
draw <- as.data.table(aces_daily)
d <- readRDS("aces_daily_sim_processed.RDS")

variability_measures <- function(x) {
  x <- na.omit(x)
  list(
    SD = sd(x),
    VAR = sd(x)^2,
    RMSSD = rmssd(x),
    MSSD = rmssd(x)^2,
    MAD = median(abs(x - median(x))),
    RANGE = range(x),
    IQR = abs(diff(quantile(x, probs = c(.25, .75)))),
    CV = sd(x) / mean(x))
}


## ----fiiv-varplot, fig.width=14, fig.height=14, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Correlations between variability measures for applied to individuals across four different measures."----

plot_grid(
  plot(SEMSummary(~ .,
    data = d[, variability_measures(PosAff), by = UserID][,-1]),
    order = "asis") +
    ggtitle("PosAff"),
  plot(SEMSummary(~ .,
    data = d[, variability_measures(NegAff), by = UserID][,-1]),
    order = "asis") +
    ggtitle("NegAff"),
  plot(SEMSummary(~ .,
    data = d[, variability_measures(COPEPrc), by = UserID][,-1]),
    order = "asis") +
    ggtitle("COPEPrc"),
  plot(SEMSummary(~ .,
    data = d[, variability_measures(SOLs), by = UserID][,-1]),
    order = "asis") +
    ggtitle("SOLs"),
ncol = 2)


## ----fiiv-varplot2, fig.width=7, fig.height=7, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Two hypothetical people given an intervention, Person A (purple) and Person B (yellow), both improve at about the same rate, but Person B is less consistent."----

iivdat <- data.table(
  Assessment = 0:15,
  PersonA = c(1, 3, 2, 4, 3, 5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10),
  PersonB = c(2, 5, 2, 6, 3, 7, 4, 8, 5, 9, 6, 10, 7, 11, 8, 12))

ggplot(iivdat, aes(Assessment)) +
  stat_smooth(aes(y = PersonA), method = "lm", se=FALSE,
              colour = viridis(2)[1], linetype = 2) +
  geom_line(aes(y = PersonA),
            colour = viridis(2)[1], size = 1) +
  stat_smooth(aes(y = PersonB), method = "lm", se=FALSE,
              colour = viridis(2)[2], linetype = 2) +
  geom_line(aes(y = PersonB),
            colour = viridis(2)[2], size = 1) +
  ylab("Outcome Scores")


## ------------------------------------------------------------------------

## ISD
sd(iivdat$PersonA)
sd(iivdat$PersonB)


## ------------------------------------------------------------------------

## ISD, after removing systematic improvements
sd(resid(lm(PersonA ~ Assessment, data = iivdat)))
sd(resid(lm(PersonB ~ Assessment, data = iivdat)))


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

  library(varian)
})

system.time(m <- varian(
  y.formula = BNegAff ~ 1,
  v.formula = PosAff ~ 1 | UserID,
  data = d,
  design = "V -> Y",
  useU = TRUE,
  totaliter = 10000,
  warmup = 500, thin = 5,
  chains = 2, verbose=TRUE,
  cl = cl))


## ----fiiv-bvmdiag1, fig.width=9, fig.height=14, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Diagnostics including percent scale reduction factor (Rhat), effective sample size, distribution of the individual standard deviations, distribution of the individual means, and individual estimates of individual standard deviations and means with credible intervals."----

## check diagnostics
vm_diagnostics(m)


## ----fiiv-bvmres1, fig.width=8, fig.height=8, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Plots of the distributions, bivariate scatter plot, and proportion of cases on each side of zero for empirical p-values."----

## extract MCMC samples
mcmc.samples <- extract(m$results,
  permute = TRUE)

## examine MCMC samples of
## the alpha regression coefficients
vmp_plot(mcmc.samples$Yalpha)


## ------------------------------------------------------------------------

## intercept of average negative affect
param_summary(mcmc.samples$YB[, 1])

## IIV on average negative affect
param_summary(mcmc.samples$Yalpha[, 1])

## individual mean on average negative affect
param_summary(mcmc.samples$Yalpha[, 2])

## residual error of average negative affect
param_summary(mcmc.samples$sigma_Y)


## ------------------------------------------------------------------------

## intercept of positive affect
param_summary(mcmc.samples$VB[, 1])

## positive affect random intercept standard deviation
param_summary(mcmc.samples$sigma_U)

## estimate of the gamma rate parameter for IIVs
param_summary(mcmc.samples$rate)

## estimate of the gamma shape parameter for IIVs
param_summary(mcmc.samples$shape)

## ------------------------------------------------------------------------

dim(mcmc.samples$Sigma_V)
str(mcmc.samples$Sigma_V)


## ------------------------------------------------------------------------

avg_dataset <- cbind(
  d[!duplicated(UserID), .(BNegAff)],
  IIV = colMeans(mcmc.samples$Sigma_V),
  IIM = colMeans(mcmc.samples$U))

avg_model <- lm(BNegAff ~ IIV + IIM, data = avg_dataset)

summary(avg_model)


## ------------------------------------------------------------------------

ind_dataset <- lapply(seq(1, 1000, by = 10), function(i) {
  cbind(
  d[!duplicated(UserID), .(BNegAff)],
  IIV = mcmc.samples$Sigma_V[i, ],
  IIM = mcmc.samples$U[i, ])
})


ind_model <- lapply(ind_dataset, function(tmpdat) {
  lm(BNegAff ~ IIV + IIM, data = tmpdat)
})

ind_model_pooled <- pool(as.mira(ind_model))


## ------------------------------------------------------------------------

raw_model <- lm(BNegAff ~ IIV + IIM,
 data = d[, .(BNegAff = BNegAff[1],
              IIV = sd(PosAff, na.rm = TRUE),
              IIM = mean(PosAff, na.rm = TRUE)),
          by = UserID])


## ------------------------------------------------------------------------

## Bayesian Results
param_summary(mcmc.samples$YB[, 1]) ## intercept
param_summary(mcmc.samples$Yalpha[, 1]) ## IIV
param_summary(mcmc.samples$Yalpha[, 2]) ## IIM

## using averages only
cbind(B = coef(avg_model), confint(avg_model))

## using raw ISDs
cbind(B = coef(raw_model), confint(raw_model))

## treating as multiply imputed
summary(ind_model_pooled, conf.int = TRUE)



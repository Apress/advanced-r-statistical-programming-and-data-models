## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----

library(checkpoint)
checkpoint("2018-09-28", R.version = "3.5.1",
  project = book_directory,
  checkpointLocation = checkpoint_directory,
  scanForPackages = FALSE,
  scan.rnw.with.knitr = TRUE, use.knitr = TRUE)

library(knitr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(viridis)
library(car)
library(mgcv)
library(VGAM)
library(ipw)
library(JWileymisc)
library(xtable)

options(
  width = 70,
  stringsAsFactors = FALSE,
  datatable.print.nrows = 20,
  datatable.print.topn = 3,
  digits = 2)


## ----fgam-poly1, fig.width=7, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Graph showing an intercept only (flat line) and progressively higher order polynomials."----

acl <- readRDS("advancedr_acl_data.RDS")

ggplot(acl, aes(AGE_W1, CESD11_W1)) +
  stat_smooth(method = "lm", formula = y ~ 1,
    colour = viridis(6)[1], linetype = 1, se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ x,
    colour = viridis(6)[2], linetype = 4, se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2),
    colour = viridis(6)[3], linetype = 2, se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3),
    colour = viridis(6)[4], linetype = 3, se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 4),
    colour = viridis(6)[5], linetype = 1, se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 10),
    colour = viridis(6)[6], linetype = 5, se = FALSE)


## ------------------------------------------------------------------------

## > and <
1:5 %gl% c(2, 4)

## > and <=
1:5 %gle% c(2, 4)

## >= and <
1:5 %gel% c(2, 4)

## >= and <=
1:5 %gele% c(2, 4)



## ----fgam-spline1, fig.width=7, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Graph showing an step function spline, linear splines, and quadratic splines, all with two inner knots."----

ggplot(acl, aes(AGE_W1, CESD11_W1)) +
  stat_smooth(method = "lm",
    formula = y ~ 1 +
      ifelse(x %gle% c(42, 65), 1, 0) +
      ifelse(x %gle% c(65, 96), 1, 0),
    colour = viridis(6)[1], linetype = 1, se = FALSE) +
  stat_smooth(method = "lm",
    formula = y ~ bs(x, df = 3, degree = 1L),
    colour = viridis(6)[2], linetype = 2, se = FALSE) +
  stat_smooth(method = "lm",
    formula = y ~ bs(x, df = 4, degree = 2L),
    colour = viridis(6)[3], linetype = 3, se = FALSE) +
  stat_smooth(method = "lm",
    formula = y ~ bs(x, df = 5, degree = 3L),
    colour = viridis(6)[4], linetype = 4, se = FALSE)


## ----fgam-spline2, fig.width=10, fig.height=9, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Graph showing B-Splines (basis splines)."----

knots <- c(33, 42, 57, 65, 72)
x <- seq(from = min(acl$AGE_W1),
         to = max(acl$AGE_W1), by = .01)

p1 <- ggplot(melt(bs(x, degree = 1,
          knots = knots, intercept = TRUE)),
          aes(Var1, value, colour = factor(Var2))) +
  geom_line() +
  scale_color_viridis("Basis", discrete = TRUE) +
  theme_tufte()

plot_grid(
  p1 +
    ggtitle("5 Knots, Degree = 1"),
  p1 %+% melt(bs(x, degree = 2,
          knots = knots, intercept = TRUE)) +
    ggtitle("5 Knots, Degree = 2"),
  p1 %+% melt(bs(x, degree = 3,
          knots = knots, intercept = TRUE)) +
    ggtitle("5 Knots, Degree = 3"),
  p1 %+% melt(bs(x, degree = 4,
          knots = knots, intercept = TRUE)) +
    ggtitle("5 Knots, Degree = 4"),
  ncol = 2)


## ------------------------------------------------------------------------

mgam <- vgam(CESD11_W1 ~ Sex + s(AGE_W1, df = 3), data = acl,
        family = uninormal(), model = TRUE)

summary(mgam)


## ------------------------------------------------------------------------

coef(mgam)


## ------------------------------------------------------------------------

## test parametric coefficient for sex
linearHypothesis(mgam, "Sex(2) FEMALE",
  coef. = coef(mgam), vcov = vcov(mgam))


## ------------------------------------------------------------------------

## test parametric coefficient for
## intercept and sex simultaneously
linearHypothesis(mgam,
  c("(Intercept):1", "Sex(2) FEMALE"),
  coef. = coef(mgam), vcov = vcov(mgam))


## ----fgam-agegam1, fig.width=9, fig.height=5, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Plot of model results for a generalized additive model with sex as a parametric term and age as a smooth spline.", warning=FALSE----

par(mfrow = c(1, 2))
plot(mgam, se = TRUE,
     lcol = viridis(4)[1], scol = viridis(4)[2])


## ------------------------------------------------------------------------

mlin <- vglm(CESD11_W1 ~ Sex + AGE_W1, data = acl,
        family = uninormal(), model = TRUE)
mquad <- vglm(CESD11_W1 ~ Sex + poly(AGE_W1, 2), data = acl,
        family = uninormal(), model = TRUE)


## ----fgam-agegam2, fig.width=9, fig.height=5, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Two panel plot showing the predicted depression symptom level by age from a generalized additive model versus a linear fit on the left and a quadratic fit on the right.", warning=FALSE----

par(mfrow = c(1, 2))
plot(mgam, se = TRUE, which.term = 2,
     lcol = viridis(4)[1], scol = viridis(4)[1])
plot(as(mlin, "vgam"), se = TRUE, which.term = 2,
     lcol = viridis(4)[2], scol = viridis(4)[2],
     overlay = TRUE, add = TRUE)

plot(mgam, se = TRUE, which.term = 2,
     lcol = viridis(4)[1], scol = viridis(4)[1])
plot(as(mquad, "vgam"), se = TRUE, which.term = 2,
     lcol = viridis(4)[3], scol = viridis(4)[3],
     overlay = TRUE, add = TRUE)


## ------------------------------------------------------------------------

mgam2 <- vgam(CESD11_W2 ~ Sex +
               s(CESD11_W1, df = 3) +
               s(AGE_W1, df = 3), data = acl,
        family = uninormal(), model = TRUE)

summary(mgam2)


## ------------------------------------------------------------------------

mgam3 <- vgam(CESD11_W2 ~ Sex +
               s(CESD11_W1, df = 3) +
               AGE_W1, data = acl,
        family = uninormal(), model = TRUE)

summary(mgam3)


## ------------------------------------------------------------------------

names(coef(mgam3))

linearHypothesis(mgam3,
  "Sex(2) FEMALE",
  coef. = coef(mgam3), vcov = vcov(mgam3))

linearHypothesis(mgam3,
  "AGE_W1",
  coef. = coef(mgam3), vcov = vcov(mgam3))


## ----fgam-gam3, fig.width=10, fig.height=10, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Plot of model results for a generalized additive model with sex and age as parametric terms and wave 1 depression symptoms as a smooth spline.", warning=FALSE----

par(mfrow = c(2, 2))
plot(mgam3, se = TRUE,
     lcol = viridis(4)[1],
     scol = viridis(4)[2])


## ------------------------------------------------------------------------


## generate new data for prediction
## use the whole range of sex and depression symptoms
## and a five number summary of age
## (min, 25th 50th 75th percentiles and max)
newdat <- as.data.table(expand.grid(
  Sex = levels(acl$Sex),
  CESD11_W1 = seq(
    from = min(acl$CESD11_W1, na.rm=TRUE),
    to = max(acl$CESD11_W1, na.rm=TRUE),
    length.out = 1000),
  AGE_W1 = fivenum(acl$AGE_W1)))

newdat$yhat <- predict(mgam3, newdata = newdat)


## ----fgam-gampred, fig.width=10, fig.height=6, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Predicted depression symptoms at wave 2 across levels of wave 1 depression symptoms at varying ages and sex.", warning=FALSE----

ggplot(newdat,
       aes(CESD11_W1, yhat,
           colour = factor(AGE_W1),
           linetype = factor(AGE_W1))) +
  geom_line() +
  scale_color_viridis("Age", discrete = TRUE) +
  scale_linetype_discrete("Age") +
  facet_wrap(~ Sex) +
  theme(legend.position = c(.75, .2),
        legend.key.width = unit(1.5, "cm")) +
  xlab("Depression Symptoms (Wave 1)") +
  ylab("Depression Symptoms (Wave 2)")


## ------------------------------------------------------------------------

detach("package:VGAM")
library(mgcv)


## ------------------------------------------------------------------------

mgam4 <- gam(CESD11_W2 ~ Sex +
               s(CESD11_W1, k = 3) +
               s(AGE_W1, k = 3), data = acl,
        family = gaussian())


## ------------------------------------------------------------------------

summary(mgam4)


## ------------------------------------------------------------------------

mgam5 <- gam(CESD11_W2 ~ Sex +
               s(CESD11_W1, k = 4) +
               s(AGE_W1, k = 4), data = acl,
        family = gaussian())

summary(mgam5)


## ----fgam-gam5, fig.width=10, fig.height=10, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Plot of model results for two generalized additive models varying the maximum flexibility of the smooth splines.", warning=FALSE----

par(mfrow = c(2, 2))
plot(mgam4, se = TRUE, scale = 0, main = "k = 3")
plot(mgam5, se = TRUE, scale = 0, main = "k = 4")


## ----fgam-gam6, fig.width=10, fig.height=10, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Plot of model results for a generalized additive models allowing splines to vary by sex.", warning=FALSE----

mgam6 <- gam(CESD11_W2 ~ Sex +
              s(CESD11_W1, k = 4, by = Sex) +
              s(AGE_W1, k = 4, by = Sex),
             data = acl,
        family = gaussian())

summary(mgam6)

par(mfrow = c(2, 2))
plot(mgam6, ask = FALSE, scale = 0)


## ------------------------------------------------------------------------

AIC(mgam5, mgam6)
BIC(mgam5, mgam6)


## ------------------------------------------------------------------------

mgam7 <- gam(CESD11_W2 ~ Sex +
               te(CESD11_W1, SelfEsteem_W1, k = 4^2),
             data = acl,
        family = gaussian())

summary(mgam7)


## ----fgam-persp3d, fig.width=10, fig.height=10, out.width='1\\linewidth', fig.pos="!h", fig.cap = "3D perspective plots showing the result of a tensor product smooth between depression symptoms and self-esteem at wave 1 predicting depression at wave 2.", warning=FALSE----

par(mfrow = c(2, 2), mar = c(.1, .1, .1, .1))
vis.gam(mgam7,
  view = c("CESD11_W1", "SelfEsteem_W1"),
  theta = 210, phi = 40,
  color = "topo",
  plot.type = "persp")
vis.gam(mgam7,
  view = c("CESD11_W1", "SelfEsteem_W1"),
  theta = 150, phi = 40,
  color = "topo",
  plot.type = "persp")
vis.gam(mgam7,
  view = c("CESD11_W1", "SelfEsteem_W1"),
  theta = 60, phi = 40,
  color = "topo",
  plot.type = "persp")
vis.gam(mgam7,
  view = c("CESD11_W1", "SelfEsteem_W1"),
  theta = 10, phi = 40,
  color = "topo",
  plot.type = "persp")


## ----fgam-contour, fig.width=7, fig.height=7, out.width='0.6\\linewidth', fig.pos="!h", fig.cap = "Contour plot showing the result of a tensor product smooth between depression symptoms and self-esteem at wave 1 predicting depression at wave 2.", warning=FALSE----

par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
vis.gam(mgam7,
  view = c("CESD11_W1", "SelfEsteem_W1"),
  color = "topo",
  plot.type = "contour")


## ------------------------------------------------------------------------

mgam8 <- gam(CESD11_W2 ~ Sex +
               ti(CESD11_W1, k = 4) +
               ti(SelfEsteem_W1, k = 4) +
               ti(CESD11_W1, SelfEsteem_W1, k = 4^2),
             data = acl,
        family = gaussian())

summary(mgam8)


## ------------------------------------------------------------------------

mgam5 <- gam(CESD11_W2 ~ Sex +
               s(CESD11_W1, k = 4) +
               s(AGE_W1, k = 4), data = acl,
        family = gaussian())

summary(mgam5)


## ----fgam-checkit, fig.width=7, fig.height=7, out.width='0.7\\linewidth', fig.pos="!h", fig.cap = "Diagnostics plots from generalized additive model.", warning=FALSE----

par(mfrow = c(2, 2))
set.seed(12345)
gam.check(mgam5)


## ------------------------------------------------------------------------

mgam5b <- gam(CESD11_W2 ~ Sex +
               s(CESD11_W1, k = 20) +
               s(AGE_W1, k = 20), data = acl,
        family = gaussian())

summary(mgam5b)


## ----fgam-gam5b, fig.width=10, fig.height=6, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Plot of model results for generalized additive model after increasing k for depression symptoms."----

par(mfrow = c(1, 2))
plot(mgam5b, se = TRUE, scale = 0)


## ------------------------------------------------------------------------

library(VGAM)
acl$CurSmoke <- as.integer(acl$Smoke_W1 == "(1) Cur Smok")

mgam.lr1 <- vgam(CurSmoke ~ s(AGE_W1, df = 3),
             family = binomialff(link = "logit"),
             data = acl, model = TRUE)

summary(mgam.lr1)


## ----fgam-lr1, fig.width=7, fig.height=7, out.width='0.7\\linewidth', fig.pos="!h", fig.cap = "Generalized additive model for age and current smoking status.", warning = FALSE----

par(mfrow = c(1, 1))
plot(mgam.lr1, se = TRUE,
     lcol = viridis(4)[1],
     scol = viridis(4)[2])


## ------------------------------------------------------------------------

## generate new data for prediction
## use the whole range of age
newdat <- as.data.table(expand.grid(
  AGE_W1 = seq(
    from = min(acl$AGE_W1, na.rm=TRUE),
    to = max(acl$AGE_W1, na.rm=TRUE),
    length.out = 1000)))

newdat$yhat <- predict(mgam.lr1,
                       newdata = newdat,
                       type = "response")


## ----fgam-lr1pred, fig.width=6, fig.height=5, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Predicted probability of smoking across ages."----

ggplot(newdat, aes(AGE_W1, yhat)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  xlab("Age (years)") +
  ylab("Probability of Smoking") +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, .4),
                  expand = FALSE)


## ------------------------------------------------------------------------

nboot <- 500

out <- matrix(NA_real_, ncol = nboot, nrow = nrow(newdat))

start.time <- proc.time()
set.seed(12345)
for (i in 1:500) {
  tmp <- vgam(CurSmoke ~ s(AGE_W1, df = 3),
             family = binomialff(link = "logit"),
             data = acl[sample(nrow(acl), replace = TRUE)], model = TRUE)
  out[, i] <- predict(tmp,
                      newdata = newdat,
                      type = "response")
}
stop.time <- proc.time()

## time to bootstrap 500 times
stop.time - start.time


## ------------------------------------------------------------------------

mean(abs(newdat$yhat - rowMeans(out)))


## ------------------------------------------------------------------------

newdat$LL <- apply(out, 1, quantile,
  probs = .025, na.rm = TRUE)

newdat$UL <- apply(out, 1, quantile,
  probs = .975, na.rm = TRUE)


## ----fgam-lr1predboot, fig.width=6, fig.height=5, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Predicted probability of smoking across ages."----

ggplot(newdat, aes(AGE_W1, yhat)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "grey80") +
  geom_line(size = 2) +
  scale_y_continuous(labels = percent) +
  xlab("Age (years)") +
  ylab("Probability of Smoking") +
  theme_tufte() +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, .5),
                  expand = FALSE)


## ------------------------------------------------------------------------

acl[, EmployG_W2 := as.character(Employment_W2)]
acl[EmployG_W2 %in% c(
  "(2) 2500+HRS", "(3) 15002499",
  "(4) 500-1499", "(5) 1-499HRS"),
  EmployG_W2 := "(2) EMPLOYED"]
acl[, EmployG_W2 := factor(EmployG_W2)]

mgam.mr1 <- vgam(EmployG_W2 ~ s(AGE_W1, k = 5),
               family = multinomial(),
               data = acl, model = TRUE)

summary(mgam.mr1)


## ----fgam-mr1, fig.width=10, fig.height=10, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Generalized additive model for age and employment status as a 5 level unordered categorical outcome resulting in four distinct effects of age."----

par(mfrow = c(2, 2))
plot(mgam.mr1, se = TRUE,
     lcol = viridis(4)[1],
     scol = viridis(4)[2])


## ------------------------------------------------------------------------

## generate new data for prediction
## use the whole range of age
newdat <- as.data.table(expand.grid(
  AGE_W1 = seq(
    from = min(acl$AGE_W1, na.rm=TRUE),
    to = max(acl$AGE_W1, na.rm=TRUE),
    length.out = 1000)))

newdat <- cbind(newdat, predict(mgam.mr1,
                newdata = newdat,
                type = "response"))

newdatlong <- melt(newdat, id.vars = "AGE_W1")

summary(newdatlong)


## ----fgam-mr1pred, fig.width=8, fig.height=7, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Predicted probability of employment status across ages."----

ggplot(newdatlong, aes(
  AGE_W1, value,
  colour = variable, linetype = variable)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_continuous("Age (years)") +
  scale_y_continuous("Probability", label = percent) +
  coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  theme_tufte() +
  theme(legend.position = c(.2, .5),
        legend.key.width = unit(2, "cm"))


## ------------------------------------------------------------------------

## negative binomial regression model
mgam.nbr1 <- vgam(NChronic12_W2 ~ Sex + s(AGE_W1, k = 5),
              family = negbinomial(),
              data = acl, model = TRUE)

summary(mgam.nbr1)


## ----fgam-nbr1, fig.width=10, fig.height=6, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Generalized additive model for sex an age and number of chronic conditions.", warning=FALSE----

par(mfrow = c(1, 2))
plot(mgam.nbr1, se = TRUE,
     lcol = viridis(4)[1],
     scol = viridis(4)[2])


## ------------------------------------------------------------------------

## generate new data for prediction
## use the whole range of age and sex
newdat <- as.data.table(expand.grid(
  Sex = levels(acl$Sex),
  AGE_W1 = seq(
    from = min(acl$AGE_W1, na.rm=TRUE),
    to = max(acl$AGE_W1, na.rm=TRUE),
    length.out = 1000)))

newdat$yhat <- predict(mgam.nbr1,
                       newdata = newdat,
                       type = "response")


## ----fgam-nbr1pred, fig.width=6, fig.height=5, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Predicted number of chronic conditions across ages by sex."----

ggplot(newdat, aes(AGE_W1, yhat, colour = Sex)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Age (years)") +
  ylab("Number Chronic Conditions") +
  theme_tufte() +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, 2.5),
                  expand = FALSE) +
  theme(legend.position = c(.2, .8),
        legend.key.width = unit(1, "cm"))


## ------------------------------------------------------------------------

detach("package:VGAM")
library(mgcv)

mgam.nbr2 <- gam(NChronic12_W2 ~ Sex + s(AGE_W1, k = 10, by = Sex),
              family = nb(), data = acl)

summary(mgam.nbr2)


## ------------------------------------------------------------------------

## generate new data for prediction
## use the whole range of age and sex
newdat <- as.data.table(expand.grid(
  Sex = levels(acl$Sex),
  AGE_W1 = seq(
    from = min(acl$AGE_W1, na.rm=TRUE),
    to = max(acl$AGE_W1, na.rm=TRUE),
    length.out = 1000)))

newdat$yhat <- predict(mgam.nbr2,
                       newdata = newdat,
                       type = "response")


## ----fgam-nbr2pred, fig.width=6, fig.height=5, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Predicted number of chronic conditions across ages by sex from an interaction model."----

ggplot(newdat, aes(AGE_W1, yhat, colour = Sex)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Age (years)") +
  ylab("Number Chronic Conditions") +
  theme_tufte() +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, 2.7),
                  expand = FALSE) +
  theme(legend.position = c(.2, .8),
        legend.key.width = unit(1, "cm"))


## ------------------------------------------------------------------------

nboot <- 500

out <- matrix(NA_real_, ncol = nboot, nrow = nrow(newdat))

start.time <- proc.time()
set.seed(12345)
for (i in 1:500) {
  tmp <- gam(NChronic12_W2 ~ Sex + s(AGE_W1, k = 10, by = Sex),
              family = nb(),
             data = acl[sample(nrow(acl), replace = TRUE)])
  out[, i] <- predict(tmp,
                      newdata = newdat,
                      type = "response")
}
stop.time <- proc.time()

## time to bootstrap 500 times
stop.time - start.time


## ------------------------------------------------------------------------

mean(abs(newdat$yhat - rowMeans(out)))


## ------------------------------------------------------------------------

newdat$LL <- apply(out, 1, quantile,
  probs = .025, na.rm = TRUE)

newdat$UL <- apply(out, 1, quantile,
  probs = .975, na.rm = TRUE)


## ----fgam-nbr1predboot, fig.width=7, fig.height=6, out.width='1\\linewidth', fig.pos="!h", fig.cap = "Predicted count of chronic conditions across ages by sex with bootstrapped confidence intervals."----

ggplot(newdat, aes(AGE_W1, yhat)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Sex), alpha = .2) +
  geom_line(aes(colour = Sex), size = 2) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Age (years)") +
  ylab("Number Chronic Conditions") +
  theme_tufte() +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, 4),
                  expand = FALSE) +
  theme(legend.position = c(.2, .8),
        legend.key.width = unit(2, "cm"))


## ------------------------------------------------------------------------

xtabs(~Sex + I(AGE_W1 > 80), data = acl[!is.na(NChronic12_W2)])



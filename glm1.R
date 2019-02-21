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
library(visreg)
library(ez)
library(emmeans)
library(rms)
library(ipw)
library(JWileymisc)
library(RcppEigen)
library(texreg)

options(
  width = 70,
  stringsAsFactors = FALSE,
  datatable.print.nrows = 20,
  datatable.print.topn = 3,
  digits = 2)


## ------------------------------------------------------------------------
f1 <- y ~ x1 + x2 + x1:x2

update(f1, . ~ .)

update(f1, w ~ .)

update(f1, . ~ . + x3)

update(f1, . ~ . - x1:x2)


## ------------------------------------------------------------------------

set.seed(1234)
example <- data.table(
  y = rnorm(9),
  Condition = factor(rep(c("A", "B", "Control"), each = 3),
                     levels = c("Control", "A", "B")))

coef(lm(y ~ Condition, data = example))


## ------------------------------------------------------------------------

example[, .(M = mean(y)), by = Condition]


## ----include=FALSE-------------------------------------------------------

printq <- sprintf("$%0.2f = %0.2f - %0.2f$",
                  coef(lm(y ~ Condition, data = example))[["ConditionA"]],
                  mean(example[Condition == "A", y]),
                  mean(example[Condition == "Control", y]))


## ------------------------------------------------------------------------

model.matrix(~ 0 + Condition, data = example)


## ------------------------------------------------------------------------

coef(lm(y ~ 0 + Condition, data = example))


## ------------------------------------------------------------------------

pf(.72, df1 = 1, df2 = 6, lower.tail = FALSE)


## ------------------------------------------------------------------------

mtcars <- as.data.table(mtcars)
mtcars[, ID := factor(1:.N)]
mtcars[, vs := factor(vs)]
mtcars[, am := factor(am)]

head(model.matrix(~ vs * am, data = mtcars))


## ------------------------------------------------------------------------

example[, ID := factor(1:.N)]

print(ezANOVA(
  data = example,
  dv = y,
  wid = ID,
  between = Condition,
  type = 3,
  detailed = TRUE))


## ------------------------------------------------------------------------

print(ezANOVA(
  data = mtcars,
  dv = mpg,
  wid = ID,
  between = vs * am,
  type = 3,
  detailed = TRUE))


## ----fglm1-tukeyhsd, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Graph of cell means with confidence intervals. Cells that share letters are not statistically significantly different based on Tukey's Honestly Significant Difference."----

mtcars[, Cells := factor(sprintf("vs=%s, am=%s", vs, am))]
TukeyHSDgg("Cells", "hp", mtcars) +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
  xlab("")


## ----fglm1-density, fig.width=5, fig.height=5, out.width='.5\\linewidth', fig.pos="!h", fig.cap = "Density plot of satisfaction with life (black line) with normal density overlayed (blue line)."----

acl <- readRDS("advancedr_acl_data.RDS")

testdistr(acl$SWL_W1, "normal",
          varlab = "Satisfaction with Life", plot = FALSE,
          extremevalues = "theoretical",
          adjust = 2)$DensityPlot


## ------------------------------------------------------------------------

m.ols <- ols(SWL_W1 ~ Sex + AGE_W1 + SESCategory, data = acl, x = TRUE)
m.ols


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## 
## texreg(m.ols, single.row = TRUE, label = "tglm1-olstex")
## 

## ----echo=FALSE, results='asis', listings=FALSE--------------------------

texreg(m.ols, single.row = TRUE, label = "tglm1-olstex", float.pos = "!hb")


## ------------------------------------------------------------------------

vif(m.ols)


## ----fglm1-quantreg, fig.width=5, fig.height=5, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = c("Graphing residuals to assess normality.", "Examining residuals versus fitted values with quantile regression to explore heteroskedasticity.")----

diagnostic.data <- data.table(
  fitted = fitted(m.ols),
  resid = residuals(m.ols))

testdistr(diagnostic.data$resid,
          "normal",
          varlab = "Satisfaction with Life Residuals", plot = FALSE,
          extremevalues = "theoretical",
          adjust = 2)$DensityPlot

ggplot(diagnostic.data, aes(fitted, resid)) +
  geom_point(alpha = .2, colour = "grey50") +
  geom_quantile(quantiles = .5, colour = 'black', size = 1) +
  geom_quantile(quantiles = c(.25, .75),
                colour = 'blue', linetype = 2, size = 1) +
  geom_quantile(quantiles = c(.05, .95),
                colour = 'black', linetype = 3, size = 1)


## ------------------------------------------------------------------------

m.glm <- glm(SWL_W1 ~ Sex + AGE_W1 + SESCategory,
             data=acl, family = gaussian(link="identity"))
m.glm

summary(m.glm)


## ------------------------------------------------------------------------

(m.ols2 <- update(m.ols, . ~ . + Employment_W1))


## ------------------------------------------------------------------------

anova(m.ols2)


## ------------------------------------------------------------------------

(m.ols3 <- update(m.ols2, . ~ . + AGE_W1 * SESCategory - Sex))


## ----fglm1-visreg1, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Estimated satisfaction with life across age by SES category. Shaded region indicates 95\\% confidence intervals for regression estimates."----

plot(visreg(m.ols3, xvar = "AGE_W1", by = "SESCategory",
            plot = FALSE), 
     overlay = TRUE, partial = FALSE, rug = FALSE, 
     xlab = "Age (years)", ylab = "Predicted Life Satisfaction",
     line = list(lty = 1:4))


## ----fglm1-visreg2, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Estimated satisfaction with life across age by SES category. Confidence intervals removed."----

plot(visreg(m.ols3, xvar = "AGE_W1", by = "SESCategory",
            plot = FALSE),
     overlay = TRUE, partial = FALSE, rug = FALSE, 
     xlab = "Age (years)", ylab = "Predicted Life Satisfaction",
     line = list(
       lty = 1:4, 
       col = c("black", "grey75", "grey50", "grey25")), 
     band = FALSE)


## ------------------------------------------------------------------------

newdata <- as.data.table(expand.grid(
  AGE_W1=quantile(acl$AGE_W1, .1):quantile(acl$AGE_W1, .9),
  SESCategory = factor(1:4, levels = levels(acl$SESCategory)),
  Employment_W1 = factor("(3) 15002499",
    levels = levels(acl$Employment_W1))))
newdata


## ------------------------------------------------------------------------

newdata[, c("SWL_W1", "SE") :=
          predict(m.ols3, newdata = newdata, se.fit = TRUE)]
newdata


## ------------------------------------------------------------------------

print(qnorm(.05/2), digits = 7)

print(qnorm(1 - (.05/2)), digits = 7)


## ----fglm1-intplot, fig.width=8, fig.height=5, out.width='.7\\linewidth', fig.pos="!h", fig.cap = "Estimated satisfaction with life across age by SES category. Shaded region indicates 95\\% confidence intervals for regression estimates."----

ggplot(newdata, aes(AGE_W1, SWL_W1, linetype=SESCategory)) +
  geom_ribbon(aes(ymin = SWL_W1 + SE * qnorm(.025),
                  ymax = SWL_W1 + SE * qnorm(.975)),
              alpha = .2) +
  geom_line(size = 1) +
  scale_x_continuous("Age (years)") +
  ylab("Satisfaction with Life") +
  theme_cowplot() +
  theme(
    legend.position = c(.8, .16),
    legend.key.width = unit(2, "cm"))


## ------------------------------------------------------------------------

confint(m.ols3)


## ------------------------------------------------------------------------

tmpdat <- na.omit(acl[, .(SWL_W1, AGE_W1, SESCategory, Employment_W1)])
## use if using Microsoft R Open with Intel's MKL linear algebra library
setMKLthreads(1)


## ------------------------------------------------------------------------

set.seed(12345)
t1 <- system.time(ols.boot <- sapply(1:500, function(i) {
  index <- sample(nrow(tmpdat),
                  size = nrow(tmpdat), replace = TRUE)
  coef(ols(SWL_W1 ~ AGE_W1 * SESCategory + Employment_W1,
           data = tmpdat[index]))
}))

t1


## ------------------------------------------------------------------------

set.seed(12345)
t2 <- system.time(rcpp.boot1 <- sapply(1:500, function(i) {
  index <- sample(nrow(tmpdat), size = nrow(tmpdat), replace = TRUE)
  coef(fastLm(SWL_W1 ~ AGE_W1 * SESCategory + Employment_W1, data = tmpdat[index]))
}))

t2


## ------------------------------------------------------------------------

set.seed(12345)
t3 <- system.time({
  y <- tmpdat[, SWL_W1]
  X <- model.matrix(~ AGE_W1 * SESCategory + Employment_W1, data = tmpdat)
  N <- nrow(tmpdat)
  rcpp.boot2 <- sapply(1:500, function(i) {
    index <- sample.int(N, size = N, replace = TRUE)
    fastLmPure(X = X[index, ], y = y[index])$coefficients
  })
})

t3


## ------------------------------------------------------------------------

set.seed(12345)
t4 <- system.time({
  y <- tmpdat[, SWL_W1]
  X <- model.matrix(~ AGE_W1 * SESCategory + Employment_W1, data = tmpdat)
  N <- nrow(tmpdat)
  rcpp.boot3 <- sapply(1:10000, function(i) {
    index <- sample.int(N, size = N, replace = TRUE)
    fastLmPure(X = X[index, ], y = y[index])$coefficients
  })
})

t4


## ------------------------------------------------------------------------

all.equal(ols.boot, rcpp.boot1, check.attributes = FALSE)

all.equal(ols.boot, rcpp.boot2, check.attributes = FALSE)


## ------------------------------------------------------------------------
m0 <- ols(CESD11_W2 ~ SelfEfficacy_W1, data = acl)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## texreg(m0, label = "tglm1-olsunadj")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
texreg(m0, label = "tglm1-olsunadj", float.pos = "!hb")

## ----fglm1-weights, fig.width=7, fig.height=8, out.width='.8\\linewidth', fig.pos="!h", fig.cap = "Raw and trimmed inverse probability weights for self-efficacy."----

## weights
w <- ipwpoint(
  exposure = SelfEfficacy_W1,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1 + NChronic12_W1,
  data = acl)
  
plot_grid(
  testdistr(w$ipw.weights, plot = FALSE)$DensityPlot,
  testdistr(winsorizor(w$ipw.weights, .01),
            plot = FALSE)$DensityPlot,
  ncol = 1)


## ----results = 'asis'----------------------------------------------------

## unweighted, unadjusted
m0 <- ols(CESD11_W2 ~ SelfEfficacy_W1, data = acl)

## weighted, adjusted 
m1 <- ols(CESD11_W2 ~ SelfEfficacy_W1, data = acl, 
  weights = winsorizor(w$ipw.weights, .01))

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## texreg(list(m0, m1),
##        label = "tglm1-weight1")
## 

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
texreg(list(m0, m1),
       label = "tglm1-weight1", float.pos = "!hb")

## ----results = 'asis'----------------------------------------------------

# weighted, fully adjusted
w2 <- ipwpoint(
  exposure = SelfEfficacy_W1,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1 + NChronic12_W1 + 
    SESCategory + Employment_W1 + BMI_W1 + Smoke_W1 + PhysActCat_W1,
  data = acl)
  
m2 <- ols(CESD11_W2 ~ SelfEfficacy_W1, data = acl, 
  weights = winsorizor(w2$ipw.weights, .01))

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## texreg(list(m0, m1, m2),
##        label = "tglm1-weight2")
## 

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
texreg(list(m0, m1, m2),
       label = "tglm1-weight2", float.pos = "!hb")

## ------------------------------------------------------------------------

m1b <- ols(CESD11_W2 ~ Sex + RaceEthnicity + AGE_W1 + 
  NChronic12_W1 + SelfEfficacy_W1, 
  data = acl)

m2b <- ols(CESD11_W2 ~ Sex + RaceEthnicity + AGE_W1 + 
  NChronic12_W1 + SESCategory + 
  Employment_W1 + BMI_W1 + Smoke_W1 + PhysActCat_W1 + 
  SelfEfficacy_W1, data = acl)


## ------------------------------------------------------------------------

m1c <- ols(CESD11_W2 ~ Sex + RaceEthnicity + AGE_W1 + 
  NChronic12_W1 + SelfEfficacy_W1, 
  data = acl, 
  weights = winsorizor(w$ipw.weights, .01))

m2c <- ols(CESD11_W2 ~ Sex + RaceEthnicity + AGE_W1 + 
  NChronic12_W1 + SESCategory + 
  Employment_W1 + BMI_W1 + Smoke_W1 + PhysActCat_W1 + 
  SelfEfficacy_W1, data = acl,
  weights = winsorizor(w2$ipw.weights, .01))


## ----fglm1-adjustcompare, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = "Comparison of the estimate and confidence interval for the association of self-efficacy with depression symptoms from various models. Covs = covariate adjusted models. IPW = inverse probability weight adjusted models. Covs + IPW = models that both include inverse probability weights and the same potential confounds explicitly in the model again."----

## write an extract function
extractor <- function(obj, label) {
  b <- coef(obj)
  ci <- confint(obj)
  data.table(
    Type = label,
    B = b[["SelfEfficacy_W1"]],
    LL = ci["SelfEfficacy_W1", "2.5 %"],
    UL = ci["SelfEfficacy_W1", "97.5 %"])
}

allresults <- rbind(
  extractor(m0,  "M0: Unadjusted"),
  extractor(m1,  "M1: Partial IPW"),
  extractor(m1b, "M1: Partial Covs"),
  extractor(m1c, "M1: Partial Covs + IPW"),
  extractor(m2,  "M2: Full IPW"),
  extractor(m2b, "M2: Full Covs"),
  extractor(m2c, "M2: Full Covs + IPW"))
allresults[, Type := factor(Type, levels = Type)]


ggplot(allresults, aes(Type, y = B, ymin = LL, ymax = UL)) + 
  geom_pointrange() + 
  coord_flip() + 
  xlab("") + ylab("Estimate + 95% CI")
  

## ------------------------------------------------------------------------

set.seed(12345)
adosleep <- data.table(
  SOLacti = rnorm(150, 4.4, 1.3)^2,
  DBAS = rnorm(150, 72, 26),
  DAS = rnorm(150, 125, 32),
  Female = rbinom(150, 1, .53),
  Stress = rnorm(150, 32, 11))
adosleep[, SSQ := rnorm(150,
             (.36 * 3 / 12.5) * SOLacti +
             (.16 * 3 / 26) * DBAS +
             (.18 * 3 / .5) * Female +
             (.20 * 3 / 11) * Stress, 2.6)]
adosleep[, MOOD := rnorm(150,
             (-.07 / 12.5) * SOLacti +
             (.29  / 3) * SSQ +
             (.14  / 26) * DBAS +
             (.21  / 32) * DAS +
             (.12  / 32) * SSQ * (DAS-50) +
             (.44  / .5) * Female +
             (.28 / 11) * Stress, 2)]
adosleep[, Female := factor(Female, levels = 0:1,
                            labels = c("Males", "Females"))]


## ----include=FALSE-------------------------------------------------------
## ols(scale(MOOD) ~ scale(SOLacti) + scale(SSQ) + scale(DBAS) + scale(DAS) + scale(SSQ):scale(DAS) + scale(Female) + scale(Stress), data = adosleep)
## ols(MOOD ~ SOLacti + SSQ + DBAS + DAS + SSQ:DAS + Female + Stress, data = adosleep)

## ----fglm1-case1, fig.width=8, fig.height=8, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Distributions of case study variables."----

plot_grid(
  testdistr(adosleep$MOOD, extremevalues = "theoretical",
            plot=FALSE, varlab = "MOOD")$Density,
  testdistr(adosleep$SSQ, extremevalues = "theoretical",
            plot=FALSE, varlab = "SSQ")$Density,
  testdistr(adosleep$SOLacti, extremevalues = "theoretical",
            plot=FALSE, varlab = "SOLacti")$Density,
  testdistr(adosleep$DAS, extremevalues = "theoretical",
            plot=FALSE, varlab = "DAS")$Density,
  ncol = 2)


## ----fglm1-case2, fig.width=6, fig.height=4.5, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Heatmap of the correlations between study variables."----

plot(SEMSummary(
  ~ MOOD + SOLacti + DBAS + DAS + Stress + SSQ,
  data = adosleep), plot = "cor") +
  theme(axis.text.x = element_text(
          angle = 45, hjust = 1, vjust = 1))


## ------------------------------------------------------------------------

egltable(c("SOLacti", "SSQ", "MOOD", "Stress",
           "DBAS", "DAS", "Female"),
         data = as.data.frame(adosleep))


## ------------------------------------------------------------------------

adosleep[, zMOOD := as.vector(scale(MOOD))]
adosleep[, zDBAS := as.vector(scale(DBAS))]
adosleep[, zDAS := as.vector(scale(DAS))]
adosleep[, zSSQ := as.vector(scale(SSQ))]
adosleep[, zSOLacti := as.vector(scale(SOLacti))]
adosleep[, zStress := as.vector(scale(Stress))]


## ------------------------------------------------------------------------

m.adosleep1 <- ols(zMOOD ~ zSOLacti + zDBAS + Female + zStress,
                   data = adosleep)
m.adosleep2 <- update(m.adosleep1, . ~ . + zSSQ + zDAS)
m.adosleep3 <- update(m.adosleep2, . ~ . + zSSQ:zDAS)

screenreg(list(m.adosleep1, m.adosleep2, m.adosleep3))


## ----fglm1-case-resids, fig.width=5, fig.height=5, out.width='.5\\linewidth', fig.pos="!h", fig.cap = "Distribution of model residuals."----

vif(m.adosleep3)

testdistr(resid(m.adosleep3), plot=FALSE, varlab = "Residuals")$QQPlot


## ------------------------------------------------------------------------

## refit model on raw data
m.adosleep.raw <- ols(MOOD ~ SOLacti + DBAS + Female +
                        Stress + SSQ * DAS,
                      data = adosleep)

## create a dataset
adosleep.newdat <- as.data.table(with(adosleep, expand.grid(
  SOLacti = mean(SOLacti),
  DBAS = mean(DBAS),
  Female = factor("Females", levels(Female)),
  Stress = mean(Stress),
  SSQ = seq(from = min(SSQ), to = max(SSQ), length.out = 100),
  DAS = mean(DAS) + c(1, -1) * sd(DAS))))

adosleep.newdat$MOOD <- predict(m.adosleep.raw,
                                newdata = adosleep.newdat,
                                se.fit = FALSE)

adosleep.newdat[, DAS := factor(round(DAS),
  levels = c(100, 161),
  labels = c("M - 1 SD", "M + 1 SD"))]


## ----fglm1-case-intplot, fig.width=5, fig.height=5, out.width='.5\\linewidth', fig.pos="!h", fig.cap = "Interaction between subjective sleep quality and overall dysfunctional beliefs predicting negative mood."----

ggplot(adosleep.newdat, aes(SSQ, MOOD, linetype=DAS)) +
  geom_line(size = 2) +
  scale_x_continuous("Subjective sleep quality\n(higher is worse)") +
  ylab("Negative Mood") +
  theme_cowplot() +
  theme(
    legend.position = c(.85, .15),
    legend.key.width = unit(2, "cm"))



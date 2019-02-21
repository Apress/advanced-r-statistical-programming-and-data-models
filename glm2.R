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
library(VGAM)
library(ipw)
library(JWileymisc)
library(xtable)
library(texreg)

options(
  width = 70,
  stringsAsFactors = FALSE,
  datatable.print.nrows = 20,
  datatable.print.topn = 3,
  digits = 2)


## ----fglm2-poisson, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Density for a Poisson distribution with lambda = 2 and lambda = 6."----

dpoisson <- data.table(X = 0:20)
dpoisson[, Lambda2 := dpois(X, lambda = 2)]
dpoisson[, Lambda6 := dpois(X, lambda = 6)]

ggplot(melt(dpoisson, id.vars = "X"), 
       aes(X, value, fill = variable)) + 
  geom_col(position = "dodge") + 
  scale_fill_viridis(discrete = TRUE) + 
  theme(legend.position = c(.7, .8)) + 
  xlab("Y Score") + ylab("Poisson Density")


## ------------------------------------------------------------------------

acl <- readRDS("advancedr_acl_data.RDS")


## ------------------------------------------------------------------------

acl$CurSmoke <- as.integer(acl$Smoke_W1 == "(1) Cur Smok")

m.lr <- vglm(CurSmoke ~ Sex, 
             family = binomialff(link = "logit"),
             data = acl, model = TRUE)
summary(m.lr)


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## or.tab <- xtabs(~ Sex + CurSmoke, data = acl)
## or.tab.res <- (or.tab[1,1]/or.tab[2,1])/(or.tab[1,2]/or.tab[2,2])
## xtable(or.tab, caption = "Observed frequency table",
##        label = "tglm2-obsfreq")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
or.tab <- xtabs(~ Sex + CurSmoke, data = acl)
or.tab.res <- (or.tab[1,1]/or.tab[2,1])/(or.tab[1,2]/or.tab[2,2])
xtable(or.tab, caption = "Observed frequency table",
       label = "tglm2-obsfreq")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------

cat(sprintf("
\\begin{equation*}
  \\frac{\\frac{%d}{%d}}{\\frac{%d}{%d}} = %0.2f
\\end{equation*}
", or.tab[1,1], or.tab[2,1], or.tab[1,2], or.tab[2,2],
or.tab.res
))


## ------------------------------------------------------------------------

preddat <- data.table(Sex = levels(acl$Sex))
preddat$yhat <- predict(m.lr, newdata = preddat,
        type = "response")


## ----fglm2-predprob0, fig.width=5, fig.height=5, out.width='.5\\linewidth', fig.pos="!h", fig.cap = "Graph showing the probability of smoking by sex."----

ggplot(preddat, aes(Sex, yhat)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Smoking Probability", labels = percent) +
  theme_tufte()


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(coef(summary(m.lr)), digits = 2,
##        caption = paste(
##   "Summary of logistic regression model",
##   "including coefficients, standard errors",
##   "andd p-values."), label = "tglm2-orsimple")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(coef(summary(m.lr)), digits = 2, 
       caption = paste(
  "Summary of logistic regression model",
  "including coefficients, standard errors",
  "andd p-values."), label = "tglm2-orsimple")

## ------------------------------------------------------------------------

## unadjusted model
m0.lr <- vglm(CurSmoke ~ SelfEfficacy_W1, 
             family = binomialff(link = "logit"),
             data = acl, model = TRUE)

## estimate IPWs
w <- ipwpoint(
  exposure = SelfEfficacy_W1,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1,
  data = acl)

## adjusted logistic regression model
m1.lr <- vglm(CurSmoke ~ SelfEfficacy_W1, 
             family = binomialff(link = "logit"),
             data = acl, model = TRUE, 
             weights = winsorizor(w$ipw.weights, .01))

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(rbind(
##   data.table(Type = "Raw", coef(summary(m0.lr))),
##   data.table(Type = "Adj", coef(summary(m1.lr)))),
##   digits = 2,
##   caption = paste("Comparison of unadjusted (raw)",
##     "and adjusted regression models"),
##   label = "tglm2-lrcompare")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(rbind(
  data.table(Type = "Raw", coef(summary(m0.lr))),
  data.table(Type = "Adj", coef(summary(m1.lr)))),
  digits = 2, 
  caption = paste("Comparison of unadjusted (raw)",
    "and adjusted regression models"),
  label = "tglm2-lrcompare")

## ----fglm2-predprob1, fig.width=5, fig.height=5, out.width='.6\\linewidth', fig.pos="!h", fig.cap = c("Graph showing the probability of smoking by self-efficacy")----

preddat2 <- data.table(SelfEfficacy_W1 = 
  seq(from = min(acl$SelfEfficacy_W1, na.rm = TRUE),
      to = max(acl$SelfEfficacy_W1, na.rm = TRUE),
      length.out = 1000))
preddat2$yhat <- predict(m1.lr, newdata = preddat2,
                        type = "response")

ggplot(preddat2, aes(SelfEfficacy_W1, yhat)) +
  geom_line() +
  scale_x_continuous("Self-Efficacy") +
  scale_y_continuous("Smoking Probability", label = percent) +
  theme_tufte() + coord_cartesian(ylim = c(.25, .40))


## ------------------------------------------------------------------------

## delta value for change in self efficacy
delta <- .01

## create a copy of the dataset
## where we increase everyone's self-efficacy by delta
aclalt <- copy(acl)
aclalt$SelfEfficacy_W1 <- aclalt$SelfEfficacy_W1 + delta

## calculate predicted probabilities
p1 <- predict(m1.lr, newdata = acl, type = "response")
p2 <- predict(m1.lr, newdata = aclalt, type = "response")

## calculate the average, marginal change in probabilities 
## per unit change in self efficacy
## in percents and rounded
round(mean((p2 - p1) / delta) * 100, 1)


## ------------------------------------------------------------------------

acl$PhysActCat_W2 <- factor(acl$PhysActCat_W2, ordered = TRUE)

## adjusted ordered logistic regression model
m0.or <- vglm(PhysActCat_W2 ~ SelfEfficacy_W1,
              family = propodds(),
              data = acl)

## estimate IPWs
w <- ipwpoint(
  exposure = SelfEfficacy_W1,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1,
  data = acl)

## adjusted ordered logistic regression model
m1.or <- vglm(PhysActCat_W2 ~ SelfEfficacy_W1, 
             family = propodds(),
             data = acl, model = TRUE, 
             weights = winsorizor(w$ipw.weights, .01))


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(rbind(
##   data.table(Type = "Raw",
##              Labels = rownames(coef(summary(m0.or))),
##              coef(summary(m0.or))),
##   data.table(Type = "Adj",
##              Labels = rownames(coef(summary(m1.or))),
##              coef(summary(m1.or)))),
##   digits = 2,
##   caption = paste("Comparison of unadjusted (raw) and",
##    "adjusted ordered logistic regression models"),
##   label = "tglm2-orcompare")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(rbind(
  data.table(Type = "Raw", 
             Labels = rownames(coef(summary(m0.or))),
             coef(summary(m0.or))),
  data.table(Type = "Adj", 
             Labels = rownames(coef(summary(m1.or))),
             coef(summary(m1.or)))),
  digits = 2, 
  caption = paste("Comparison of unadjusted (raw) and",
   "adjusted ordered logistic regression models"),
  label = "tglm2-orcompare")

## ----fglm2-predprob2, fig.width=5, fig.height=5, out.width='.6\\linewidth', fig.pos="!h", fig.cap = c("Graph showing the probability of different physical activity categories by self-efficacy")----

preddat3 <- data.table(SelfEfficacy_W1 = 
  seq(from = min(acl$SelfEfficacy_W1, na.rm = TRUE),
      to = max(acl$SelfEfficacy_W1, na.rm = TRUE),
      length.out = 1000))
preddat3 <- cbind(preddat3, 
  predict(m1.or, newdata = preddat3,
          type = "response"))
preddat3 <- melt(preddat3, id.vars = "SelfEfficacy_W1")

ggplot(preddat3, aes(SelfEfficacy_W1, value, 
                     colour = variable, linetype = variable)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) + 
  scale_x_continuous("Self-Efficacy") +
  scale_y_continuous("Activity Probability", label = percent) +
  coord_cartesian(ylim = c(0, .6), expand = FALSE) + 
  theme_tufte() + 
  theme(legend.position = c(.7, .8), 
        legend.key.width = unit(2, "cm"))


## ------------------------------------------------------------------------

## delta value for change in self efficacy
delta <- .01

## create a copy of the dataset
## where we increase everyone's self-efficacy by delta
aclalt <- copy(acl)
aclalt$SelfEfficacy_W1 <- aclalt$SelfEfficacy_W1 + delta

## calculate predicted probabilities
p1 <- predict(m1.or, newdata = acl, type = "response")
p2 <- predict(m1.or, newdata = aclalt, type = "response")

## average marginal change in probability of 
## membership in each category
round(colMeans((p2 - p1) / delta) * 100, 1)


## ------------------------------------------------------------------------

acl[, EmployG_W2 := as.character(Employment_W2)]
acl[EmployG_W2 %in% c(
  "(2) 2500+HRS", "(3) 15002499", 
  "(4) 500-1499", "(5) 1-499HRS"), 
  EmployG_W2 := "(2) EMPLOYED"]
acl[, EmployG_W2 := factor(EmployG_W2)]


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(as.data.frame(table(acl$EmployG_W2)),
##        caption = "Frequency table of employment",
##        label = "tglm2-freqtab")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(as.data.frame(table(acl$EmployG_W2)),
       caption = "Frequency table of employment",
       label = "tglm2-freqtab")

## ------------------------------------------------------------------------

## unadjusted multinomial logistic regression model
m0.mr <- vglm(EmployG_W2 ~ SelfEfficacy_W1,
              family = multinomial(),
              data = acl, model = TRUE)

## estimate IPWs
w <- ipwpoint(
  exposure = SelfEfficacy_W1,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1,
  data = acl)

## adjusted multinomial logistic regression model
m1.mr <- vglm(EmployG_W2 ~ SelfEfficacy_W1,
              family = multinomial(),
             data = acl, model = TRUE, 
             weights = winsorizor(w$ipw.weights, .01))


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(rbind(
##   data.table(Type = "Raw",
##              Labels = rownames(coef(summary(m0.mr))),
##              coef(summary(m0.mr))),
##   data.table(Type = "Adj",
##              Labels = rownames(coef(summary(m1.mr))),
##              coef(summary(m1.mr)))),
##   digits = 2,
##   caption = paste("Comparison of unadjusted (raw) and",
##    "adjusted multinomial  logistic regression models"),
##   label = "tglm2-mrcompare")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(rbind(
  data.table(Type = "Raw", 
             Labels = rownames(coef(summary(m0.mr))),
             coef(summary(m0.mr))),
  data.table(Type = "Adj", 
             Labels = rownames(coef(summary(m1.mr))),
             coef(summary(m1.mr)))),
  digits = 2, 
  caption = paste("Comparison of unadjusted (raw) and",
   "adjusted multinomial  logistic regression models"),
  label = "tglm2-mrcompare")

## ----fglm2-predprob3, fig.width=5, fig.height=5, out.width='.6\\linewidth', fig.pos="!h", fig.cap = c("Graph showing the probability of different employment categories by self-efficacy")----

preddat4 <- data.table(SelfEfficacy_W1 = 
  seq(from = min(acl$SelfEfficacy_W1, na.rm = TRUE),
      to = max(acl$SelfEfficacy_W1, na.rm = TRUE),
      length.out = 1000))
preddat4 <- cbind(preddat4, 
  predict(m1.mr, newdata = preddat4,
          type = "response"))
preddat4 <- melt(preddat4, id.vars = "SelfEfficacy_W1")

ggplot(preddat4, aes(
  SelfEfficacy_W1, value, 
  colour = variable, linetype = variable)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) + 
  scale_x_continuous("Self-Efficacy") +
  scale_y_continuous("Probability", label = percent) +
  coord_cartesian(ylim = c(0, .65), expand = FALSE) + 
  theme_tufte() + 
  theme(legend.position = c(.18, .82), 
        legend.key.width = unit(2, "cm"))


## ------------------------------------------------------------------------

## delta value for change in self efficacy
delta <- .01

## create a copy of the dataset
## where we increase everyone's self-efficacy by delta
aclalt <- copy(acl)
aclalt$SelfEfficacy_W1 <- aclalt$SelfEfficacy_W1 + delta

## calculate predicted probabilities
p1 <- predict(m1.mr, newdata = acl, type = "response")
p2 <- predict(m1.mr, newdata = aclalt, type = "response")

## average marginal change in probability of 
## membership in each category
round(colMeans((p2 - p1) / delta) * 100, 1)


## ------------------------------------------------------------------------

egltable(c("NChronic12_W1", "NChronic12_W2"), 
         data = acl, parametric = FALSE)


## ----fglm2-freqplot, fig.width=5, fig.height=6, out.width='.6\\linewidth', fig.pos="!h", fig.cap = c("Graph showing the frequency of each number of chronic conditions at each wave in the ACL data")----

plot_grid(
  ggplot(acl, aes(NChronic12_W1)) + 
  geom_bar() + theme_tufte(),
  ggplot(acl, aes(NChronic12_W2)) + 
  geom_bar() + theme_tufte(),
  ncol = 1, 
  labels = c("Wave 1", "Wave 2"),
  label_x = .8)


## ------------------------------------------------------------------------

## unadjusted poisson regression model
m0.pr <- vglm(NChronic12_W2 ~ SelfEfficacy_W1,
              family = poissonff(),
              data = acl, model = TRUE)

summary(m0.pr)


## ------------------------------------------------------------------------

## unadjusted negative binomial regression model
m0.nbr <- vglm(NChronic12_W2 ~ SelfEfficacy_W1,
              family = negbinomial(),
              data = acl, model = TRUE)

AIC(m0.nbr) - AIC(m0.pr)
BIC(m0.nbr) - BIC(m0.pr)


## ----fglm2-simplot, fig.width=6, fig.height=5, out.width='.6\\linewidth', fig.pos="!h", fig.cap = c("Graph showing the frequency of each number of chronic conditions based on the true data, simulations from the negative binomial model, and simulations from the poisson regression model.")----

test.pr <- simulate(m0.pr, nsim = 1, seed = 1234)$sim_1
test.nbr <- simulate(m0.nbr, nsim = 1, seed = 1234)$sim_1
test.all <- data.table(
  Type = rep(c("Truth", "Poisson", "Negative\nBinomial"),
             times = c(
               nrow(model.frame(m0.pr)), 
               length(test.pr),
               length(test.nbr))),
  Score = c(
    model.frame(m0.pr)$NChronic12_W2,
    test.pr,
    test.nbr))

ggplot(test.all, aes(Score, fill = Type)) + 
  geom_bar(position = "dodge") + 
  scale_fill_viridis(discrete = TRUE) + 
  theme_tufte() + 
  theme(legend.position = c(.8, .8))


## ------------------------------------------------------------------------

## estimate IPWs
w <- ipwpoint(
  exposure = SelfEfficacy_W1,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1,
  data = acl)

## adjusted negative binomial regression model
m1.nbr <- vglm(NChronic12_W2 ~ SelfEfficacy_W1,
              family = negbinomial(),
             data = acl, model = TRUE, 
             weights = winsorizor(w$ipw.weights, .01))


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(rbind(
##   data.table(Type = "Raw",
##              Labels = rownames(coef(summary(m0.nbr))),
##              coef(summary(m0.nbr))),
##   data.table(Type = "Adj",
##              Labels = rownames(coef(summary(m1.nbr))),
##              coef(summary(m1.nbr)))),
##   digits = 2,
##   caption = paste("Comparison of unadjusted (raw) and",
##    "adjusted negative binomial regression models"),
##   label = "tglm2-nbrcompare")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(rbind(
  data.table(Type = "Raw", 
             Labels = rownames(coef(summary(m0.nbr))),
             coef(summary(m0.nbr))),
  data.table(Type = "Adj", 
             Labels = rownames(coef(summary(m1.nbr))),
             coef(summary(m1.nbr)))),
  digits = 2, 
  caption = paste("Comparison of unadjusted (raw) and",
   "adjusted negative binomial regression models"),
  label = "tglm2-nbrcompare")

## ----fglm2-predcount1, fig.width=5, fig.height=5, out.width='.6\\linewidth', fig.pos="!h", fig.cap = c("Graph showing the predicted number of chronic conditions as a function of self-efficacy.")----

preddat5 <- data.table(SelfEfficacy_W1 = 
  seq(from = min(acl$SelfEfficacy_W1, na.rm = TRUE),
      to = max(acl$SelfEfficacy_W1, na.rm = TRUE),
      length.out = 1000))
preddat5$yhat <- predict(m1.nbr, newdata = preddat5,
          type = "response")

ggplot(preddat5, aes(SelfEfficacy_W1, yhat)) + 
  geom_line() +
  scale_x_continuous("Self-Efficacy") +
  scale_y_continuous("Expected Number Conditions") + 
  theme_tufte()


## ------------------------------------------------------------------------

acl[, Smoke_W2W1 := NA_character_]
acl[Smoke_W1 == "(3) Nevr Smo" & 
    Smoke_W2 == "(3) W2 Never Smoker", 
    Smoke_W2W1 := "Stable Never Smoker"]
acl[Smoke_W1 == "(2) Past Smo" & 
    Smoke_W2 == "(2) W2 Former Smoker", 
    Smoke_W2W1 := "Stable Former Smoker"]
acl[Smoke_W1 == "(1) Cur Smok" & 
    Smoke_W2 == "(1) W2 Current Smoker", 
    Smoke_W2W1 := "Stable Current Smoker"]
acl[Smoke_W1 %in% c("(2) Past Smo", "(3) Nevr Smo") & 
    Smoke_W2 == "(1) W2 Current Smoker", 
    Smoke_W2W1 := "New Smoker"]
acl[Smoke_W1 == "(1) Cur Smok" & 
    Smoke_W2 == "(2) W2 Former Smoker", 
    Smoke_W2W1 := "Recently Quit Smoker"]

acl[, Smoke_W2W1 := factor(Smoke_W2W1,
  levels = c("Stable Never Smoker", "Stable Former Smoker",
             "Stable Current Smoker", "Recently Quit Smoker", 
             "New Smoker"))]

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(as.data.frame(table(acl$Smoke_W2W1)),
##        caption = "Frequency table of smoking over time",
##        label = "tglm2-freqtab-smoke")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(as.data.frame(table(acl$Smoke_W2W1)),
       caption = "Frequency table of smoking over time",
       label = "tglm2-freqtab-smoke")

## ------------------------------------------------------------------------

acl[, SES := as.numeric(SESCategory)]

mr.ses <- vglm(Smoke_W2W1 ~ Sex + SES + AGE_W1,
  family = multinomial(),
  data = acl, model = TRUE)

mr.psych <- vglm(Smoke_W2W1 ~ SWL_W1 + InformalSI_W1 + 
  FormalSI_W1 + SelfEfficacy_W1 + CESD11_W1,
  family = multinomial(),
  data = acl, model = TRUE)

mr.health <- vglm(Smoke_W2W1 ~ PhysActCat_W1 + 
  BMI_W1 + NChronic12_W1,
  family = multinomial(),
  data = acl, model = TRUE)


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(
##  data.table(
##   Model = c("Sociodemographics", "Psychosocial",  "Health"),
##   AIC = c(AIC(mr.ses), AIC(mr.psych), AIC(mr.health)),
##   BIC = c(BIC(mr.ses), BIC(mr.psych), BIC(mr.health))),
##   caption = "Model Comparisons",
##   label = "tglm2-modelcomparisons")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(
 data.table(
  Model = c("Sociodemographics", "Psychosocial",  "Health"),
  AIC = c(AIC(mr.ses), AIC(mr.psych), AIC(mr.health)),
  BIC = c(BIC(mr.ses), BIC(mr.psych), BIC(mr.health))),
  caption = "Model Comparisons",
  label = "tglm2-modelcomparisons")

## ------------------------------------------------------------------------

summary(mr.ses)


## ------------------------------------------------------------------------

acl[, AGE_W1 := AGE_W1 / 10]


## ------------------------------------------------------------------------

mr.ses1 <- vglm(Smoke_W2W1 ~ Sex + SES + AGE_W1,
              family = multinomial(refLevel = 1),
              data = acl, model = TRUE)
mr.ses2 <- update(mr.ses1, 
                  family = multinomial(refLevel = 2))
mr.ses3 <- update(mr.ses1, 
                  family = multinomial(refLevel = 3))


## ------------------------------------------------------------------------
data.table(
  Ref = "Stable Never Smoker",
  Term = names(coef(mr.ses1)),
  OR = exp(coef(mr.ses1)),
  exp(confint(mr.ses1)))

data.table(
  Ref = "Stable Current Smoker",
  Term = names(coef(mr.ses3)),
  OR = exp(coef(mr.ses3)),
  exp(confint(mr.ses3)))


## ------------------------------------------------------------------------

## delta value for change in age and SES
delta <- .01

## create a copy of the dataset
## where we increase everyone's age by delta
aclage <- copy(acl)
aclage[, AGE_W1 := AGE_W1 + delta]

## create a copy of the dataset
## where we increase everyone's SES by delta
aclses <- copy(acl)
aclses[, SES := SES + delta]

## create two copies of the data
## one where se set everyone to "female" and another to "male"
aclfemale <- copy(acl)
aclfemale[, Sex := factor("(2) FEMALE", 
                          levels = levels(acl$Sex))]

aclmale <- copy(acl)
aclmale[, Sex := factor("(1) MALE", 
                        levels = levels(acl$Sex))]

## calculate predicted probabilities
p.ref <- predict(mr.ses1, newdata = acl, 
                 type = "response")
p.age <- predict(mr.ses1, newdata = aclage, 
                 type = "response")
p.ses <- predict(mr.ses1, newdata = aclses, 
                 type = "response")
p.female <- predict(mr.ses1, newdata = aclfemale, 
                    type = "response")
p.male <- predict(mr.ses1, newdata = aclmale, 
                    type = "response")


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## xtable(
## data.table(
##   Level = colnames(p.ref),
##   Age = colMeans((p.age - p.ref) / delta) * 100,
##   SES = colMeans((p.ses - p.ref) / delta) * 100,
##   Female = colMeans(p.female - p.male) * 100),
##   digits = 2,
##   caption = "Average marginal change in predicted probability",
##   label = "tglm2-margprobs")

## ----echo=FALSE, results='asis', listings=FALSE--------------------------
xtable(
data.table(
  Level = colnames(p.ref),
  Age = colMeans((p.age - p.ref) / delta) * 100,
  SES = colMeans((p.ses - p.ref) / delta) * 100,
  Female = colMeans(p.female - p.male) * 100),
  digits = 2,
  caption = "Average marginal change in predicted probability",
  label = "tglm2-margprobs")


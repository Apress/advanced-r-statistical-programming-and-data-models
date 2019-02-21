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
library(MASS)
library(mvtnorm)
library(mgcv)
library(quantreg)
library(JWileymisc)
library(data.table)

options(width = 70, digits = 2)


## ----fmdv-density2d, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "2D empirical density plot for multivariate normal data."----

mu <- c(0, 0)
sigma <- matrix(c(1, .5, .5, 1), 2)

set.seed(1234)
d <- as.data.table(rmvnorm(500, mean = mu, sigma = sigma))
setnames(d, names(d), c("x", "y"))

ggplot(d, aes(x, y)) +
  geom_point(colour = "grey60") +
  geom_density2d(size = 1, colour = "black") +
  theme_cowplot()


## ----fmdv-density2dnorm, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "2D empirical density versus multivariate normal density plot."----

testd <- as.data.table(expand.grid(
  x = seq(from = min(d$x), to = max(d$x), length.out = 50),
  y = seq(from = min(d$y), to = max(d$y), length.out = 50)))
testd[, Density := dmvnorm(cbind(x, y), mean = colMeans(d), sigma = cov(d))]

ggplot(d, aes(x, y)) +
  geom_contour(aes(x, y, z = Density), data = testd,
               colour = "blue", size = 1, linetype = 2) +
  geom_density2d(size = 1, colour = "black") +
  theme_cowplot()


## ----fmdv-uninorm, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Univariate density plots showing the simulated variables are univariate normal."----

set.seed(1234)
d2 <- data.table(x = rnorm(500))
d2[, y := ifelse(abs(x) > 1, x, -x)]

plot_grid(
  testdistr(d2$x, plot = FALSE)$Density,
  testdistr(d2$y, plot = FALSE, varlab = "Y")$Density,
  ncol = 2)


## ----fmdv-2dnotnorm, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "2D density plot showing data that are not multivariate normal."----

testd2 <- as.data.table(expand.grid(
  x = seq(from = min(d2$x), to = max(d2$x), length.out = 50),
  y = seq(from = min(d2$y), to = max(d2$y), length.out = 50)))
testd2[, Density := dmvnorm(cbind(x, y), mean = colMeans(d2), sigma = cov(d2))]

ggplot(d2, aes(x, y)) +
  geom_contour(aes(x, y, z = Density), data = testd2,
               colour = "blue", size = 1, linetype = 2) +
  geom_density2d(size = 1, colour = "black") +
  theme_cowplot()


## ----fmdv-testdistr-mvnorm, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = c("Density plot superimposing multivariate normal distribution and QQ plot, showing multivariate normal data.", "Density plot superimposing multivariate normal distribution and QQ plot, showing data that are not multivariate normal.")----

testdistr(d, "mvnorm", ncol = 2)

testdistr(d2, "mvnorm", ncol = 2)


## ----fmdv-mtcars-mvnorm, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Density plot superimposing multivariate normal distribution and QQ plot for mtcars data."----

testdistr(mtcars, "mvnorm", ncol = 2)


## ----fmdv-uvoutlier, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Density plot superimposing normal distribution for data with an anomalous value (Panel A) and with the anomalous value removed (Panel B)."----

mu <- c(0, 0, 0)
sigma <- matrix(.7, 3, 3)
diag(sigma) <- 1

set.seed(12345)
d <- as.data.table(rmvnorm(200, mean = mu, sigma = sigma))[order(V1)]
d[c(1, 200), V3 := c(2.2, 50)]

plot_grid(
  testdistr(d$V3, extremevalues = "theoretical", plot=FALSE)$Density,
  testdistr(d[V3 < 40]$V3, extremevalues = "theoretical", plot=FALSE)$Density,
  ncol = 2, labels = c("A", "B"))


## ----fmdv-mvoutliers, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = paste0("Graph of multivariate normality and (multivariate) anomalous values", c(".", " with one extreme anomalous value removed."))----

testdistr(d, "mvnorm", ncol = 2, extremevalues = "theoretical")

testdistr(d[V3 < 40], "mvnorm", ncol = 2, extremevalues = "theoretical")


## ----fmdv-mvrobust, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = paste0("Graph of multivariate normality and (multivariate) anomalous values", c(" using the robust estimator.", " with both anomalous value removed."))----

testdistr(d, "mvnorm", ncol = 2, robust = TRUE, extremevalues = "theoretical")

testdistr(d[-c(1,200)], "mvnorm", ncol = 2, extremevalues = "theoretical")


## ----fmdv-boxplot-cut, fig.width=8, fig.height=8, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Boxplots from cutting a continuous variable into quartiles showing a non-linear relationship."----

set.seed(12345)
d2 <- data.table(x = rnorm(100))
d2[, y := rnorm(100, mean = 2 + x + 2 * x^2, sd = 3)]

p.cut3 <- ggplot(
  data = d2[, .(y,
    xcut = cut(x, quantile(x,
      probs = seq(0, 1, by = 1/3)), include.lowest = TRUE))],
  aes(xcut, y)) +
  geom_boxplot(width=.25) +
  theme(axis.text.x = element_text(
          angle = 45, hjust = 1, vjust = 1)) +
  xlab("")

p.cut4 <- p.cut3 %+% d2[, .(y,
    xcut = cut(x, quantile(x,
      probs = seq(0, 1, by = 1/4)), include.lowest = TRUE))]

p.cut5 <- p.cut3 %+% d2[, .(y,
    xcut = cut(x, quantile(x,
      probs = seq(0, 1, by = 1/5)), include.lowest = TRUE))]

p.cut10 <- p.cut3 %+% d2[, .(y,
    xcut = cut(x, quantile(x,
      probs = seq(0, 1, by= 1/10)), include.lowest = TRUE))]

plot_grid(
  p.cut3, p.cut4,
  p.cut5, p.cut10,
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  align = "hv")


## ----fmdv-loessline, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Loess line of best fit showing a non-linear relationship."----

ggplot(d2, aes(x, y)) +
  geom_point(colour="grey50") +
  stat_smooth(method = "loess", colour = "black") +
  stat_smooth(method = "lm", colour = "blue", linetype = 2)


## ----fmdv-loesslineq, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Loess line and quadratic line."----

ggplot(d2, aes(x, y)) +
  geom_point(colour="grey50") +
  stat_smooth(method = "loess", colour = "black") +
  stat_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              colour = "blue", linetype = 2)


## ----fmdv-loess-span, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Loess line with varying degree of smoothing."----

ggplot(d2, aes(x, y)) +
  geom_point(colour="grey50") +
  stat_smooth(method = "loess", span = .2,
              colour = "black") +
  stat_smooth(method = "loess", span = 2,
              colour = "black", linetype = 2)


## ----fmdv-loess-mvbv, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Loess line of bivariate relationship from multivariate data."----

set.seed(1234)
d3 <- data.table(
  x = rnorm(500),
  w = rnorm(500),
  z = rbinom(500, 1, .4))
d3[, y := rnorm(500, mean = 3 +
       ifelse(x < 0 & w < 0, -2, 0) * x +
       ifelse(x < 0, 0, 2) * w * x^2 + 4 * z * w,
    sd = 1)]
d3[, z := factor(z)]

ggplot(d3, aes(x, y)) +
  geom_point(colour="grey50") +
  stat_smooth(method = "loess", colour = "black")


## ----fmdv-loess-mv, fig.width=8, fig.height=8, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Loess lines for multivariate data."----

ggplot(d3, aes(x, winsorizor(y, .01), colour = z)) +
  geom_point() +
  stat_smooth(method = "loess") +
  scale_colour_manual(values = c("1" = "black", "0" = "grey40")) +
  facet_wrap(~ cut(w, quantile(w), include.lowest = TRUE))


## ----fmdv-gam-pred, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Contour plots showing predicted y values for different combinations of x and w panelled by z."----

m <- gam(winsorizor(y, .01) ~ z + te(x, w, k = 7, by = z), data = d3)

newdat <- expand.grid(
  x = seq(min(d3$x), max(d3$x), length.out = 100),
  w = seq(min(d3$w), max(d3$w), length.out = 100),
  z = factor(0:1, levels = levels(d3$z)))

newdat$yhat <- predict(m, newdata = newdat)

ggplot(newdat, aes(x = x, y = w, z = yhat)) +
  geom_raster(aes(fill = yhat)) +
  geom_contour(colour = "white", binwidth = 1, alpha = .5) +
  facet_wrap(~ z)


## ------------------------------------------------------------------------

diris <- as.data.table(iris)
diris[, .(V = var(Sepal.Length)), by = Species]


## ----fmdv-slboxplot, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Box and Whisker Diagrams of sepal length by species.  Outliers appear as dots."----

plot_grid(
  ggplot(diris, aes(Species, Sepal.Length)) +
    geom_boxplot() +
    xlab(""),
  ggplot(diris[, .(Sepal.Length = Sepal.Length -
                              median(Sepal.Length)), by = Species],
         aes(Species, Sepal.Length)) +
    geom_boxplot() +
    xlab(""),
  ncol = 2, labels = c("A", "B"), align = "hv")


## ----fmdv-violinplot, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Violin plots with box and whisker diagrams in center."----

ggplot(diris, aes(Species, Sepal.Length)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  xlab("")


## ----fmdv-mvviolinplot, fig.width=8, fig.height=8, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Violin plots with box and whisker diagrams in center by quartiles of x, colored by z."----

## create cuts
d3[, xquartile := cut(x, quantile(x), include.lowest = TRUE)]
d3[, wquartile := cut(w, quantile(w), include.lowest = TRUE)]
d3[, yclean := winsorizor(y, .01)]

## median center y by group to facilitate comparison
d3[, yclean := yclean - median(yclean),
   by = .(xquartile, wquartile, z)]

p <- position_dodge(.5)

ggplot(d3, aes(xquartile, yclean, colour = z)) +
  geom_violin(position = p) +
  geom_boxplot(position = p, width = .1) +
  scale_colour_manual(values = c("1" = "black", "0" = "grey40")) +
  facet_wrap(~ wquartile) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) +
  coord_cartesian(ylim = c(-5, 5), expand = FALSE)


## ------------------------------------------------------------------------

set.seed(1234)
d4 <- data.table(x = runif(500, 0, 5))
d4[, y1 := rnorm(500, mean = 2 + x, sd = 1)]
d4[, y2 := rnorm(500, mean = 2 + x, sd = .25 + x)]


## ----fmdv-quantreg, fig.width=8, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Homoskedasticity versus Heteroskedasticity.", warning=FALSE----

plot_grid(
  ggplot(d4, aes(x, y1)) +
    geom_point(colour = "grey70") +
    geom_quantile(quantiles = .5, colour = 'black') +
    geom_quantile(quantiles = c(.25, .75),
                  colour = 'blue', linetype = 2) +
    geom_quantile(quantiles = c(.05, .95),
                  colour = 'black', linetype = 3),
  ggplot(d4, aes(x, y2)) +
    geom_point(colour = "grey70") +
    geom_quantile(quantiles = .5, colour = 'black') +
    geom_quantile(quantiles = c(.25, .75),
                  colour = 'blue', linetype = 2) +
    geom_quantile(quantiles = c(.05, .95),
                  colour = 'black', linetype = 3),
  ncol = 2, labels = c("A", "B"))



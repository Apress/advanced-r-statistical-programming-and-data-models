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
library(JWileymisc)
library(data.table)

options(width = 70, digits = 2)


## ----fudv-dotplot-mpg, fig.width=4, fig.height=1.5, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Stacked dot plot of miles per gallon from old cars."----

ggplot(mtcars, aes(mpg)) +
  geom_dotplot()


## ----fudv-hist-sl, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Histogram of sepal length from the iris data."----

ggplot(iris, aes(Sepal.Length)) +
  geom_histogram()


## ----fudv-hist-lynx-raw, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Histogram of annual Canadian lynx trappings."----

ggplot(data.table(lynx = as.vector(lynx)), aes(lynx)) +
  geom_histogram()


## ----fudv-hist-lynx-log, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Histogram of annual Canadian lynx trappings after a natural log transformation."----

ggplot(data.table(lynx = as.vector(lynx)), aes(log(lynx))) +
  geom_histogram()


## ----fudv-density, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "This is the density plot for our sepal lengths."----

ggplot(iris, aes(Sepal.Length)) +
  geom_density()


## ----fudv-densityadjust, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = c("A noisy density plot.", "A very smooth density plot.")----

ggplot(iris, aes(Sepal.Length)) +
  geom_density(adjust = .5)

ggplot(iris, aes(Sepal.Length)) +
  geom_density(adjust = 5)


## ----fudv-qqnorm, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Normal data look like a straight line.  Sepal.Length seems fairly normal."----

ggplot(iris, aes(sample = Sepal.Length)) +
  geom_qq()


## ------------------------------------------------------------------------

qnorm(p = .1, mean = 0, sd = 1)


## ------------------------------------------------------------------------

qnorm(p = c(.25, .50, .75), mean = 0, sd = 1)


## ------------------------------------------------------------------------

ppoints(n = 3, a = 0)


## ------------------------------------------------------------------------

ppoints(n = 3)


## ----fudv-manual-qnorm, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Shows theoretical norms on the x-axis (based on predictions from mean and standard deviation)."----

qplot(
  x = qnorm(
    p = ppoints(length(iris$Sepal.Length)),
    mean = mean(iris$Sepal.Length),
    sd = sd(iris$Sepal.Length)),
  y = sort(iris$Sepal.Length),
  xlab = "Theoretical Normal Quantiles",
  ylab = "Sepal Length") +
  geom_abline(slope = 1, intercept = 0)


## ----fudv-qqlnorm, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = c("Testing whether lynx data are consistent with a log normal distribution.", "Testing whether lynx data are consistent with a Poisson distribution.")----

ggplot(data.table(lynx = as.vector(lynx)), aes(sample = lynx)) +
  geom_qq(distribution = qlnorm)

ggplot(data.table(lynx = as.vector(lynx)), aes(sample = lynx)) +
  geom_qq(distribution = qpois, dparams = list(lambda = mean(lynx)))


## ----fudv-densitynorm, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "A normal curve and our density plot (with default smoothness of 1)."----

ggplot(iris, aes(Sepal.Length)) +
  geom_density() +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(iris$Sepal.Length),
                  sd = sd(iris$Sepal.Length)),
                colour = "blue")


## ------------------------------------------------------------------------

set.seed(1234)
y <- rbeta(150, 1, 4)
head(y)


## ----warning=FALSE-------------------------------------------------------

y.fit <- fitdistr(y, densfun = "beta",
                  start = list(shape1 = .5, shape2 = .5))


## ------------------------------------------------------------------------

y.fit

y.fit$estimate["shape1"]

y.fit$estimate["shape2"]


## ------------------------------------------------------------------------

logLik(y.fit)


## ------------------------------------------------------------------------

y.fit2 <- fitdistr(y, densfun = "normal")
logLik(y.fit2)


## ----fudv-testdistr-norm, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Density plot with superimposed normal distributions and normal Q-Q plot."----

testdistr(y)


## ----fudv-testdistr, fig.width=6, fig.height=6, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Shows density plot with superimposed beta or normal distributions along with Q-Q plot fits."----

test.beta <- testdistr(y, "beta",
                       starts = list(shape1 = .5, shape2 = .5),
                       varlab = "Y", plot = FALSE)

test.normal <- testdistr(y, "normal", varlab = "Y", plot = FALSE)

plot_grid(
   test.beta$DensityPlot, test.beta$QQPlot,
   test.normal$DensityPlot, test.normal$QQPlot,
   ncol = 2)


## ----fudv-testdistr-pois, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Discrete observed proportions with the theoretical probabilies from a poisson plotted in blue."----

set.seed(1234)
y <- rnbinom(500, mu = 5, size = 2)
testdistr(y, "poisson")


## ----fudv-testdistr-nbinom, fig.width=6, fig.height=4, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Discrete observed proportions with the theoretical probabilies from a negative binomial distribution plotted in blue."----

testdistr(y, "nbinom")


## ------------------------------------------------------------------------

pnorm(c(-3, 3))


## ----fudv-anomaly-sd, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Panel graph showing stacked dot plots with anomalous values."----

set.seed(1234)
d <- data.table(
  y1 = rnorm(200, 0, 1),
  y2 = rnorm(200, 0, .2) + rep(c(-3, -1, 1, 3), each = 50))

plot_grid(
  qplot(c(d$y1, rep(5, 3)), geom = "dotplot", binwidth = .1),
  qplot(c(d$y2, rep(5, 3)), geom = "dotplot", binwidth = .1),
  ncol = 1, labels = c("A", "B"))


## ----fudv-anomaly-gamma, fig.width=4, fig.height=4, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Panel graph showing randomly generated (no added anomalous values) data from a Gamma and normal distribution."----

set.seed(1234)
d2 <- data.table(
  y1 = rgamma(200, 1, .1),
  y2 = rnorm(200, 10, 10))

plot_grid(
  qplot(d2$y1, geom = "dotplot", binwidth = 1),
  qplot(d2$y2, geom = "dotplot", binwidth = 1),
  ncol = 1, labels = c("A", "B"))


## ----fudv-anomaly-testdistr, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Graph showing highlighting of extreme values."----

testdistr(d$y1, extremevalues = "empirical",
          ev.perc = .01)


## ----fudv-anomaly-td-gamma, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = paste("Graph showing highlighting of extreme values based on theoretical percentiles from a", c("normal distribution.", "Gamma distribution."))----

testdistr(d2$y1, "normal", extremevalues = "theoretical",
          ev.perc = .001)

testdistr(d2$y1, "gamma", extremevalues = "theoretical",
          ev.perc = .001)


## ----fudv-anomaly-masked, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Graph showing an anomalous value of 100 masked by the more extreme anomalous value of 1000."----

testdistr(c(d2$y2, 100, 1000), "normal",
          extremevalues = "theoretical",
          ev.perc = .001)


## ----fudv-anomaly-robust, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Graph highlighting extreme values based on a robust estimator."----

testdistr(c(d2$y2, 100, 1000), "normal",
          robust = TRUE,
          extremevalues = "theoretical",
          ev.perc = .001)


## ------------------------------------------------------------------------

winsorizor(1:10, .1)


## ----fudv-anomaly-wins, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "Panel graph comparing data before (A) and after (B) winsorizing the (empirical) bottom and top 1\\%."----

plot_grid(
  testdistr(d2$y1, "gamma", extremevalues = "theoretical",
            ev.perc = .005, plot=FALSE)$QQPlot,
  testdistr(winsorizor(d2$y1, .01), "gamma", extremevalues = "theoretical",
            ev.perc = .005, plot=FALSE)$QQPlot,
  ncol = 2, labels = c("A", "B"), align = "hv")



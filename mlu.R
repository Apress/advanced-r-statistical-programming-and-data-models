## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE----

 library(checkpoint)
 checkpoint("2018-09-28", R.version = "3.5.1",
   project = book_directory,
   checkpointLocation = checkpoint_directory,
   scanForPackages = FALSE,
   scan.rnw.with.knitr = TRUE, use.knitr = TRUE)

library(ggplot2)
library(cowplot)
library(viridis)
library(scales)
library(readxl)
library(data.table)
library(ape)
library(MASS)
library(matrixStats)

options(width = 70, digits = 2)


## ----eval = FALSE--------------------------------------------------------
## 
## source("https://bioconductor.org/biocLite.R")
## biocLite("pcaMethods")
## 

## ----echo=TRUE, message=FALSE, results = "hide"--------------------------

library(pcaMethods)


## ------------------------------------------------------------------------

## Note: download Excel file  from publisher website first
dRaw <- read_excel("Gender_StatsData_worldbank.org_ccby40.xlsx")
dRaw <- as.data.table(dRaw) # convert data to data.table format.


## ------------------------------------------------------------------------

str(dRaw)

summary(dRaw)

unique(dRaw$CountryName)

unique(dRaw$IndicatorCode)


## ------------------------------------------------------------------------

dRaw[,`Indicator Name`:= NULL]


## ------------------------------------------------------------------------

## collapse columns into a super long dataset
## with Year as a new variable
d <- melt(dRaw, measure.vars = 3:20, variable.name = "Year")
head(d)
str(d)

## finally cast the data wide again
## this time with separate variables by indicator code
## keeping a country and time (Year) variable
d <- dcast(d, CountryName + Year ~ IndicatorCode)

head(d)
str(d)


## ------------------------------------------------------------------------

## rename columns with shortened, unique names
x<-colnames(d)
x<-gsub("[[:punct:]]", "", x)
(y <- abbreviate(x, minlength = 4, method = "both.sides"))
names(d) <- y

## shorten regional names to abbreviations.
d$CntN<-abbreviate(d$CntN, minlength = 5,
                   method = "left.kept")


## ------------------------------------------------------------------------

summary(d)

str(d)

d[, Year := as.character(Year)]


## ----mlu-gnpadoplot1, fig.width=7, fig.height=10, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Plot of Gross National Product Per Capita and Adolescent Fertility Rate per 1,000 women."----

## ggplot2 plot object indicating x and y variables
p1 <- ggplot(d, aes(NYGN, SPAD))

## make a grid of two plots
plot_grid(
  ## first plot data points only
  p1 + geom_point(),
  ## data poins colored by year
  p1 + geom_point(aes(colour = Year)) +
    scale_colour_viridis(discrete = TRUE),
  ncol = 1
)


## ----mlu-kmeans1, fig.width=9, fig.height=9, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Plot of Gross National Product Per Capita and Adolescent Fertility Rate per 1,000 women for different numbers of k clusters."----

set.seed(2468)
wgss <- vector("numeric", 8)
plots <- vector("list", 9)
p1 <- ggplot(d, aes(NYGN, SPAD))

for(i in 2:9) {
  km <- kmeans(d[, .(NYGN, SPAD)],
             centers = i)

  wgss[i - 1] <- km$tot.withinss

  plots[[i - 1]] <- p1 +
    geom_point(aes_(colour = factor(km$cluster))) +
    scale_color_viridis(discrete = TRUE) +
    theme(legend.position = "none") +
    ggtitle(paste("kmeans centers = ", i))
}

plots[[9]] <- ggplot() +
  geom_point(aes(x = 2:9, y = wgss)) +
  xlab("Number of Clusters") +
  ylab("Within SS") +
  ggtitle("Scree Plot")

do.call(plot_grid, c(plots, ncol = 3))


## ------------------------------------------------------------------------

summary(d[,.(NYGN, SPAD)])


## ------------------------------------------------------------------------

x <- scale(d[,.(NYGN, SPAD)])
summary(x)


## ----mlu-kmeans2, fig.width=9, fig.height=9, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Plot of Gross National Product Per Capita and Adolescent Fertility Rate per 1,000 women for different numbers of k clusters."----

set.seed(2468)
wgss <- vector("numeric", 8)
plots <- vector("list", 9)
p1 <- ggplot(d, aes(NYGN, SPAD))

for(i in 2:9) {
  km <- kmeans(x, centers = i)

  wgss[i - 1] <- km$tot.withinss

  plots[[i - 1]] <- p1 +
    geom_point(aes_(colour = factor(km$cluster))) +
    scale_color_viridis(discrete = TRUE) +
    theme(legend.position = "none") +
    ggtitle(paste("kmeans centers = ", i))
}

plots[[9]] <- ggplot() +
  geom_point(aes(x = 2:9, y = wgss)) +
  xlab("Number of Clusters") +
  ylab("Within SS") +
  ggtitle("Scree Plot")

do.call(plot_grid, c(plots, ncol = 3))


## ----mlu-kmeans3, fig.width=9, fig.height=9, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Plot of Gross National Product Per Capita and Adolescent Fertility Rate per 1,000 women for different numbers of iterations."----

set.seed(2468)
plots <- vector("list", 9)
p1 <- ggplot(d, aes(NYGN, SPAD))

for(i in 6:14) {
  km <- kmeans(x, centers = 6, iter.max = i)

  plots[[i - 5]] <- p1 +
    geom_point(aes_(colour = factor(km$cluster))) +
    scale_color_viridis(discrete = TRUE) +
    theme(legend.position = "none") +
    ggtitle(paste("kmeans iters = ", i))
}

do.call(plot_grid, c(plots, ncol = 3))


## ----mlu-kmeans4, fig.width=9, fig.height=9, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Plot of Gross National Product Per Capita and Adolescent Fertility Rate per 1,000 women for different nstart values."----

set.seed(2468)
plots <- vector("list", 9)
p1 <- ggplot(d, aes(NYGN, SPAD))

for(i in 1:9) {
  km <- kmeans(x, centers = 6, iter.max = 10, nstart = i)

  plots[[i]] <- p1 +
    geom_point(aes_(colour = factor(km$cluster))) +
    scale_color_viridis(discrete = TRUE) +
    theme(legend.position = "none") +
    ggtitle(paste("kmeans nstarts = ", i))
}

do.call(plot_grid, c(plots, ncol = 3))


## ----mlu-kmeans5, fig.width=6, fig.height=6, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Scree Plot for all ."----

x <- scale(d[,-c(1,2)])
wgss<-0
set.seed(2468)
for( i in 1:11){
  km <- kmeans(x, centers = i)
  wgss[i]<-km$tot.withinss
}

ggplot() +
  geom_point(aes(x = 1:11, y = wgss)) +
  xlab("Number of Clusters") +
  ylab("Within SS") +
  ggtitle("Scree Plot - All Variables")


## ------------------------------------------------------------------------

kmAll <- kmeans(x, centers = 4, nstart = 25)
x <- cbind(d[, c(1,2)], x,
           Cluster = kmAll$cluster)
tail(x)


## ------------------------------------------------------------------------

xtabs(~ CntN + Cluster, data = x)


## ------------------------------------------------------------------------

unique(x[
  order(CntN, Year, Cluster),
  .(CntN, Year, Cluster)][
    CntN=="EsA&P"])

unique(x[
  order(CntN, Year, Cluster),
  .(CntN, Year, Cluster)][
    CntN == "ErpnU"])


## ------------------------------------------------------------------------

hdist <- dist(d[,.(NYGN, SPAD)])
str(hdist)


## ----mlu-hclust1, fig.width=14, fig.height=10, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Cluster Dendrogram with row numbers."----

hclust <- hclust(hdist)
plot(hclust)


## ----mlu-hclust2, fig.width=14, fig.height=12, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "Cluster Dendrogram with country names and year."----

x <- d[, .(CntN, Year, NYGN, SPAD)]
x[, Key := paste(CntN, Year)]
x[, CntN := NULL]
x[, Year := NULL]

hdist <- dist(x[,.(NYGN, SPAD)])
hclust <- hclust(hdist)
plot(hclust, labels = x$Key)


## ----mlu-hclust3, fig.width=14, fig.height=12, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Cluster Dendrogram with country names and year and a height line."----

plot(hclust, labels = x$Key)
abline(h = 30000, col = "blue")


## ------------------------------------------------------------------------

summary(x)
d[, mean(NYGN), by = CntN][order(V1)]
d[, mean(SPAD), by = CntN][order(V1)]


## ----mlu-hclust4, fig.width=14, fig.height=12, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Cluster Dendrogram with country names and year and another height line."----

plot(hclust, labels = x$Key)
abline(h = 20000, col = "blue")


## ----mlu-hclust5, fig.width=14, fig.height=12, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Cluster Dendrogram with country names and year and all dimensions of data."----

x <- copy(d)
x[, Key := paste(CntN, Year)]
x[, CntN := NULL]
x[, Year := NULL]

hdist <- dist(x[, -12])
hclust <- hclust(hdist)

plot(hclust, labels = x$Key)


## ----mlu-hclust6, fig.width=14, fig.height=12, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Cluster Dendrogram using the ward.D2 method."----

hclust <- hclust(hdist, method = "ward.D2")
plot(hclust, labels = x$Key)


## ----mlu-hclust7, fig.width=14, fig.height=12, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Cluster Dendrogram with scaling."----

x <- scale(d[,-c(1,2)])
row.names(x) <- paste(d$CntN, d$Year)
hdist <- dist(x)
hclust <- hclust(hdist)

plot(hclust, labels = paste(d$CntN, d$Year))
abline(h = 6, col = "blue")


## ------------------------------------------------------------------------

cut_hclust <- cutree(hclust, h = 6)
unique(cut_hclust)


## ------------------------------------------------------------------------

dcopy <- as.data.table(copy(d))
dcopy[, cluster:= NA_integer_]

dcopy$cluster <- cutree(hclust, k = 3)

tail(dcopy)


## ----mlu-hclust8, fig.width=14, fig.height=14, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Variations on Dendrogram via ape package."----

plot(as.phylo(hclust), type = "cladogram")


## ----mlu-hclust9, fig.width=14, fig.height=14, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Variations on Dendrogram via ape package."----

plot(as.phylo(hclust), type = "fan")


## ----mlu-hclust10, fig.width=14, fig.height=14, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "Variations on Dendrogram via ape package."----

plot(as.phylo(hclust), type = "radial")


## ----mlu-hclust11, fig.width=14, fig.height=14, out.width='.9\\linewidth', fig.pos="!ht", fig.cap = "unrooted type on 4 clusters."----

hclust4 <- cutree(hclust, k = 4)
plot(as.phylo(hclust), type = "unrooted", label.offset = 1,
     tip.color = hclust4, cex = 0.8)


## ----mlu-pca1, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "A highly correlated plot - are there really two dimensions here?"----

cor(d$NYGD, d$NYGN)

summary(d[,.(NYGD, NYGN)])

ggplot(d, aes(NYGD, NYGN)) +
  geom_point()


## ----mlu-pca2, fig.width=6, fig.height=6, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "A highly correlated plot - are there really two dimensions here?"----

ggplot(d, aes(NYGD, SESC)) +
  geom_point()

cor(d$NYGD, d$SESC)


## ------------------------------------------------------------------------

x <- d[,.( NYGN, SPAD)]
res <- pca(x, method="svd", center=TRUE, scale = "uv")

summary(res)


## ----mlu-pca3, fig.width=7, fig.height=7, out.width='.8\\linewidth', fig.pos="!ht", fig.cap = "comparison of raw data and pca"----

biplot(res, main = "Biplot of PCA")


## ----mlu-pca4, fig.width=5, fig.height=5, out.width='.5\\linewidth', fig.pos="!ht", fig.cap = "Scree plot for traditional PCA on all features in the data."----

x <- d[, -c(1,2)]
res <- pca(x, method="svd", center=TRUE, scale = "uv",
           nPcs = ncol(x))

summary(res)

## reverse scree plot
ggplot() +
  geom_bar(aes(1:11, cumsum(res@R2)),
           stat = "identity") +
  scale_x_continuous("Principal Component", 1:11) +
  scale_y_continuous(expression(R^2), labels = percent) +
  ggtitle("Scree Plot") +
  coord_cartesian(xlim = c(.5, 11.5), ylim = c(.5, 1),
                  expand = FALSE)


## ----mlu-pca5, fig.width=6, fig.height=6, out.width='.7\\linewidth', fig.pos="!ht", fig.cap = "Biplot for first two principal components."----

biplot(res, choices = c(1, 2))


## ------------------------------------------------------------------------

head(scores(res))
round(cor(scores(res)),2)


## ----mlu-pca6, fig.width=5, fig.height=10, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Plot loadings from PCA models with and without outliers using traditional, SVD, PCA and robust PCA."----

x <- d[, -c(1,2)]
x <- prep(x, center = TRUE, scale = "uv")

xout <- copy(x)
xout[1:5, "NYGD"] <- (-10)

res1 <- pca(x, method = "svd",
            center = FALSE, nPcs = 4)
res2 <- pca(xout, method = "svd",
            center = FALSE, nPcs = 4)

res1rob <- pca(x, method = "robustPca",
               center = FALSE, nPcs = 4)
res2rob <- pca(xout, method = "robustPca",
               center = FALSE, nPcs = 4)
plot_grid(
  ggplot() +
    geom_point(aes(
      x = as.numeric(loadings(res1)),
     y = as.numeric(loadings(res2)))) +
    xlab("Loadings, SVD, No Outliers") +
    ylab("Loadings, SVD, Outliers"),
  ggplot() +
    geom_point(aes(
      x = as.numeric(loadings(res1rob)),
     y = as.numeric(loadings(res2rob)))) +
    xlab("Loadings, Robust PCA, No Outliers") +
    ylab("Loadings, Robust PCA, Outliers"),
  ncol = 1)


## ------------------------------------------------------------------------

x <- scale(d[, -c(1,2)])
row.names(x) <- paste(d$CntN, d$Year)
head(x)

sdist <- dist(x)

xSammon <- sammon(sdist, k = 2)
head(xSammon$points)


## ----mlu-sam1, fig.width=5, fig.height=5, out.width='.6\\linewidth', fig.pos="!ht", fig.cap = "Plot of Sammon points with text labels"----

plot(xSammon$points, type = "n")
text(xSammon$points, labels = row.names(x) )



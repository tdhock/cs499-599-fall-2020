x <- iris[, c("Petal.Length", "Sepal.Length")]
cl <- stats::kmeans(x, 3)
cluster.dt <- data.table::data.table(
  x, cluster=factor(cl$cluster))
library(ggplot2)
## single panel ggplot of clusters with a different color for each
## cluster. 
ggplot()+
  geom_point(aes(
    x=Petal.Length, y=Sepal.Length, color=cluster),
    data=cluster.dt)

## single panel ggplot of clusters with a different color for each
## label/Species. 
ggplot()+
  geom_point(aes(
    x=Petal.Length, y=Sepal.Length, color=Species),
    data=iris)

## multi-panel ggplot, clusters in one panel, labels in the other.

## create data table with common column names.
library(data.table)
combined.dt <- rbind(
  data.table(iris)[, .(
    Petal.Length, Sepal.Length,
    cluster=as.integer(Species),
    x.var="label")],
  data.table(cluster.dt, x.var="clustering")
)  
ggplot()+
  geom_point(aes(
    x=Petal.Length, y=Sepal.Length, color=cluster),
    data=combined.dt)+
  facet_grid(x.var ~ .)

## search wikipedia for "Hungarian algorithm" which computes most
## likely assignment of clusters to labels.

## show how clustering is random with nstart=1.

## discuss set.seed -> control for the R pseudo-random number
## generation.
set.seed(100)
cl <- stats::kmeans(x, 3)
cluster.dt <- data.table::data.table(
  x, cluster=factor(cl$cluster))
ggplot()+
  geom_point(aes(
    x=Petal.Length, y=Sepal.Length, color=cluster),
    data=cluster.dt)

## use nstart=100 to remove randomness.
cl <- stats::kmeans(x, 3, nstart=100)
cluster.dt <- data.table::data.table(
  x, cluster=factor(cl$cluster))
ggplot()+
  geom_point(aes(
    x=Petal.Length, y=Sepal.Length, color=cluster),
    data=cluster.dt)

## one panel for each number of clusters.
cluster.dt.list <- list()
metric.dt.list <- list()
for(K in 2:5){
  cl <- stats::kmeans(x, K, nstart=100)
  metric.dt.list[[paste(K)]] <- data.table(
    K,
    ARI=pdfCluster::adj.rand.index(
      iris$Species, cl$cluster),
    sum.squares=cl$tot.withinss)
  cluster.dt.list[[paste(K)]] <- data.table(
    K, x, cluster=factor(cl$cluster))
}
cluster.dt <- do.call(rbind, cluster.dt.list)
metric.dt <- do.call(rbind, metric.dt.list)
ggplot()+
  geom_point(aes(
    x=Petal.Length, y=Sepal.Length,
    color=cluster),
    data=cluster.dt)+
  facet_wrap("K")

## summary plot with ARI and squared error.
ggplot()+
  geom_line(aes(
    K, ARI),
    data=metric.dt)

ggplot()+
  geom_line(aes(
    K, sum.squares),
    data=metric.dt)

## Exercise: how to create a multi-panel ggplot that shows both the
## ARI and sum.squares in separate panels, on top of each other?

data(spirals, package="kernlab")
str(spirals)

library(data.table)
library(ggplot2)
spirals.dt <- data.table(spirals)
str(spirals.dt)
ggplot()+
  geom_point(aes(
    V1, V2),
    data=spirals.dt)

kmeans.result <- stats::kmeans(spirals, 2)
kmeans.result$cluster
kmeans.result[["cluster"]] #1

d.mat <- stats::dist(spirals)
h.tree <- stats::hclust(d.mat)
stats::cutree(h.tree, k=2) #2

specc.result <- kernlab::specc(spirals, 2)
specc.result@.Data #3

## Exercise: how to visualize all of these cluster IDs in a
## multi-panel ggplot, one panel for each of the three algorithms?

## Need columns: algorithm, V1, V2, cluster.id ... how many rows do we
## need in spiral.clusters data.table? 900, one row for each data
## point / algorithm combination.
spiral.clusters <- rbind(
  data.table(
    algorithm="kmeans", spirals.dt, cluster.id=kmeans.result[["cluster"]]),
  data.table(
    algorithm="hclust", spirals.dt, cluster.id=stats::cutree(h.tree, k=2)),
  data.table(
    algorithm="specc", spirals.dt, cluster.id=specc.result@.Data)
)

ggplot()+
  geom_point(aes(
    V1, V2, color=factor(cluster.id)),
    data=spiral.clusters)+
  facet_grid(. ~ algorithm)

degree.dt.list <- list()
for(degree in 1:4){
  specc.result <- kernlab::specc(
    spirals, 2, kernel=kernlab::polydot(degree=degree))
  degree.dt.list[[paste(degree)]] <- data.table(
    degree, spirals.dt, cluster.id=specc.result@.Data)
}
degree.dt <- do.call(rbind, degree.dt.list)

ggplot()+
  geom_point(aes(
    V1, V2, color=factor(cluster.id)),
    data=degree.dt)+
  facet_grid(. ~ degree)

## specc on subset of data? classifying columns instead of rows, specc
## is treating columns of data table as observations. solution:
## convert data table to matrix before passing to specc (rows of
## matrix are considered observations).

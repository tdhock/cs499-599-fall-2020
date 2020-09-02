head(iris)

time.vec <- system.time({
  N <- 150
  d.mat <- stats::dist(iris[1:N, 1:4], method="manhattan")
  as.matrix(d.mat)
  cl.tree <- stats::hclust(d.mat, method="single")
  stats::cutree(cl.tree, k=1:N)
})
str(time.vec)

plot(cl.tree)



if(FALSE){
  install.packages("microbenchmark")
}

time.dt.list <- list()
for(N in seq(5, 150, by=5)){
  X <- iris[1:N, 1:4]
  timing.df <- microbenchmark::microbenchmark(hclust={
    d.mat <- stats::dist(X, method="manhattan")
    as.matrix(d.mat)
    cl.tree <- stats::hclust(d.mat, method="single")
    stats::cutree(cl.tree, k=3)
  }, kmeans={
    stats::kmeans(X, 3)
  })
  time.dt.list[[paste(N)]] <- data.table(
    N, timing.df)
}
time.dt <- do.call(rbind, time.dt.list)

library(data.table)
data.table(timing.df)

## Exercise: plot timings versus data set size...?

time.dt[, data.size := N]
time.dt[, time.seconds := time/1e9]
time.dt[, algorithm := expr]
library(ggplot2)
ggplot()+
  geom_point(aes(
    x=data.size,
    y=time.seconds,
    color=algorithm),
    data=time.dt)

median.dt <- time.dt[, .(
  median.seconds=median(time.seconds)
), by=.(N, algorithm)]
ggplot()+
  geom_line(aes(
    x=N,
    y=median.seconds,
    color=algorithm),
    data=median.dt)


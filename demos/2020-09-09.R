## halfcircle/moons, spiral, and grid of gaussian clusters data sets.
library(data.table)
library(ggplot2)
set.seed(1)
halfcircle <- function(r, center = c(0, 0), class, sign, N=150, noise=0.5) {
  angle <- runif(N, 0, pi)
  rad <- rnorm(N, r, noise)
  data.table(
    V1 = rad * cos(angle) + center[1],
    V2 = sign * rad * sin(angle) + center[2],
    class = factor(class))
}
X.dt <- rbind(
  halfcircle(4, c(0, 0), 1, 1),
  halfcircle(4, c(4, 2), 2, -1))

## TODO ggplot.
ggplot()+
  geom_point(aes(
    V1, V2, color=class),
    data=X.dt)

## kmeans
X.mat <- as.matrix(X.dt[, 1:2])
kmeans.result <- stats::kmeans(X.mat, 2)
X.dt[, kmeans := factor(kmeans.result[["cluster"]])]

ggplot()+
  geom_point(aes(
    V1, V2, color=kmeans),
    data=X.dt)


## TODO hclust.

## TODO for loop over linkage methods?
d.mat <- stats::dist(X.mat)

## BAD: DO NOT DO THIS!!!
method.dt <- NULL
## Let N = number of iterations of your for loop.
for(method in c("single", "average", "complete")){
  hc.result <- stats::hclust(d.mat, method=method)
  pred.cluster.vec <- stats::cutree(hc.result, 2)
  ## BAD/SLOW/INEFFICIENT. every rbind call here performs an O(i)
  ## operation, from i=1 to N. => overall quadratic O(N^2)
  ## time/space. (300, 600, 900, ...)
  method.dt <- rbind(method.dt, data.table(
    X.mat,
    method,
    cluster=pred.cluster.vec))
}
## equivalent of:
rbind(data1, rbind(data2, rbind(data3, NULL)))

## Good/efficient way. 
method.dt.list <- list()
for(method in c("single", "average", "complete")){
  hc.result <- stats::hclust(d.mat, method=method)
  pred.cluster.vec <- stats::cutree(hc.result, 2)
  ## computational complexity of this data.table creation (one
  ## iteration) is constant O(1), over all iterations linear
  ## O(N). (300, 300, 300)
  method.dt.list[[method]] <- data.table(
    X.mat,
    method,
    cluster=pred.cluster.vec)
}
method.dt <- do.call(rbind, method.dt.list) # O(N) rows (900)

## equivalent of:
rbind(data1, data2, data3)
rbind(method.dt.list[[1]], method.dt.list[[2]], method.dt.list[[3]])

## READING: https://tdhock.github.io/blog/2017/rbind-inside-outside/

## Discussion of parallel computation
## https://tdhock.github.io/blog/2020/fast-parameter-exploration/

## DO NOT USE ONE GEOM_LINE PER METHOD.. BAD/REPETITIVE. do NOT use
## aes(visual_property="some_constant")
ggplot()+
  geom_line(aes(
    V1, V2, color="single"),
    data=method.dt.list[["single"]])+
  geom_line(aes(
    V1, V2, color="average"),
    data=method.dt.list[["average"]])

## You should use aes(visual_property=data_variable) as below:
ggplot()+
  geom_point(aes(
    V1, V2, color=cluster),
    data=method.dt)+
  facet_grid(. ~ method)

## TODO list of data tables idiom (rbind outside/after for loop) ->
## linear time. VS wrong way (rbind in each iteration of for loop)
## quadratic time.

specc.result <- kernlab::specc(X.mat, 2)
X.dt[, specc := factor(specc.result@.Data) ]

ggplot()+
  geom_point(aes(
    V1, V2, color=specc),
    data=X.dt)

## Example 2: spirals data.
data(spirals, package="kernlab")
X.mat <- spirals
X.dt <- data.table(X.mat)

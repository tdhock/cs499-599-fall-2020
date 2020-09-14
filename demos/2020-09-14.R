str(iris)

## https://tdhock.github.io/blog/2020/set-assignment/

## train set = fitting the unsupervised model parameters.

## validation = doing model selection, fitting/choosing the
## hyper-parameters. ===> how is the model doing for some new/unseen
## observations? how is the model generalizing to new data points?

## hyper-parameters must be fixed before fitting the unsupervised
## learning model / calling the function which invokes the
## unsupervised learning algorithm. e.g. kmeans = number of clusters,
## distance metric / input matrix normalization. e.g. hclust =
## distance function, linkage criterion. (input to the learning
## algorithm)

## model parameters / regular parameters (not hyper-parameters) = the
## values which are inferred/learned by the unsupervised learning
## algorithm. e.g. kmeans = cluster centers / mean values. e.g. hclust
## = splits/tree. (output by the learning algorithm)

(set.prop.vec <- c(validation=0.5, train=0.5))
(N <- nrow(iris))
(rounded.counts <- floor(set.prop.vec*(N+1)))
(not.shuffled.sets <- rep(names(set.prop.vec), rounded.counts))

table(set=not.shuffled.sets, label=iris$Species)

set.seed(1)
shuffled.sets <- sample(not.shuffled.sets)
table(set=shuffled.sets, label=iris$Species)

library(mclust)
library(data.table)
X.mat <- as.matrix(iris[,1:4])
lik.dt.list <- list()
for(n.clusters in 1:5){
  X.train <- X.mat[shuffled.sets == "train",]
  mclust.result <- mclust::Mclust(
    X.train, n.clusters, modelNames="VEV")
  for(set in names(set.prop.vec)){
    X.set <- X.mat[shuffled.sets == set,]
    log.lik.vec <- mclust::dens(
      modelName = mclust.result[["modelName"]],
      data = X.set,
      parameters = mclust.result[["parameters"]],
      logarithm=TRUE)
    total.log.lik <- sum(log.lik.vec)
    rbind(my=total.log.lik, mclust=mclust.result[["loglik"]])
    lik.dt.list[[paste(n.clusters, set)]] <- data.table(
      n.clusters, set, total.log.lik)
  }
}
(lik.dt <- do.call(rbind, lik.dt.list))

library(ggplot2)
ggplot()+
  geom_line(aes(
    n.clusters, -total.log.lik, color=set),
    data=lik.dt)

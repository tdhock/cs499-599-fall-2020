## use initialization subset to reduce computation time.
n.clusters <- 10
zip.dt <- data.table::fread("zip.test.gz")
zip.mat <- as.matrix(zip.dt[, -1])
library(mclust)
fit <- mclust::Mclust(
  zip.mat,
  n.clusters,
  initialization=list(subset=1:100))


## use modelNames to consider one type of model, to get increasing
## log.lik curve. Each model type has a different number of
## parameters, and the Mclust function uses the BIC model selection
## criterion to choose one of those models (maybe not the same one for
## each value of G, which is why the loglik curve may decrease).
## Figure
## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/bin/nihms793803f2.jpg
## from article https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/
## local copy linked on course web page github README.

## from ?mclustModelNames

## ‘"EII"’ spherical, equal volume

## ‘"VII"’ spherical, unequal volume

## ‘"EEI"’ diagonal, equal volume and shape

## ‘"VEI"’ diagonal, varying volume, equal shape

## ‘"EVI"’ diagonal, equal volume, varying shape

## ‘"VVI"’ diagonal, varying volume and shape

## use verbose=0 to suppress printed output.






n.clusters <- 10
zip.dt <- data.table::fread("zip.test.gz")
zip.mat <- as.matrix(zip.dt[, -1])
library(mclust)
fit <- mclust::Mclust(
  zip.mat,
  n.clusters,
  initialization=list(subset=1:100),
  modelNames=c("EII", "VII", "EEI", "EVI", "VEI", "VVI"),
  verbose=0)

n.clusters.vec <- seq(1, 20, by=2)
metrics.dt.list <- list()
library(data.table)
for(n.clusters in n.clusters.vec){
  fit <- mclust::Mclust(
    zip.mat,
    n.clusters,
    initialization=list(subset=1:100),
    verbose = 0,
    modelNames="EII")
  one.result <- data.table(
    n.clusters,
    log.lik=fit$loglik,
    ARI=pdfCluster::adj.rand.index(
      fit$classification, zip.dt[["V1"]]))
  print(one.result)
  metrics.dt.list[[paste(n.clusters)]] <- one.result
}
metrics.dt <- do.call(rbind, metrics.dt.list)

library(ggplot2)
ggplot()+
  geom_line(aes(
    n.clusters, ARI),
    data=metrics.dt)

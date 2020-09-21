##install.packages("NPflow")

## main function NPflow::DPMpost which is not super user friendly.

##install.packages("PReMiuM")

## lots of errors, too many dependencies.
## https://www.jstatsoft.org/article/view/v064i07

if(FALSE){
  install.packages("dirichletprocess")
  install.packages("DPpackage")
}

## https://cloud.r-project.org/web/packages/dirichletprocess/vignettes/dirichletprocess.pdf
library(dirichletprocess)
X <- scale(iris[,1:4])
dp <- dirichletprocess::DirichletProcessMvnormal(
  X,
  alphaPriors = c(2, 0.01), #Gamma(a,b) prior for alpha with a,b>0.
  numInitialClusters = 5)
dp.fit <- dirichletprocess::Fit(dp, 100)
table(dp.fit$clusterLabels, iris$Species)


if(!file.exists("ESL.mixture.rda")){
  download.file(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda",
    "ESL.mixture.rda")
}
objs <- load("ESL.mixture.rda")
X.mat <- ESL.mixture[["x"]]
dp <- dirichletprocess::DirichletProcessMvnormal(
  X.mat,
  alphaPriors = c(2, 4), #Gamma(a,b) prior for alpha with a,b>0.
  numInitialClusters = 5)
dp.fit <- dirichletprocess::Fit(dp, 100)

library(data.table)
cl.dt <- data.table(
  X.mat, cluster=dp.fit$clusterLabels)
library(ggplot2)
ggplot()+
  geom_point(aes(
    V1, V2, color=factor(cluster)),
    data=cl.dt)



if(!file.exists("zip.test.gz")){
  download.file(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.test.gz",
    "zip.test.gz")
}
zip.test <- data.table::fread("zip.test.gz")

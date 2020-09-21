if(FALSE){
  install.packages("dirichletprocess")
}

## https://cloud.r-project.org/web/packages/dirichletprocess/vignettes/dirichletprocess.pdf
library(dirichletprocess)
## linear transformation on each column so that mean=0 and sd=1.
X <- scale(iris[,1:4])
dp <- dirichletprocess::DirichletProcessMvnormal(
  ## Gaussian is for 1d data, Mvnorm is for 2+ dimensions/features (iris=4).
  X,
  alphaPriors = c(2, 0.01), #Gamma(a,b) prior for alpha with a,b>0.
  numInitialClusters = 5)
dp.fit <- dirichletprocess::Fit(dp, its=100)#iterations of sampling algorithm.
table(dp.fit$clusterLabels, iris$Species)


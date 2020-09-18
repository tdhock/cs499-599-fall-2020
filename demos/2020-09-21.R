##install.packages("NPflow")

## main function NPflow::DPMpost which is not super user friendly.

##install.packages("PReMiuM")

## lots of errors, too many dependencies.
## https://www.jstatsoft.org/article/view/v064i07

if(FALSE){
  install.packages("dirichletprocess")
  install.packages("DPpackage")

## https://cloud.r-project.org/web/packages/dirichletprocess/vignettes/dirichletprocess.pdf
library(dirichletprocess)
X <- scale(iris[,1:4])
dp <- dirichletprocess::DirichletProcessMvnormal(
  X,
  alphaPriors = c(2, 0.01), #Gamma(a,b) prior for alpha with a,b>0.
  numInitialClusters = 5)
dp.fit <- dirichletprocess::Fit(dp, 100)
table(dp.fit$clusterLabels, iris$Species)


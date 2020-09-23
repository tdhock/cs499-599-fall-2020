vignette("dirichletprocess", package="dirichletprocess")

## Comparing finite/infinite figures in textbook.

## Gamma distribution prior

(conc.vec <- seq(0, 40, l=101))
## https://en.wikipedia.org/wiki/Gamma_distribution
(dens.vec <- dgamma(conc.vec, shape=27.5, scale=1.0))
(dens.vec <- dgamma(conc.vec, shape=1, scale=2))
## see also help(dgamma).
(dens.vec <- dgamma(conc.vec, shape=1, rate=2))
(dens.vec <- dgamma(conc.vec, shape=27.5, rate=1.0))
library(ggplot2)
library(data.table)
dens.dt <- data.table(
  concentration=conc.vec,
  density=dens.vec)
ggplot()+
  geom_line(aes(
    concentration, density),
    data=dens.dt)

X <- scale(iris[,1:4])
dp <- dirichletprocess::DirichletProcessMvnormal(
  X,
  ## large concentration parameter (alpha) values means large number
  ## of clusters.
  alphaPriors = c(270, 1), 
  numInitialClusters = 5)
dp.fit <- dirichletprocess::Fit(dp, its=100)
dp.fit$alphaChain
table(dp.fit$clusterLabels, iris$Species)

dp.small <- dirichletprocess::DirichletProcessMvnormal(
  X,
  ## small concentration parameter (alpha) values means small number
  ## of clusters.
  alphaPriors = c(1, 1),
  numInitialClusters = 5)
dp.small.fit <- dirichletprocess::Fit(dp, its=100)
dp.small.fit$alphaChain
table(dp.small.fit$clusterLabels, iris$Species)

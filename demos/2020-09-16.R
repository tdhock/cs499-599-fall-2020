N.true.clusters <- 2
true.means <- 1:N.true.clusters
N.simulated.data <- 20
(means.simulated.data <- rep(
  true.means, each=N.simulated.data/N.true.clusters))
set.seed(1)
true.sd <- 0.2
sim.data.vec <- rnorm(N.simulated.data, means.simulated.data, sd=true.sd)
library(data.table)
(sim.data.dt <- data.table(feature=sim.data.vec))
library(ggplot2)
ggplot()+
  geom_point(aes(
    x=feature),
    y=0,
    data=sim.data.dt)

## add true density.
(feature.grid.vec <- seq(0, N.true.clusters+1, by=0.01))
density.dt.list <- list()
for(true.cluster.i in 1:N.true.clusters){
  density.dt.list[[true.cluster.i]] <- data.table(
    true.cluster.i,
    feature=feature.grid.vec,
    density=dnorm(
      feature.grid.vec,
      true.means[[true.cluster.i]],
      sd=true.sd))
}
(density.dt <- do.call(rbind, density.dt.list))
ggplot()+
  geom_point(aes(
    x=feature, y=0),
    data=sim.data.dt)+
  geom_line(aes(
    feature, density, color=factor(true.cluster.i)),
    data=density.dt)

(mixture.dt <- density.dt[, .(
  mix.density=mean(density)
), by=feature ])
ggplot()+
  geom_point(aes(
    x=feature, y=0),
    data=sim.data.dt)+
  geom_line(aes(
    feature, mix.density),
    data=mixture.dt)

library(mclust)
n.clusters <- 2
mclust.model <- mclust::Mclust(sim.data.dt, n.clusters, modelNames="E")
mclust.density.vec <- mclust::dens(
  modelName = mclust.model$modelName,
  data = feature.grid.vec,
  parameters = mclust.model$parameters)

mclust.true.dt <- rbind(
  data.table(mixture.dt, model="true"),
  data.table(
    feature=feature.grid.vec,
    mix.density=mclust.density.vec,
    model="mclust"))
ggplot()+
  geom_point(aes(
    x=feature, y=0),
    data=sim.data.dt)+
  geom_line(aes(
    feature, mix.density, color=model),
    data=mclust.true.dt)

mclust.components.list <- list()
for(n.clusters in 1:9){
  mclust.model <- mclust::Mclust(sim.data.dt, n.clusters, modelNames="E")
  mclust.density.vec <- mclust::dens(
    modelName = mclust.model$modelName,
    data = feature.grid.vec,
    parameters = mclust.model$parameters)
  mclust.components.list[[n.clusters]] <- rbind(
    data.table(n.clusters, mixture.dt, model="true"),
    data.table(
      n.clusters,
      feature=feature.grid.vec,
      mix.density=mclust.density.vec,
      model="mclust"))
}
(mclust.components <- do.call(rbind, mclust.components.list))

ggplot()+
  geom_point(aes(
    x=feature, y=0),
    data=sim.data.dt)+
  geom_line(aes(
    feature, mix.density, color=model),
    data=mclust.components)+
  facet_wrap("n.clusters")

(set.prop.vec <- c(validation=0.5, train=0.5))
(rounded.counts <- floor(set.prop.vec*(N.simulated.data+1)))
(not.shuffled.sets <- rep(names(set.prop.vec), rounded.counts))
set.seed(1)
(shuffled.sets <- sample(not.shuffled.sets))
sim.data.dt[, set := shuffled.sets ]
sim.data.dt

mclust.split.list <- list()
lik.dt.list <- list()
for(n.clusters in 1:9){
  mclust.model <- mclust::Mclust(
    sim.data.dt[set=="train", feature],
    n.clusters,
    modelNames="E")
  mclust.density.vec <- mclust::dens(
    modelName = mclust.model$modelName,
    data = feature.grid.vec,
    parameters = mclust.model$parameters)
  for(s in names(set.prop.vec)){
    set.dt <- sim.data.dt[set == s]
    log.lik.vec <- mclust::dens(
      modelName = mclust.model[["modelName"]],
      data = set.dt$feature,
      parameters = mclust.model[["parameters"]],
      logarithm=TRUE)
    total.log.lik <- sum(log.lik.vec)
    lik.dt.list[[paste(n.clusters, s)]] <- data.table(
      n.clusters, set=s, total.log.lik)
  }
  mclust.split.list[[n.clusters]] <- rbind(
    data.table(n.clusters, mixture.dt, model="true"),
    data.table(
      n.clusters,
      feature=feature.grid.vec,
      mix.density=mclust.density.vec,
      model="mclust"))
}
(mclust.split <- do.call(rbind, mclust.split.list))
(lik.dt <- do.call(rbind, lik.dt.list))

ggplot()+
  geom_line(aes(
    n.clusters, -total.log.lik, color=set),
    data=lik.dt)

## alphaPrior demo.

## Burns Ch 47 Be Claustrophobic.

## Chinese Restaurant Process / Stick Breaking Process, MLAPP figure
## 25.5.
concentration <- 5 #alpha
max.clusters <- 30
beta.vec <- rep(NA_real_, max.clusters)
pi.vec <- rep(NA_real_, max.clusters)
for(k in 1:max.clusters){
  beta.vec[[k]] <- rbeta(1, 1, concentration) # draw from Beta(1, alpha)
  prev.pi <- if(k==1)numeric(0) else pi.vec[1:(k-1)]
  pi.vec[[k]] <- beta.vec[[k]] * ( 1 - sum(prev.pi) )
}
library(ggplot2)
library(data.table)
(stick.breaking.dt <- data.table(
  cluster=seq_along(pi.vec),
  probability=pi.vec))
ggplot()+
  geom_bar(aes(
    x=cluster,
    y=probability),
    stat="identity",
    data=stick.breaking.dt)

stick.breaking.dt[, cum.prob := cumsum(probability)]
stick.breaking.dt[, stick.size := 1-c(0, cum.prob[-.N]) ]
stick.breaking.dt[, start := 1-stick.size ]
stick.breaking.dt[, end := 1 ]
stick.breaking.dt[, iteration := -cluster ]
ggplot()+
  geom_segment(aes(
    start,
    iteration,
    xend=end,
    yend=iteration),
    data=stick.breaking.dt)+
  geom_point(aes(
    cum.prob,
    iteration),
    data=stick.breaking.dt)

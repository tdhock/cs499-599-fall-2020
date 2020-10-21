## Burns Ch 8 Procrastinate

vignette("depmixS4", package="depmixS4")

data(neuroblastoma, package="neuroblastoma")
library(data.table)

pro.dt <- data.table(neuroblastoma$profiles)[profile.id=="4" & chromosome=="2"]

library(ggplot2)
gg <- ggplot()+
  geom_point(aes(
    position, logratio),
    data=pro.dt)

hmm <- depmixS4::depmix(logratio ~ 1, pro.dt, 3)
hmm.fit <- depmixS4::fit(hmm)
slotNames(hmm.fit)
hmm.fit@prior
hmm.fit@transition
hmm.fit@response
hmm.fit@posterior

resp.dt.list <- list()
for(resp.i in seq_along(hmm.fit@response)){
  param.list <- hmm.fit@response[[resp.i]][[1]]@parameters
  resp.dt.list[[paste(resp.i)]] <- data.table(
    state=resp.i,
    mean=param.list[["coefficients"]],
    sd=param.list[["sd"]])
}
resp.dt <- do.call(rbind, resp.dt.list)
post.dt <- data.table(hmm.fit@posterior)
post.dt[, index := 1:.N]

states.for.data <- resp.dt[post.dt, on="state"]

states.for.data[, sum(dnorm(pro.dt$logratio, mean, sd, log=TRUE))]
hmm.fit

pro.dt[, index := 1:.N]
ggplot()+
  geom_point(aes(
    index, logratio),
    data=pro.dt)+
  geom_ribbon(aes(
    index, ymin=mean-sd, ymax=mean+sd),
    data=states.for.data,
    fill="green",
    alpha=0.2)+
  geom_line(aes(
    index, mean),
    data=states.for.data,
    color="green")

## rle = run length encoding, for more compressed version of hidden
## markov model, in terms of segments, like optimal changepoint
## models.
state.rle <- rle(post.dt$state)
segs.dt <- data.table(
  n.data=state.rle[["lengths"]],
  state=state.rle[["values"]])
segs.dt[, end := cumsum(n.data)]
segs.dt[, start := c(1, end[-.N]+1)]
segs.with.params <- resp.dt[segs.dt, on="state"]

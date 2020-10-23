## Burns Ch 26 Be Lazy

## vary number of states, animint2.

data(neuroblastoma, package="neuroblastoma")
library(data.table)

pro.dt <- data.table(neuroblastoma$profiles)[profile.id=="79" & chromosome=="2"]

library(animint2)
ggplot()+
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

pro.dt[, index := 1:.N]
ggplot()+
  geom_point(aes(
    index, logratio),
    data=pro.dt)+
  geom_vline(aes(#changes
    xintercept=end+0.5
  ),
  color="green",
  data=segs.with.params[-.N])+
  geom_rect(aes(#sd
    xmin=start, xmax=end,
    ymin=mean-sd, ymax=mean+sd
  ),
  data=segs.with.params,
  fill="green",
  alpha=0.2,
  )+
  geom_segment(aes(#mean
    x=start, xend=end,
    y=mean, yend=mean),
    color="green",
    size=2,
    data=segs.with.params)

nstates.segs.list <- list()
loss.dt.list <- list()
for(nstates in 1:5){
  hmm <- depmixS4::depmix(logratio ~ 1, pro.dt, nstates)
  hmm.fit <- depmixS4::fit(hmm)
  loss.dt.list[[paste(nstates)]] <- data.table(
    nstates, loss=-depmixS4::logLik(hmm.fit))
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
  state.rle <- rle(post.dt$state)
  segs.dt <- data.table(
    n.data=state.rle[["lengths"]],
    state=state.rle[["values"]])
  segs.dt[, end := cumsum(n.data)]
  segs.dt[, start := c(1, end[-.N]+1)]
  nstates.segs.list[[paste(nstates)]] <- data.table(
    nstates, resp.dt[segs.dt, on="state"])
}
nstates.segs <- do.call(rbind, nstates.segs.list)
loss.dt <- do.call(rbind, loss.dt.list)

ggplot()+
  geom_point(aes(
    nstates, loss),
    data=loss.dt)

ggplot()+
  facet_grid(nstates ~ .)+
  geom_point(aes(
    index, logratio),
    data=pro.dt)+
  geom_vline(aes(#changes
    xintercept=start-0.5
  ),
  color="green",
  data=nstates.segs[1 < start])+
  geom_rect(aes(#sd
    xmin=start, xmax=end,
    ymin=mean-sd, ymax=mean+sd
  ),
  data=nstates.segs,
  fill="green",
  alpha=0.2,
  )+
  geom_segment(aes(#mean
    x=start-0.5, xend=end+0.5,
    y=mean, yend=mean),
    color="green",
    size=2,
    data=nstates.segs)



animint(
  ggplot()+
  geom_line(aes(
    nstates, loss),
    data=loss.dt)+
  geom_rect(aes(
    xmin=nstates-0.5, xmax=nstates+0.5,
    ymin=-Inf, ymax=Inf),
    alpha=0.5,
    clickSelects="nstates",
    data=loss.dt),
  ggplot()+
  geom_point(aes(
    index, logratio),
    data=pro.dt)+
  geom_vline(aes(#changes
    xintercept=start-0.5
  ),
  showSelected="nstates",
  color="green",
  data=nstates.segs[1 < start])+
  geom_rect(aes(#sd
    xmin=start, xmax=end,
    ymin=mean-sd, ymax=mean+sd
  ),
  showSelected="nstates",
  data=nstates.segs,
  fill="green",
  alpha=0.2,
  )+
  geom_segment(aes(#mean
    x=start-0.5, xend=end+0.5,
    y=mean, yend=mean),
    color="green",
    showSelected="nstates",
    size=2,
    data=nstates.segs)
)  

## http://members.cbio.mines-paristech.fr/~thocking/animint2-manual/Ch03-showSelected.html


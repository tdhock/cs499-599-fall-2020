## cross-validation model selection.
data(neuroblastoma, package="neuroblastoma")
library(data.table)

pro.dt <- data.table(neuroblastoma$profiles)[profile.id=="4" & chromosome=="2"]
pro.dt[, set := rep(rep(c("subtrain","validation"), each=10), l=.N)]
pro.dt[, index := 1:.N]
library(ggplot2)
ggplot()+
  geom_point(aes(
    position, logratio, color=set),
    data=pro.dt)

nstates.segs.list <- list()
loss.dt.list <- list()
set.seed(1)
for(nstates in 1:5){
  subtrain.dt <- pro.dt[set=="subtrain"]
  hmm <- depmixS4::depmix(logratio ~ 1, subtrain.dt, nstates)
  hmm.fit <- depmixS4::fit(hmm)
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
  post.dt[, index := subtrain.dt$index]
  state.rle <- rle(post.dt$state)
  segs.dt <- data.table(
    n.data=state.rle[["lengths"]],
    state=state.rle[["values"]])
  segs.dt[, end := cumsum(n.data)]
  segs.dt[, start := c(1, end[-.N]+1)]
  getPos <- function(subtrain.index){
    orig.index <- subtrain.dt$index[ subtrain.index ]
    pro.dt$position[ orig.index ]
  }
  change.pos <- (
    getPos(segs.dt[-.N, end])+getPos(segs.dt[-1, start])
  )/2
  segs.dt[, start.pos := c(pro.dt[1, position], change.pos)]
  segs.dt[, end.pos := c(change.pos, pro.dt[.N, position])]
  segs.with.params <- resp.dt[segs.dt, on=.(state=state)]
  ## non-equi join.
  data.with.params <- segs.with.params[pro.dt, .(
    set, index, mean, sd, logratio
  ), on=.(start.pos <= position, end.pos >= position)]
  lik.dt <- data.with.params[, .(
    neg.log.lik=-sum(dnorm(logratio, mean, sd, log=TRUE))
  ), by=set]
  loss.dt.list[[paste(nstates)]] <- data.table(
    nstates, lik.dt)
  nstates.segs.list[[paste(nstates)]] <- data.table(
    nstates, segs.with.params)
}
nstates.segs <- do.call(rbind, nstates.segs.list)
loss.dt <- do.call(rbind, loss.dt.list)

ggplot()+
  geom_line(aes(
    nstates, neg.log.lik, color=set),
    data=loss.dt)

ggplot()+
  facet_grid(nstates ~ .)+
  geom_point(aes(
    position, logratio, color=set),
    data=pro.dt)+
  geom_vline(aes(#changes
    xintercept=start.pos
  ),
  color="green",
  data=nstates.segs[1 < start])+
  geom_rect(aes(#sd
    xmin=start.pos, xmax=end.pos,
    ymin=mean-sd, ymax=mean+sd
  ),
  data=nstates.segs,
  fill="green",
  alpha=0.2,
  )+
  geom_segment(aes(#mean
    x=start.pos-0.5, xend=end.pos+0.5,
    y=mean, yend=mean),
    color="green",
    size=2,
    data=nstates.segs)




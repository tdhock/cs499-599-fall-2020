library(ggplot2)
library(data.table)

data(neuroblastoma, package="neuroblastoma")
pro.dt <- data.table(neuroblastoma$profiles)
one.pro <- pro.dt[profile.id=="79"]
ggplot()+
  theme_bw()+theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ chromosome, scales="free")+
  geom_point(aes(
    position, logratio),
    data=one.pro)
## model for one chrom
one.chrom <- one.pro[chromosome=="2"]
ggplot()+
  geom_point(aes(
    position, logratio),
    data=one.chrom)

some.pro <- pro.dt[profile.id %in% 110:120 & chromosome==2]
ggplot()+
  theme_bw()+theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(profile.id ~ ., scales="free")+
  geom_point(aes(
    position, logratio),
    data=some.pro)

model <- depmixS4::depmix(
  logratio ~ 1, data=one.chrom, nstates=4, family=gaussian())
model.fit <- depmixS4::fit(model)
methods::slotNames(model.fit)
resp.param.mat <- sapply(
  model.fit@response, function(L)unlist(L[[1]]@parameters))
rownames(resp.param.mat) <- c("mean", "sd")
resp.param.dt <- data.table(t(resp.param.mat), state=1:ncol(resp.param.mat))
state.rle <- rle(model.fit@posterior$state)
state.dt <- do.call(data.table, state.rle)
state.segs <- resp.param.dt[state.dt, on=.(state=values)]
state.segs[, end := cumsum(lengths)]
state.segs[, start := c(1, end[-.N]+1)]
state.segs[, end.show := end+0.5]
state.segs[, start.show := start-0.5]
state.changes <- state.segs[1 < start]
ggplot()+
  geom_point(aes(
    seq_along(logratio), logratio),
    data=one.chrom)+
  geom_vline(aes(
    xintercept=start.show),
    color="green",
    data=state.changes)+
  geom_segment(aes(
    start.show, mean,
    xend=end.show, yend=mean),
    color="green",
    size=1,
    data=state.segs)+
  geom_rect(aes(
    xmin=start.show, ymin=mean-sd,
    xmax=end.show, ymax=mean+sd),
    fill="green",
    alpha=0.1,
    data=state.segs)
cpt.fit <- changepoint::cpt.meanvar(
  one.chrom$logratio, method="SegNeigh", Q=nrow(state.segs),
  penalty="Manual")
cpt.params <- do.call(data.table, cpt.fit@param.est)
cpt.params[, end := cpt.fit@cpts]
cpt.params[, start := c(1, end[-.N]+1)]
cpt.params[, end.show := end+0.5]
cpt.params[, start.show := start-0.5]
cpt.params[, sd := sqrt(variance)]
cpt.changes <- cpt.params[1 < start]
both.segs <- rbind(
  cpt.params[, data.table(
    algorithm="DP", start.show, end.show, mean, sd)],
  state.segs[, data.table(
    algorithm="HMM", start.show, end.show, mean, sd)])
both.changes <- both.segs[1 < start.show]
one.chrom[, index := seq_along(logratio)]
ggplot()+
  geom_point(aes(
    index, logratio),
    data=one.chrom)+
  geom_vline(aes(
    xintercept=start.show),
    color="green",
    data=both.changes)+
  geom_segment(aes(
    start.show, mean,
    xend=end.show, yend=mean),
    color="green",
    size=1,
    data=both.segs)+
  geom_rect(aes(
    xmin=start.show, ymin=mean-sd,
    xmax=end.show, ymax=mean+sd),
    fill="green",
    alpha=0.1,
    data=both.segs)+
  facet_grid(algorithm ~ .)
one.chrom[both.segs, .(
  algorithm,
  nll=sum(-dnorm(logratio, mean, sd, log=TRUE)),
  N.data=.N
), on=.(index > start.show, index < end.show), by=.EACHI
][, .(total.nll=sum(nll)), by=algorithm]

some.chrom <- one.pro[chromosome %in% 1:2]
ggplot()+
  theme_bw()+theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ chromosome, scales="free", space="free")+
  geom_point(aes(
    position/1e6, logratio),
    data=some.chrom)

count.vec <- table(paste(some.chrom$chromosome))
lik.dt.list <- list()
for(nstates in 1:10){
  model <- depmixS4::depmix(
    some.chrom$logratio ~ 1, nstates=nstates, family=gaussian(),
    ntimes=as.integer(count.vec))
  model.fit <- depmixS4::fit(model)
  lik.dt.list[[nstates]] <- data.table(
    nstates, log.lik=depmixS4::logLik(model.fit))
}
lik.dt <- do.call(rbind, lik.dt.list)

ggplot()+
  geom_line(aes(
    nstates, log.lik),
    data=lik.dt)

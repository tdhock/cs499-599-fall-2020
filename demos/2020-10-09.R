## Burns Ch 25 Be Poetic.

data(neuroblastoma, package="neuroblastoma")
str(neuroblastoma)
library(data.table)
data.list <- list()
one.seq.list <- list()
(select.dt <- data.table(profile.id="8", chromosome="1"))
for(data.name in names(neuroblastoma)){
  all.data.dt <- data.table(neuroblastoma[[data.name]])
  data.list[[data.name]] <- all.data.dt
  one.seq.list[[data.name]] <- all.data.dt[select.dt, on=names(select.dt)]
}

## For extra credit timings problem: data table nearest rolling join
## to get a grid of data sets on the log scale from min to max number
## of data.
(count.dt <- data.list$profiles[, .(count=.N), by=.(profile.id, chromosome)])
(desired <- data.table(
  count=count.dt[, 10^seq(log10(min(count)), log10(max(count)), l=5)]))
count.dt[
  desired, .(profile.id, chromosome, actual=x.count, desired=i.count),
  roll="nearest", on=.(count)]
## then do a for loop over each of these data sets, use microbenchmark
## in each iteration.

library(ggplot2)
(gg <- ggplot()+
  geom_rect(aes(
    xmin=min, xmax=max, ymin=-Inf, ymax=Inf, fill=annotation),
    data=one.seq.list$annotations)+
  geom_point(aes(x=position, y=logratio), data=one.seq.list$profiles))

##install.packages("binsegRcpp")
models <- binsegRcpp::binseg_normal(one.seq.list$profiles$logratio, 10)
(segs.dt <- coef(models))
for(col.name in c("start", "end")){
  col.value <- segs.dt[[col.name]]
  set(segs.dt, j=paste0(col.name, ".pos"),
      value=one.seq.list$profiles$position[col.value])
}
segs.dt

segs.dt[, end.before := c(NA, end.pos[-.N]), by=.(segments) ]
change.dt <- data.table(select.dt, segs.dt[1 < start])
change.dt[, changepoint := (start.pos+end.before)/2]
gg.models <- gg+
  facet_grid(segments ~ .)+
  geom_segment(aes(
    x=start.pos, y=mean,
    xend=end.pos, yend=mean),
    color="green",
    data=segs.dt)+
  geom_vline(aes(
    xintercept=changepoint),
    color="green",
    data=change.dt)

## error rate computation.
models.dt <- data.table(models, select.dt)
error.list <- penaltyLearning::labelError(
  models=models.dt,
  labels=one.seq.list$annotations,
  changes=change.dt,
  change.var="changepoint",
  model.vars="segments",
  problem.vars=c("profile.id", "chromosome"))
str(error.list)

gg.models+
  geom_rect(aes(
    xmin=min, xmax=max,
    ymin=-Inf, ymax=Inf,
    linetype=status),
    fill=NA,
    color="black",
    size=2,
    data=error.list$label.errors)



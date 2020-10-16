## optimal detection example
data(neuroblastoma, package="neuroblastoma")
str(neuroblastoma)
library(data.table)
data.list <- list()
one.seq.list <- list()
(select.dt <- data.table(profile.id="4", chromosome="2"))
for(data.name in names(neuroblastoma)){
  all.data.dt <- data.table(neuroblastoma[[data.name]])
  data.list[[data.name]] <- all.data.dt
  one.seq.list[[data.name]] <- all.data.dt[select.dt, on=names(select.dt)]
}

library(ggplot2)
(gg <- ggplot()+
  geom_point(aes(x=position, y=logratio), data=one.seq.list$profiles))

max.segments <- 5
optimal.models <- jointseg::Fpsn(
  one.seq.list$profiles$logratio, max.segments)
optimal.models$t.est
## Want: data table with columns: segments, start, end, mean.
segs.dt.list <- list()
for(n.segs in 1:max.segments){
  end <- optimal.models$t.est[n.segs, 1:n.segs]
  start <- c(1, end[-length(end)]+1)
  segs.dt.list[[paste(n.segs)]] <- data.table(start, end)[, .(
    segments=n.segs,
    mean=mean(one.seq.list$profiles$logratio[start:end]),
    algorithm="DP"           
  ), by=.(start, end)]
}
segs.dt <- do.call(rbind, segs.dt.list)
for(col.name in c("start", "end")){
  col.value <- segs.dt[[col.name]]
  set(segs.dt, j=paste0(col.name, ".pos"),
      value=one.seq.list$profiles$position[col.value])
}
segs.dt[, end.before := c(NA, end.pos[-.N]), by=.(segments) ]
change.dt <- data.table(select.dt, segs.dt[1 < start])
change.dt[, changepoint := (start.pos+end.before)/2]


## how to do the same thing for 100 different sequences?
seq.id.dt <- unique(data.list$profiles[, .(profile.id, chromosome)])
max.segments <- 5
all.segs.dt.list <- list()
all.changes.dt.list <- list()

n.seqs <- 100
all.seq.i <- 1:n.seqs
not.computed.yet <- all.seq.i[! all.seq.i %in% names(all.segs.dt.list)]
for(seq.i in not.computed.yet){
  cat(sprintf("%4d / %4d seqs\n", seq.i, n.seqs))
  select.dt <- seq.id.dt[seq.i]
  for(data.name in names(neuroblastoma)){
    one.seq.list[[data.name]] <- data.list[[data.name]][
      select.dt, on=names(select.dt)]
  }
  this.max <- if(nrow(one.seq.list$profiles) < max.segments){
    nrow(one.seq.list$profiles)
  }else{
    max.segments
  }
  optimal.models <- jointseg::Fpsn(
    one.seq.list$profiles$logratio, this.max)
  segs.dt.list <- list()
  for(n.segs in 1:this.max){
    end <- optimal.models$t.est[n.segs, 1:n.segs]
    start <- c(1, end[-length(end)]+1)
    segs.dt.list[[paste(n.segs)]] <- data.table(start, end)[, .(
      segments=n.segs,
      mean=mean(one.seq.list$profiles$logratio[start:end]),
      algorithm="DP"           
    ), by=.(start, end)]
  }
  segs.dt <- do.call(rbind, segs.dt.list)
  for(col.name in c("start", "end")){
    col.value <- segs.dt[[col.name]]
    set(segs.dt, j=paste0(col.name, ".pos"),
        value=one.seq.list$profiles$position[col.value])
  }
  segs.dt[, end.before := c(NA, end.pos[-.N]), by=.(segments) ]
  change.dt <- data.table(select.dt, segs.dt[1 < start])
  change.dt[, changepoint := (start.pos+end.before)/2]
  all.segs.dt.list[[paste(seq.i)]] <- segs.dt[, data.table(
    select.dt, start.pos, end.pos, segments, mean)]
  all.changes.dt.list[[paste(seq.i)]] <- change.dt[, data.table(
    select.dt, changepoint, segments)]
}  
all.segs.dt <- do.call(rbind, all.segs.dt.list)
all.changes.dt <- do.call(rbind, all.changes.dt.list)

all.select.dt <- seq.id.dt[all.seq.i]
all.labels <- data.list$annotations[
  all.select.dt, on=.(profile.id, chromosome), nomatch=0L]
models.dt <- all.segs.dt[, .SD[1], by=.(profile.id, chromosome, segments)]
error.list <- penaltyLearning::labelError(
  models=models.dt,
  labels=all.labels,
  changes=all.changes.dt,
  change.var="changepoint",
  model.vars="segments",
  problem.vars=c("profile.id", "chromosome"))

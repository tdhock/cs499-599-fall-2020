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
binseg.models <- binsegRcpp::binseg_normal(
  one.seq.list$profiles$logratio, max.segments)
optimal.models <- jointseg::Fpsn(
  one.seq.list$profiles$logratio, max.segments)

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
segs.dt.list$binseg <- data.table(coef(binseg.models), algorithm="BinSeg")
segs.dt <- do.call(rbind, segs.dt.list)
for(col.name in c("start", "end")){
  col.value <- segs.dt[[col.name]]
  set(segs.dt, j=paste0(col.name, ".pos"),
      value=one.seq.list$profiles$position[col.value])
}
segs.dt
segs.dt[, end.before := c(NA, end.pos[-.N]), by=.(algorithm, segments) ]
change.dt <- data.table(select.dt, segs.dt[1 < start])
change.dt[, changepoint := (start.pos+end.before)/2]
(gg.models <- gg+
  facet_grid(segments ~ .)+
  geom_segment(aes(
    x=start.pos, y=mean,
    xend=end.pos, yend=mean,
    color=algorithm, size=algorithm),
    data=segs.dt)+
  geom_vline(aes(
    xintercept=changepoint,
    color=algorithm, size=algorithm),
    data=change.dt))+
  scale_size_manual(values=c(DP=2, BinSeg=1))


## Why is functional pruning so fast slides?

## model selection.


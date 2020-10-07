## Burns Chapter 4 Carve Reality.

data(neuroblastoma, package="neuroblastoma")
## install.packages("neuroblastoma")

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

library(ggplot2)
(gg <- ggplot()+
  geom_rect(aes(
    xmin=min, xmax=max, ymin=-Inf, ymax=Inf, fill=annotation),
    data=one.seq.list$annotations)+
  geom_point(aes(x=position, y=logratio), data=one.seq.list$profiles))

##install.packages("binsegRcpp")
models <- binsegRcpp::binseg_normal(one.sequence$logratio, 10)

ggplot()+
  geom_point(aes(
    segments, loss),
    data=models)

(segs.dt <- coef(models))

for(col.name in c("start", "end")){
  col.value <- segs.dt[[col.name]]
  set(segs.dt, j=paste0(col.name, ".pos"),
      value=one.seq.list$profiles$position[col.value])
}

gg+
  facet_grid(segments ~ .)+
  geom_segment(aes(
    x=start.pos, y=mean,
    xend=end.pos, yend=mean),
    color="green",
    data=segs.dt)


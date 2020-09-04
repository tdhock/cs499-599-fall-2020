## Covid slide announcement.

## times=3 argument for microbenchmark
library(data.table)
(linear.scale.seq <- seq(5, 150, by=5)) #evenly spaced on linear scale axes.
## 10^seq... for log-scale sequences.
(log.scale.seq <- as.integer(c(10^seq(1, 2, l=10), 150))) #evenly spaced on log scale axes.
time.dt.list <- list()
for(N in log.scale.seq){
  X <- iris[1:N, 1:4]
  timing.df <- microbenchmark::microbenchmark(hclust={
    d.mat <- stats::dist(X, method="manhattan")
    as.matrix(d.mat)
    cl.tree <- stats::hclust(d.mat, method="single")
    stats::cutree(cl.tree, k=3)
  }, kmeans={
    stats::kmeans(X, 3)
  }, times=10)
  time.dt.list[[paste(N)]] <- data.table(
    N, timing.df)
}
time.dt <- do.call(rbind, time.dt.list)

library(ggplot2)
time.dt[, seconds := time/1e9]
ggplot()+
  geom_point(aes(
    N, seconds, color=expr),
    data=time.dt)

## geom_ribbon quartile bands to show variance.
time.stats <- time.dt[, .(
  median.seconds=median(seconds),#central trend
  mean.seconds=mean(seconds),
  sd.seconds=sd(seconds),
  q25=quantile(seconds, 0.25),#lower confidence band
  q75=quantile(seconds, 0.75)#upper confidence band
), by=.(N, expr)]
## Exercise: how to plot mean +/- SD instead of median and quartiles?
ggplot()+
  geom_point(aes(
    N, median.seconds, color=expr),
    data=time.stats)+
  geom_ribbon(aes(
    x=N, #horizontal position
    ## ymax=mean.seconds + sd.seconds, #upper
    ## ymin=mean.seconds - sd.seconds, #lower limit of ribbon
    ymax=q75,
    ymin=q25,
    fill=expr),
    alpha=0.5,
    data=time.stats)+
  scale_x_log10()+
  scale_y_log10()

## ggdendro for making plots of cluster trees with a different linkage
## method in each panel.
segs.dt.list <- list()
lab.dt.list <- list()
for(linkage.method in c("single", "average")){
  cl.tree <- stats::hclust(d.mat, method=linkage.method)
  dendro.result <- ggdendro::dendro_data(cl.tree)
  (label.dt <- data.table(dendro.result[["labels"]]))
  label.dt[, Species := iris[as.integer(label), "Species"] ]
  segs.dt.list[[linkage.method]] <- data.table(
    linkage.method,
    dendro.result[["segments"]])
  lab.dt.list[[linkage.method]] <- data.table(
    linkage.method,
    label.dt)
}
(lab.dt <- do.call(rbind, lab.dt.list))
(segs.dt <- do.call(rbind, segs.dt.list))

ggplot()+
  facet_grid(. ~ linkage.method, labeller=label_both)+
  geom_segment(aes(
    x=x,
    y=y,
    xend=xend,
    yend=yend),
    data=segs.dt)+
  geom_point(aes(
    x, y, color=Species),
    data=lab.dt)

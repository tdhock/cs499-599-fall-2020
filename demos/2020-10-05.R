## Reading: Burns Ch 16 Travel in Space. (computing train/valid sum of squares for kmeans)

## New homework.

## neuroblastoma data.
data(neuroblastoma, package="neuroblastoma")
## install.packages("neuroblastoma")

str(neuroblastoma)
library(data.table)
(pro.dt <- data.table(neuroblastoma[["profiles"]]))

one.sequence <- pro.dt[profile.id=="8" & chromosome=="1"]
library(ggplot2)
ggplot()+
  geom_point(aes(x=position, y=logratio), data=one.sequence)

##install.packages("binsegRcpp")
models <- binsegRcpp::binseg_normal(one.sequence$logratio, 5)

ggplot()+
  geom_point(aes(
    segments, loss),
    data=models)

(segs.dt <- coef(models))

one.sequence[, index := seq_along(logratio)]
ggplot()+
  geom_point(aes(
    x=index, y=logratio),
    data=one.sequence)+
  geom_segment(aes(
    x=start, y=mean,
    xend=end, yend=mean),
    color="green",
    data=segs.dt)+
  facet_grid(segments ~ ., labeller=label_both)+
  geom_vline(aes(
    xintercept=start-0.5),
    color="green",
    data=segs.dt[start>1])

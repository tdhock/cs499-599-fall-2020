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
data.vec <- one.seq.list$profiles$logratio
optimal.models <- jointseg::Fpsn(
  data.vec, max.segments)

mean.value <- mean(data.vec)
rbind(ours=sum((mean.value-data.vec)^2),
      Fpsn=optimal.models$allCost[1, length(data.vec)])
## sum of squares - (sum of data)^2 / number of data
Qj <- cumsum(data.vec^2)
Sj <- cumsum(data.vec)
rbind(
  ours=Qj - Sj^2 / seq_along(data.vec),
  Fpsn=optimal.models$allCost[1,])

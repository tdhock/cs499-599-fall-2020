some.ids <- c(
  "20167.22", "20168.19", "20168.11", "20154.4", "20159.3", "20159.1", 
  "30011.22", "20005.2", "30020.4")
one.id <- "20167.22"
data.list <- list()
for(data.type in c("signals", "labels")){
  f <- sprintf("%s.csv.gz", data.type)
  if(!file.exists(f)){
    u <- paste0("https://raw.githubusercontent.com/tdhock/LOPART-paper/master/data-for-LOPART-", f)
    download.file(u, f)
  }
  type.dt <- data.table::fread(f)
  data.list[[data.type]] <- type.dt[one.id, on="sequenceID"]
}
data.list

library(ggplot2)
ggplot()+
  geom_point(aes(
    data.i, logratio),
    data=data.list$signals)

max.segs <- 20
cpt.fit <- changepoint::cpt.mean(
  data.list$signals$logratio, method="SegNeigh", penalty="Manual", Q=max.segs)

js.fit <- jointseg::Fpsn(data.list$signals$logratio, max.segs)
bs.fit <- binsegRcpp::binseg_normal(data.list$signals$logratio, max.segs)

library(data.table)
loss.dt <- rbind(
  data.table(algorithm="DP", segments=1:max.segs, loss=js.fit$J.est),
  bs.fit[, .(algorithm="BinSeg", segments, loss)])
ggplot()+
  geom_line(aes(
    segments, loss, color=algorithm),
    data=loss.dt)

if(!file.exists("zip.test.gz")){
  download.file(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.test.gz",
    "zip.test.gz")
}

## download and install data.table package from internet.
if(!require("data.table")){
  install.packages("data.table")
}

zip.test <- data.table::fread("zip.test.gz")

obs.i <- 1000
one.digit <- zip.test[obs.i]
one.digit.tall <- data.table::data.table(
  col=rep(1:16, 16),
  row=-rep(1:16, each=16),
  intensity=as.numeric(one.digit[, -1, with=FALSE]))
library(ggplot2)
ggplot()+
  geom_tile(aes(
    x=col, y=row, fill=intensity),
    data=one.digit.tall)+
  scale_fill_gradient(low="black", high="white")

## List of data tables idiom.

## 1. initialize an empty list variable before the for loop (typically
## variable name suffix something.list).
some.digits.list <- list()
## 2. for loop (or several nested for loops) over different values of
## data that you want to plot, here obs.i means different examples of
## digits.
for(label.i in 0:9){
  all.label.data <- zip.test[V1 == label.i]
  for(obs.i in 1:7){
    one.digit <- all.label.data[obs.i]
    ## 3. save a data table as an element of that list that was
    ## initialized before the for loop.
    save.id <- paste(label.i, obs.i)
    some.digits.list[[save.id]] <- data.table::data.table(
      obs.i, 
      col=rep(1:16, 16),
      row=-rep(1:16, each=16),
      intensity=as.numeric(one.digit[, -1, with=FALSE]),
      label=one.digit[[1]])
  }
}
## 4. combine all data tables in the list into a single data table
## that we can plot.
some.digits <- do.call(rbind, some.digits.list)

table(zip.test[, 1])

gg <- ggplot()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(obs.i ~ label, labeller=label_both)+
  coord_equal(expand=FALSE)+
  geom_tile(aes(
    col, row, fill=intensity),
    data=some.digits)+
  scale_fill_gradient(low="black", high="white")
png("2020-08-21.png", width=4, height=4, res=100)
print(gg)
dev.off()

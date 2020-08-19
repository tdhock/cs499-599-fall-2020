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

obs.i <- 1
one.digit <- zip.test[obs.i]

## we need a data table with columns: row, col, intensity... and one
## row for every pixel.

## we need to know dimensions for a single image = 16 x 16.

## change data from [-1, 1] => {0,1}.

## nested for loop? for loops are ok/efficient if what you are
## manipulating inside that for loop is a vector/matrix, but
## bad/inefficient if scalar.

one.digit.tall <- data.table::data.table(
  col=rep(1:16, 16),
  row=-rep(1:16, each=16),
  intensity=as.numeric(one.digit[, -1, with=FALSE]))
library(ggplot2)
ggplot()+
  geom_tile(aes(
    col, row, fill=intensity),
    data=one.digit.tall)+
  scale_fill_gradient(low="black", high="white")


some.digits.list <- list()
for(obs.i in 1:10){
  one.digit <- zip.test[obs.i]
  some.digits.list[[obs.i]] <- data.table::data.table(
    obs.i, 
    col=rep(1:16, 16),
    row=-rep(1:16, each=16),
    intensity=as.numeric(one.digit[, -1, with=FALSE]))
}
some.digits <- do.call(rbind, some.digits.list)

ggplot()+
  facet_wrap("obs.i")+
  geom_tile(aes(
    col, row, fill=intensity),
    data=some.digits)+
  scale_fill_gradient(low="black", high="white")

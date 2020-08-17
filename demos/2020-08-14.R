if(!file.exists("SAheart.data")){
  download.file(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data",
    "SAheart.data")
}

## list all files in current directory.
dir()

## print working directory.
getwd()

## download and install data.table package from internet.
if(!require("data.table")){
  install.packages("data.table")
}

SAheart.dt <- data.table::fread("SAheart.data")
SAheart.dt = data.table::fread("SAheart.data")

##prints a compact representation of the structure of any R object.
str(SAheart.dt)

SAheart.dt[ row.selection, column.selection ]

## data table with a subset of columns.
SAheart.dt[ , .(sbp, tobacco, ldl, adiposity) ]

## data table with a subset of columns and rows.
SAheart.dt[10:20 , .(sbp, tobacco, ldl, adiposity) ]

## data table with a subset of columns and rows.
subset.dt <- SAheart.dt[
  famhist == "Present" ,
  .(sbp, tobacco, ldl, adiposity) ]

str(subset.dt)

subset.mat <- as.matrix(subset.dt)
str(subset.mat)

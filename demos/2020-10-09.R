## Burns Ch 25 Be Poetic.

data(neuroblastoma, package="neuroblastoma")
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

## For extra credit timings problem: data table nearest rolling join
## to get a grid of data sets on the log scale from min to max number
## of data.
(count.dt <- data.list$profiles[, .(count=.N), by=.(profile.id, chromosome)])
desired <- data.table(
  count=count.dt[, 10^seq(log10(min(count)), log10(max(count)), l=10)])
count.dt[
  desired, .(profile.id, chromosome, actual=x.count, desired=i.count),
  roll="nearest", on=.(count)]
# then do a for loop over each of these data sets, use microbenchmark
# in each iteration.

## error rate computation.

## model selection.


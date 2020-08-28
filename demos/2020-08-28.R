## Announcements for in person class next week.

## - Set up your cohort rotations and communicate them to the students

## - Remind students NOT to come in person if they're sick or have been exposed to someone who's sick

## - Remind students to do their health check app

## - Watch for last-minute changes in NAU in-person attendance policy which can occur at any time and will be critical to follow

## - Be strict in enforcing classroom rules (who attends when, coming sick, etc.)

## - Start on time, try to end a little early, get students out of class right on time to help with traffic management. Hold your after class discussions elsewhere

## - Clean surfaces before and after, as much as possible

## - Wear your clear mask so students can see you talking

## - Test your classroom tech before Monday; get good at booting up your tech fast and helping students troubleshoot

library(data.table)
library(mclust)
library(ggplot2)

## TODO manual color legends
head(iris)
gg.object <- ggplot()+
  geom_point(aes(
    Sepal.Length, Sepal.Width, color=Species),
    data=iris)+
  scale_color_manual(values=c(
    versicolor="red",
    virginica="black",
    setosa="blue",
    kmeans="black",
    Mclust="red"))

pdf("2020-08-28-iris.pdf", width=10, height=5)#start drawing to file.
print(gg.object)#draw something to previously started graphics file.
dev.off()#stop drawing to prev graphics device file.
system("evince 2020-08-28-iris.pdf")

## TODO pdf, png graphics devices.

## what is the difference between png(raster, matrix of pixels,
## compressed, NOT good format if you want to zoom in to see details,
## or if you want to print it out, in those cases you will probably
## start to see the pixels) and pdf? (vector, abstract description of
## how to draw the picture at any size, good for zooming in and
## printout, bad if there are a lot of things drawn in the plot,
## millions of data points = bad/slow rendering)

## Rule of thumb: if you have large number of data points, use png.
## otherwise, use pdf.

## res means resolution, rule of thumb: for screen rendering res=100
## is good, for print res=400 is good.
png("2020-08-28-iris.png", width=10, height=5, units="in", res=100)
print(gg.object)#draw something to previously started graphics file.
dev.off()#stop drawing to prev graphics device file.

## use modelNames to consider one type of model, to get increasing
## log.lik curve. Each model type has a different number of
## parameters, and the Mclust function uses the BIC model selection
## criterion to choose one of those models (maybe not the same one for
## each value of G, which is why the loglik curve may decrease).
## Figure
## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/bin/nihms793803f2.jpg
## from article https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/
## local copy linked on course web page github README.
iris.model <- mclust::Mclust(
  iris[, 1:4],
  G=3, #number of clusters
  modelNames="EII") # model constraints
plot(iris.model, what = "classification")

## from ?mclustModelNames

## ‘"EII"’ spherical, equal volume

## ‘"VII"’ spherical, unequal volume

## ‘"EEI"’ diagonal, equal volume and shape

## ‘"VEI"’ diagonal, varying volume, equal shape

## ‘"EVI"’ diagonal, equal volume, varying shape

## ‘"VVI"’ diagonal, varying volume and shape

## use initialization subset to reduce computation time.
n.clusters <- 10
zip.dt <- data.table::fread("zip.test.gz")
zip.mat <- as.matrix(zip.dt[, -1])
fit <- mclust::Mclust(
  zip.mat,
  n.clusters,
  initialization=list(subset=1:100))

## Use both modelNames and initialization in a for loop, saving ARI
## and log likelihood in each iteration.
n.clusters.vec <- seq(1, 20, by=2)
metrics.dt.list <- list()
for(n.clusters in n.clusters.vec){
  fit <- mclust::Mclust(
    zip.mat,
    n.clusters,
    initialization=list(subset=1:100),
    verbose = 0,
    modelNames="EII")
  one.result <- data.table(
    n.clusters,
    log.lik=fit$loglik,
    ARI=pdfCluster::adj.rand.index(
      fit$classification, zip.dt[["V1"]]))
  print(one.result)
  metrics.dt.list[[paste(n.clusters)]] <- one.result
}
metrics.dt <- do.call(rbind, metrics.dt.list)

ggplot()+
  geom_line(aes(
    n.clusters, ARI),
    data=metrics.dt)

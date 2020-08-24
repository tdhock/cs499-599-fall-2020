library(animation)
animation::kmeans.ani()

x = cbind(X1 = runif(50), X2 = runif(50))
centers = 4
hints = c("Move centers!", "Find cluster?")
pch = 1:centers
col = 1:centers
x = as.matrix(x)
ocluster = sample(centers, nrow(x), replace = TRUE)
if (length(centers) == 1) {
  centers = x[sample(nrow(x), centers), ]
}else{
  centers = as.matrix(centers)
}
numcent = nrow(centers)
dst = matrix(nrow = nrow(x), ncol = numcent)
j = 1
pch = rep(pch, length = numcent)
col = rep(col, length = numcent)

for (j in 1:ani.options("nmax")) {
  dev.hold()
  plot(x, pch = pch[ocluster], col = col[ocluster], panel.first = grid())
  mtext(hints[1], 4)
  points(centers, pch = pch[1:numcent], cex = 3, lwd = 2, 
         col = col[1:numcent])
  ani.pause()
  for (i in 1:numcent) {
    dst[, i] = sqrt(apply((t(t(x) - unlist(centers[i, 
                                                   ])))^2, 1, sum))
  }
  ncluster = apply(dst, 1, which.min)
  plot(x, type = "n")
  mtext(hints[2], 4)
  grid()
  ocenters = centers
  for (i in 1:numcent) {
    xx = subset(x, ncluster == i)
    polygon(xx[chull(xx), ], density = 10, col = col[i], 
            lty = 2)
    points(xx, pch = pch[i], col = col[i])
    centers[i, ] = apply(xx, 2, mean)
  }
  points(ocenters, cex = 3, col = col[1:numcent], pch = pch[1:numcent], 
         lwd = 2)
  ani.pause()
  if (all(centers == ocenters)) 
    break
  ocluster = ncluster
}

plot(Sepal.Length ~ Petal.Length, iris, col=Species)
x <- iris[, c("Petal.Length", "Sepal.Length")]
cl <- stats::kmeans(x, 3)
x11()
plot(x, col=cl$cluster)

## ggplot version.
library(data.table)
cl <- stats::kmeans(x, 5)
cluster.dt <- rbind(
  data.table(x, variable="Species(label)", value=as.integer(iris$Species)),
  data.table(x, variable="kmeans(clustering)", value=cl$cluster))
library(ggplot2)
## try changing the number of clusters K and see how the ARI changes.
ARI <- pdfCluster::adj.rand.index(iris$Species, cl$cluster)
ggplot()+
  ggtitle(paste(
    "ARI=", ARI))+
  geom_point(aes(
    Petal.Length, Sepal.Length, color=factor(value)),
    data=cluster.dt)+
  facet_grid(. ~ variable)+
  coord_equal()


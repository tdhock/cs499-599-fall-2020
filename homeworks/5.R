library(data.table)
library(ggplot2)
set.seed(1)
N <- 150
noise <- 0.5
halfcircle <- function(r, center = c(0, 0), class, sign) {
  angle <- runif(N, 0, pi)
  rad <- rnorm(N, r, noise)
  data.table(
    x = rad * cos(angle) + center[1],
    y = sign * rad * sin(angle) + center[2],
    class = factor(class))
}
pts <- rbind(
  halfcircle(4, c(0, 0), 1, 1),
  halfcircle(4, c(4, 2), 2, -1))

x <- as.matrix(pts[, 1:2])
data(spirals, package="kernlab")
x <- spirals
d <- stats::dist(x)
h <- stats::hclust(d, "single")
hc.vec <- stats::cutree(h, 2)


spec.result <- kernlab::specc(x, 2)
sp.vec <- spec.result@.Data

set.seed(1)
k <- kernlab::kernelMatrix(kernlab::rbfdot(0.1), x)
e <- eigen(k, symmetric=TRUE)
xi <- e[["vectors"]][, 1:5]
plot(xi)
yi <- xi/sqrt(rowSums(xi^2))
plot(yi)
sp.vec <- stats::kmeans(yi, 2)[["cluster"]]
to.show <- data.table(
  unname(x),
  cluster = sp.vec)
ggplot()+
  geom_point(aes(
    V1, V2, color=factor(cluster)),
    data=to.show)+
  coord_equal()

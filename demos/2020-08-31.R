head(iris)
str(iris)

dist.mat <- stats::dist(iris[, 1:4])
str(dist.mat)
N <- nrow(iris)

## number of entries in lower triangle of N x N matrix.
N*(N-1)/2

iris5 <- iris[1:5, 1:4]
dist5.mat <- stats::dist(iris5)
as.matrix(dist5.mat)

## compute one distance matrix entry. (euclidean)
p1 <- iris[1, 1:4]
p2 <- iris[2, 1:4]
sqrt(sum((p1-p2)^2))
d <- as.numeric(p1-p2)
sqrt(t(d) %*% d)

## compute L1 distance.
sum(abs(p1-p2))
as.matrix(stats::dist(iris5, method="manhattan"))

## compute cluster tree.
cl.tree <- stats::hclust(dist.mat)
plot(cl.tree)

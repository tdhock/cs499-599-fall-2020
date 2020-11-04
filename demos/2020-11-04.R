library(ggplot2)
library(data.table)
## 0. Finish example from last time with iris data.

## 0. iris data scatterplot two columns with one principal component line
##    and orthogonal projection segments.
i.df <- iris[, 2:3]
str(i.df)
gg <- ggplot()+
  geom_point(aes(
    y=Petal.Length, x=Sepal.Width),
    data=i.df)+
  coord_equal()
gg

pc.fit <- prcomp(i.df)
str(pc.fit)
pc.fit[["rotation"]]


lambda.vec <- pc.fit$x[,1] ## this is lambda from the book!
line.dt.list <- list()
PC1 <- pc.fit[["rotation"]][,1]
PC1.mat <- matrix(PC1, nrow=nrow(iris), ncol=2, byrow=TRUE)
mean.vec <- colMeans(i.df)
mean.mat <- matrix(mean.vec, nrow=nrow(iris), ncol=2, byrow=TRUE)
pred.mat <- mean.mat + PC1.mat * pc.fit[["x"]][, 1]
colnames(pred.mat) <- colnames(i.df)
pred.dt <- data.table(pred.mat)
(gg.point <- gg+
  geom_point(aes(
    y=Petal.Length, x=Sepal.Width),
    size=2,
    color="red",
    data=pred.dt))
abline.dt <- data.table(
  method="slope/intercept",
  ## f(Petal.Length) =
  ## mean(Petal.Length)
  ## - mean(Sepal.Width)*PC1(Petal.Length)/PC1(Sepal.Width)
  ## + f(Sepal.Width)*PC1(Petal.Length)/PC1(Sepal.Width)
  intercept=mean.vec[["Petal.Length"]] 
  -mean.vec[["Sepal.Width"]]*PC1[["Petal.Length"]]/PC1[["Sepal.Width"]],
  slope=PC1[["Petal.Length"]]/PC1[["Sepal.Width"]])
segs.dt <- data.table(data=i.df, pred=pred.dt)
gg.point+
  geom_abline(aes(
    color=method,
    slope=slope,
    intercept=intercept),
    data=abline.dt)+
  geom_segment(aes(
    x=data.Sepal.Width,
    xend=pred.Sepal.Width,
    y=data.Petal.Length,
    yend=pred.Petal.Length),
    data=segs.dt)

## sum of squared error using first PC.
sum( (pred.mat - i.df)^2)

## 1. MNIST scatterplot of digit labels on first two pca directions (all
## classes).
if(file.exists("figure-fashion-mnist-data.rds")){
  data.list <- readRDS("figure-fashion-mnist-data.rds")
}else{
  data.list <- list(
    fashion=keras::dataset_fashion_mnist(),
    digits=keras::dataset_mnist())
  saveRDS(data.list, "figure-fashion-mnist-data.rds")
}
str(data.list$digits)

X.mat.list <- list()
obs.i.vec <- 1:1000
for(obs.i in obs.i.vec){
  X.mat.list[[obs.i]] <- as.numeric(data.list$digits$train$x[obs.i, , ])
}
X.mat <- do.call(rbind, X.mat.list)

digit <- data.list$digits$train$y[obs.i.vec]
## specifying rank arg is faster, use it if you don't need all the
## principal components.
digits.fit <- prcomp(X.mat, rank=2)
digits.pc.dt <- data.table(
  digits.fit$x, digit)

ggplot()+
  geom_text(aes(
    PC1, PC2, label=digit),
    data=digits.pc.dt)

## 2. mean and first two eigendigits. reconstruction % and number of
##    components in different panels for a single example. (MLAPP Fig
##    12.6)
X.one.class <- X.mat[digit==5,]
pr.one.class <- prcomp(X.one.class, rank=2)

obs.i <- 3
n.pixels <- 28
one.digit.dt <- data.table(
  ##intensity=X.one.class[obs.i,],#data
  ##intensity=pr.one.class$center,#mean
  ##intensity=pr.one.class$rotation[,1] * pr.one.class$x[obs.i,1],#1st pc only.
  intensity=pr.one.class$center +#reconstruction using mean + 1st pc.
    pr.one.class$rotation[,1] * pr.one.class$x[obs.i,1],
  row=rep(1:n.pixels, n.pixels),
  col=rep(1:n.pixels, each=n.pixels))
ggplot()+
  geom_tile(aes(
    col, -row, fill=intensity),
    data=one.digit.dt)+
  scale_fill_gradient(low="black", high="white")


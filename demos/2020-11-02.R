## 0. iris data scatterplot two columns with one principal component line
##    and orthogonal projection segments.
i.df <- iris[, 2:3]
str(i.df)

library(ggplot2)
gg <- ggplot()+
  geom_point(aes(
    y=Petal.Length, x=Sepal.Width),
    data=i.df)+
  coord_equal()

pc.fit <- prcomp(i.df)
str(pc.fit)
pc.fit[["rotation"]]


lambda.vec <- seq(-4, 4)
lambda.vec <- pc.fit$x[,1]
line.dt.list <- list()
PC1 <- pc.fit[["rotation"]][,1]
f <- function(l)mean.vec + l * PC1
mean.vec <- colMeans(i.df)
for(lambda in lambda.vec){
  line.vec <- f(lambda)
  line.dt.list[[paste(lambda)]] <- data.table(lambda, t(line.vec))
}
line.dt <- do.call(rbind, line.dt.list)
line.dt[, method := "grid"]
(gg.line <- gg+
  geom_line(aes(
    y=Petal.Length, x=Sepal.Width, color=method),
    size=2,
    data=line.dt))

##y= mx + b

## Petal.Length = slope * Sepal.Width + intercept

## What is the intercept? that is b where 

## f(Petal.Length) = b = mean(Petal.Length) + lambda * PC1(Petal.Length)

## f(Sepal.Width) = 0 = mean(Sepal.Width)  + lambda * PC1(Sepal.Width)

## lambda = -mean(Sepal.Width) / PC1(Sepal.Width)
lambda.intercept <- -mean.vec[["Sepal.Width"]]/PC1[["Sepal.Width"]]
f(lambda.intercept)
abline.dt <- data.table(
  method="slope/intercept",
  ## f(Petal.Length) =
  ## mean(Petal.Length)
  ## - mean(Sepal.Width)*PC1(Petal.Length)/PC1(Sepal.Width)
  ## + f(Sepal.Width)*PC1(Petal.Length)/PC1(Sepal.Width)
  intercept=mean.vec[["Petal.Length"]] +
    lambda.intercept*PC1[["Petal.Length"]],
  slope=PC1[["Petal.Length"]]/PC1[["Sepal.Width"]])

## [f(Sepal.Width) - mean(Sepal.Width)]/PC1(Sepal.Width) = lambda

## f(Petal.Length) = mean(Petal.Length) +
## [(f(Sepal.Width)-mean(Sepal.Width))/PC1(Sepal.Width)] * PC1(Petal.Length)

gg.line+
  geom_abline(aes(
    color=method,
    slope=slope,
    intercept=intercept),
    data=abline.dt)

## TODO orthogonal projection onto line.

library(keras)
## from https://github.com/rstudio/keras/issues/937
if(FALSE){
  install.packages("keras")
  keras::install_keras(version = "2.1.6", tensorflow = "1.5")
}
keras::use_implementation("keras")
keras::use_backend("tensorflow")

library(ggplot2)
library(data.table)
i.df <- data.table(iris[, 2:3])
str(i.df)
gg <- ggplot()+
  geom_point(aes(
    y=Petal.Length, x=Sepal.Width, color=data.type),
    data=data.table(i.df, data.type="data"))+
  scale_color_manual(values=c(data="black", model="red"))+
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
abline.dt <- data.table(
  method="slope/intercept",
  ## f(Petal.Length) =
  ## mean(Petal.Length)
  ## - mean(Sepal.Width)*PC1(Petal.Length)/PC1(Sepal.Width)
  ## + f(Sepal.Width)*PC1(Petal.Length)/PC1(Sepal.Width)
  intercept=mean.vec[["Petal.Length"]] 
  -mean.vec[["Sepal.Width"]]*PC1[["Petal.Length"]]/PC1[["Sepal.Width"]],
  slope=PC1[["Petal.Length"]]/PC1[["Sepal.Width"]])
segs.dt <- data.table(data=i.df, pred=pred.dt, data.type="model")
gg+
  geom_abline(aes(
    color=data.type,
    slope=slope,
    intercept=intercept),
    data=data.table(abline.dt, data.type="model"))+
  geom_segment(aes(
    x=data.Sepal.Width,
    xend=pred.Sepal.Width,
    color=data.type,
    y=data.Petal.Length,
    yend=pred.Petal.Length),
    data=segs.dt)

n.input.output <- ncol(i.df)
n.code.units <- 1
model <- keras::keras_model_sequential() %>%
  keras::layer_dense(
    input_shape = n.input.output, units = n.code.units, name="code") %>%
  keras::layer_dense(units = n.input.output)
compiled.model <- keras::compile(
  model,
  optimizer=keras::optimizer_sgd(),
  loss=keras::loss_mean_squared_error)
i.mat <- as.matrix(i.df)
fit.model <- keras::fit(compiled.model, x=i.mat, y=i.mat)

str(fit.model)
fit.model[["metrics"]][["loss"]]

pred.mat <- predict(compiled.model, i.mat)#last layer.
pred.dt <- data.table(pred.mat)
names(pred.dt) <- names(i.df)

## predictions are linear since no activation was specified in
## layer_dense!
gg+
  geom_point(aes(
    Sepal.Width, Petal.Length, color=data.type),
    data=data.table(pred.dt, data.type="model"))

# code layer predictions.
intermediate_layer_model <- keras::keras_model(
  inputs = compiled.model$input,
  outputs = keras::get_layer(compiled.model, "code")$output)
intermediate_output <- predict(intermediate_layer_model, i.mat)


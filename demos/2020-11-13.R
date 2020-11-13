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
  scale_color_manual(values=c(
    data="black",
    PCA="red",
    autoencoder="deepskyblue"))+
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
segs.dt <- data.table(data=i.df, pred=pred.dt, data.type="PCA")
(gg.pca <- gg+
  geom_abline(aes(
    color=data.type,
    slope=slope,
    intercept=intercept),
    data=data.table(abline.dt, data.type="PCA"))+
  geom_segment(aes(
    x=data.Sepal.Width,
    xend=pred.Sepal.Width,
    color=data.type,
    y=data.Petal.Length,
    yend=pred.Petal.Length),
    data=segs.dt))

n.input.output <- ncol(i.df)
n.code.units <- 1
model <- keras::keras_model_sequential() %>%
  keras::layer_dense(
    input_shape = n.input.output,#first/input layer
    units = n.code.units, name="code", activation="sigmoid") %>%#second layer
  keras::layer_dense(units = n.input.output)#third/output layer
compiled.model <- keras::compile(
  model,
  optimizer=keras::optimizer_sgd(lr=0.025),
  loss=keras::loss_mean_squared_error)
i.mat <- as.matrix(i.df)
fit.stats <- keras::fit(
  compiled.model,
  x=i.mat,#inputs
  y=i.mat,#outputs (same as inputs for autoencoder)
  batch_size = nrow(i.mat),
  epochs=2000)
pred.mat <- predict(compiled.model, i.mat)#last layer.
pred.dt <- data.table(pred.mat)
names(pred.dt) <- names(i.df)
## predictions are linear since no activation was specified in
## layer_dense!
gg.pca+
  geom_point(aes(
    Sepal.Width, Petal.Length, color=data.type),
    data=data.table(pred.dt, data.type="autoencoder"))

# code layer predictions.
intermediate_layer_model <- keras::keras_model(
  inputs = compiled.model$input,
  outputs = keras::get_layer(compiled.model, "code")$output)
intermediate_output <- predict(intermediate_layer_model, i.mat)

## simulated nonlinear data.
set.seed(1)
N <- 100
x <- runif(N, 0, 10)
y <- sin(x)+x/10+rnorm(N, 0, 0.2)
sim.dt <- data.table(x, y)
(gg.sim <- ggplot()+
  scale_color_manual(values=c(
    data="black",
    PCA="red",
    autoencoder="deepskyblue"))+
   geom_point(aes(
     x, y, color=data.type),
     data=data.table(sim.dt, data.type="data")))

pc.fit <- prcomp(sim.dt)
PC1 <- pc.fit[["rotation"]][,1]
PC1.mat <- matrix(PC1, nrow=nrow(sim.dt), ncol=2, byrow=TRUE)
mean.vec <- colMeans(sim.dt)
mean.mat <- matrix(mean.vec, nrow=nrow(sim.dt), ncol=2, byrow=TRUE)
pred.mat <- mean.mat + PC1.mat * pc.fit[["x"]][, 1]
colnames(pred.mat) <- colnames(sim.dt)
pred.dt <- data.table(pred.mat)
abline.dt <- data.table(
  method="slope/intercept",
  ## f(Petal.Length) =
  ## mean(Petal.Length)
  ## - mean(Sepal.Width)*PC1(Petal.Length)/PC1(Sepal.Width)
  ## + f(Sepal.Width)*PC1(Petal.Length)/PC1(Sepal.Width)
  intercept=mean.vec[["y"]] 
  -mean.vec[["x"]]*PC1[["y"]]/PC1[["x"]],
  slope=PC1[["y"]]/PC1[["x"]])
segs.dt <- data.table(data=sim.dt, pred=pred.dt, data.type="PCA")
(gg.pca <- gg.sim+
   coord_equal()+
  geom_abline(aes(
    color=data.type,
    slope=slope,
    intercept=intercept),
    data=data.table(abline.dt, data.type="PCA"))+
  geom_segment(aes(
    x=data.x,
    xend=pred.x,
    color=data.type,
    y=data.y,
    yend=pred.y),
    data=segs.dt))

n.input.output <- ncol(sim.dt)
n.code.units <- 2
model <- keras::keras_model_sequential() %>%
  keras::layer_dense(
    input_shape = n.input.output,#first/input layer
    # unit = activation( weight * input_unit )
    units = n.code.units, name="code", activation="sigmoid") %>%#second layer
  keras::layer_dense(units = n.input.output)#third/output layer
compiled.model <- keras::compile(
  model,
  optimizer=keras::optimizer_sgd(),
  loss=keras::loss_mean_squared_error)
sim.mat <- as.matrix(sim.dt)
fit.stats <- keras::fit(
  compiled.model,
  x=sim.mat,#inputs
  y=sim.mat,#outputs (same as inputs for autoencoder)
  ##batch_size = nrow(sim.mat),
  epochs=1000)
pred.mat <- predict(compiled.model, sim.mat)#last layer.
pred.dt <- data.table(pred.mat)
names(pred.dt) <- names(sim.dt)
## predictions are linear since no activation was specified in
## layer_dense!
gg.pca+
  geom_point(aes(
    x, y, color=data.type),
    data=data.table(pred.dt, data.type="autoencoder"))

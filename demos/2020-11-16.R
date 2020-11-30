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
str(X.mat)

X.sc.mat <- (X.mat-max(X.mat))/(min(X.mat)-max(X.mat))
range(X.sc.mat)
digits.fit <- prcomp(X.sc.mat, rank=2)

## loss? squared error between predicted and actual data.
pred.mat <- matrix(digits.fit$center, nrow(X.sc.mat), ncol(X.sc.mat), byrow=TRUE)
for(pc.num in 1:ncol(digits.fit$x)){
  rot.vec <- digits.fit[["rotation"]][,pc.num]
  rot.mat <- matrix(rot.vec, nrow=nrow(X.sc.mat), ncol=ncol(X.sc.mat), byrow=TRUE)
  pred.mat <- pred.mat + rot.mat * digits.fit[["x"]][, pc.num]
  print(total.squared.error <- sum((pred.mat - X.sc.mat)^2))
}
total.squared.error/nrow(X.sc.mat)

n.hidden <- 100
activation <- "relu"
model <- keras::keras_model_sequential() %>%
  keras::layer_dense(
    input_shape = ncol(X.sc.mat),#input
    units = n.hidden,
    activation = activation) %>% #first hidden
  keras::layer_dense(units = 2, activation = activation, name="code") %>%
  keras::layer_dense(units = n.hidden, activation=activation) %>%
  keras::layer_dense(units = ncol(X.sc.mat))

## n parameters in a layer = number of weights used for predicting all
## of the hidden units in that given the previous layer = (number of
## previous layer units + 1) * (number of units in this layer), the 1
## comes from an intercept/bias term, which can be turned off with
## use_bias=FALSE.
compiled <- keras::compile(
  model,
  loss=keras::loss_mean_squared_error,
  optimizer=keras::optimizer_adagrad(lr=0.06))
fit.data <- keras::fit(
  compiled, x=X.sc.mat, y=X.sc.mat, epochs=100)
pred.keras.mat <- predict(compiled, X.sc.mat)
rbind(autoencoder=sum((pred.keras.mat-X.sc.mat)^2),
      pca=total.squared.error)

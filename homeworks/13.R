library(ggplot2)
library(data.table)
if(FALSE){
  install.packages("Rtsne") 
  install.packages("princurve") 
  install.packages("elasticnet") 
  install.packages("kohonen") 
}

iris_unique <- unique(iris) # Remove duplicates
set.seed(42) # Sets seed for reproducibility
tsne_out <- Rtsne::Rtsne(as.matrix(iris_unique[,1:4])) # Run TSNE
plot(tsne_out$Y,col=iris_unique$Species,asp=1) # Plot the result

zip.test <- data.table::fread("../demos/zip.test.gz")
zip.mat <- as.matrix(zip.test[,-1])
zip.tsne <- Rtsne::Rtsne(zip.mat, verbose=TRUE)
zip.dt <- data.table(digit=zip.test[[1]], zip.tsne[["Y"]])

ggplot()+
  geom_text(aes(
    V1, V2, label=digit),
    data=zip.dt)

princurve::principal_curve

## spam
prefix <- "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase."
spam.list <- list()
for(suffix in c("names", "data")){
  f <- paste0("spambase.", suffix)
  if(!file.exists(f)){
    u <- paste0(prefix, suffix)
    download.file(u, f)
  }
}
spambase.dt <- data.table::fread("spambase.data")
spambase.names <- readLines("spambase.names")[34:90]
setnames(spambase.dt, c(sub(":.*", "", spambase.names), "spam"))

spambase.mat <- scale(spambase.dt[,names(spambase.dt)!="spam",with=FALSE])
spambase.keep <- !duplicated(spambase.mat)
n.components <- 2
features.per.component <- 3
spambase.spca <- elasticnet::spca(
  spambase.mat, n.components, rep(features.per.component, n.components),
  sparse="varnum")

spambase.tsne <- Rtsne::Rtsne(spambase.mat[spambase.keep,], verbose=1)

spambase.tsne.dt <- data.table(
  spambase.dt[spambase.keep, .(spam)],
  spambase.tsne[["Y"]])
ggplot()+
  geom_point(aes(
    V1, V2, color=factor(spam)),
    data=spambase.tsne.dt)

## too slow.
spambase.kpca <- kernlab::kpca(spambase.mat)

##https://bradleyboehmke.github.io/HOML/autoencoders.html h2o

## keras sequential model + dense layers.
model %>%
  layer_dense(units = 6, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 2, activation = "tanh", name = "bottleneck") %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))
# extract the bottleneck layer
intermediate_layer_model <- keras_model(inputs = model$input, outputs = get_layer(model, "bottleneck")$output)
intermediate_output <- predict(intermediate_layer_model, x_train)


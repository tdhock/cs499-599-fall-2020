## 3. for one class first two pca directions, sample grid. (ESL Fig 14.23)
library(animint2)
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
obs.i.vec <- 1:3000
for(obs.i in obs.i.vec){
  X.mat.list[[obs.i]] <- as.numeric(data.list$digits$train$x[obs.i, , ])
}
X.mat <- do.call(rbind, X.mat.list)

digit <- data.list$digits$train$y[obs.i.vec]
## specifying rank arg is faster, use it if you don't need all the
## principal components.
is.one.digit <- digit==3
X.one.digit <- X.mat[is.one.digit,]
y.one.digit <- digit[is.one.digit]
table(y.one.digit)
digits.fit <- prcomp(X.one.digit, rank=2)

tile.dt.list <- list()
n.pixels <- 28
for(digit.i in 1:nrow(X.one.digit)){
  tile.dt.list[[digit.i]] <- data.table(
    digit.i,
    intensity=X.one.digit[digit.i,],
    row=rep(1:n.pixels, n.pixels),
    col=rep(1:n.pixels, each=n.pixels))
}
tile.dt <- do.call(rbind, tile.dt.list)
ggplot()+
  geom_tile(aes(
    col, -row, fill=intensity),
    data=tile.dt)+
  facet_wrap("digit.i")

digits.pc.mat <- digits.fit[["x"]]
digits.pc.dt <- data.table(digits.pc.mat)
digits.pc.dt[, digit.i := 1:.N]
line.dt.list <- list()
expand.grid.args <- list()
for(pc.name in colnames(digits.pc.mat)){
  pc.vec <- digits.pc.dt[[pc.name]]
  n.grid <- 6
  pc.grid <- seq(min(pc.vec), max(pc.vec), l=n.grid)[-c(1,n.grid)]
  expand.grid.args[[pc.name]] <- pc.grid
  line.dt.list[[pc.name]] <- data.table(
    pc.name,
    type="grid",
    pc.grid)
}
grid.point.dt <- data.table(do.call(expand.grid, expand.grid.args))
grid.point.dt[, type := "grid"]
digits.pc.dt[, type := "data"]
ggplot()+
  geom_point(aes(
    PC1, PC2, color=type),
    data=digits.pc.dt)+
  geom_point(aes(
    PC1, PC2, color=type),
    data=grid.point.dt)+
  geom_vline(aes(
    xintercept=pc.grid, color=type),
    data=line.dt.list[["PC1"]])+
  geom_hline(aes(
    yintercept=pc.grid, color=type),
    data=line.dt.list[["PC2"]])

grid.tile.dt.list <- list()
for(grid.i in 1:nrow(grid.point.dt)){
  one.grid.point <- grid.point.dt[grid.i]
  one.grid.mat <- one.grid.point[, matrix(
    c(PC1, PC2), nrow=nrow(digits.pc.mat), ncol=2, byrow=TRUE)]
  dist.vec <- rowSums((one.grid.mat-digits.pc.mat)^2)
  closest.i <- which.min(dist.vec)
  grid.tile.dt.list[[grid.i]] <- data.table(
    one.grid.point,
    grid.i,
    intensity=X.one.digit[grid.i,],
    row=rep(1:n.pixels, n.pixels),
    col=rep(1:n.pixels, each=n.pixels))
}
grid.tile.dt <- do.call(rbind, grid.tile.dt.list)

ggplot()+
  geom_tile(aes(
    col, -row, fill=intensity),
    data=grid.tile.dt)+
  scale_fill_gradient(low="black", high="white")+
  coord_equal()+
  facet_grid(PC2 ~ PC1, labeller=label_both)


animint(
  ggplot()+
  geom_point(aes(
    PC1, PC2),
    clickSelects="digit.i",
    data=digits.pc.dt),
  ggplot()+
  geom_tile(aes(
    col, -row, fill=intensity),
    showSelected="digit.i",
    data=tile.dt)+
  scale_fill_gradient(low="black", high="white")+
  coord_equal()
)

  
  

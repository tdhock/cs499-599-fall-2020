loss.dt.list <- list()
change.dt.list <- list()
## penalized model selection / evaluation with Fpsn (dynamic
## programming).
data(neuroblastoma, package="neuroblastoma")
library(data.table)
all.profiles <- data.table(neuroblastoma$profiles)
all.labels <- data.table(neuroblastoma$annotations)
label.counts <- all.labels[, .(
  positive.labels=sum(annotation=="breakpoint"),
  negative.labels=sum(annotation=="normal")
), by=.(profile.id)]
label.counts[positive.labels==3 & negative.labels==3]
profile.id.show <- "2"
labels.show <- all.labels[profile.id==profile.id.show]
profiles.show <- all.profiles[
  profile.id==profile.id.show & chromosome %in% labels.show$chromosome]
label.i.todo <- 1:nrow(labels.show)
label.i.done <- as.integer(unique(sub(" .*", "", names(loss.dt.list))))
label.i.new <- label.i.todo[! label.i.todo %in% label.i.done]
max.segments <- 10
for(label.i in label.i.new){
  cat(sprintf("label.i=%d\n", label.i))
  one.label <- labels.show[label.i]
  select.dt <- one.label[, .(profile.id, chromosome)]
  pro.dt <- all.profiles[select.dt, on=names(select.dt)]
  ## Code from demos about dynamic programming for changepoint
  ## detection (2020-10-16).
  this.max <- if(nrow(pro.dt) < max.segments){
    nrow(pro.dt)
  }else{
    max.segments
  }
  optimal.models <- jointseg::Fpsn(pro.dt$logratio, this.max)
  segs.dt.list <- list()
  for(n.segs in 1:this.max){
    end <- optimal.models$t.est[n.segs, 1:n.segs]
    start <- c(1, end[-length(end)]+1)
    segs.dt.list[[paste(n.segs)]] <- data.table(start, end)[, .(
      segments=n.segs,
      mean=mean(pro.dt$logratio[start:end]),
      algorithm="DP"           
    ), by=.(start, end)]
  }
  segs.dt <- do.call(rbind, segs.dt.list)
  for(col.name in c("start", "end")){
    col.value <- segs.dt[[col.name]]
    set(segs.dt, j=paste0(col.name, ".pos"),
        value=pro.dt$position[col.value])
  }
  segs.dt[, end.before := c(NA, end.pos[-.N]), by=.(segments) ]
  change.dt <- data.table(select.dt, segs.dt[1 < start])
  change.dt[, changepoint := (start.pos+end.before)/2]
  this.loss.dt <- data.table(
    segments=1:this.max,
    loss=optimal.models$J.est)
  penalty <- 0.12
  this.loss.dt[, crit.value := loss + penalty*segments]
  loss.dt.list[[paste(label.i)]] <- data.table(
    select.dt, this.loss.dt)
  change.dt.list[[paste(label.i)]] <- change.dt[, data.table(
    select.dt, changepoint, segments)]
}  
change.dt <- do.call(rbind, change.dt.list)
loss.dt <- do.call(rbind, loss.dt.list)

## Compute model selection function, which maps penalty (lambda)
## values to model complexity (segments) values.
all.model.selection <- loss.dt[, penaltyLearning::modelSelection(
  .SD, "loss", "segments"),
  by=.(profile.id, chromosome)]
pred.penalty.dt <- loss.dt[, data.table(
  pred.log.lambda=log(10)
), by=.(profile.id, chromosome)]

## Compute label error, fp/fn for each selected model.
error.list <- penaltyLearning::labelError(
  models=all.model.selection,
  labels=labels.show,
  changes=change.dt,
  problem.vars=c("profile.id", "chromosome"),
  change.var="changepoint",
  model.vars="segments")
error.list$model.errors[, .(
  profile.id, chromosome, min.lambda, max.lambda, segments, fp, fn)]

## Compute ROC curve, FPR/TPR for every prediction threshold (default
## prediction threshold is zero).
roc.list <- penaltyLearning::ROChange(
  error.list$model.errors,
  pred.penalty.dt,
  problem.vars=c("profile.id", "chromosome"))
roc.list$roc[, .(min.thresh, max.thresh, FPR, TPR, errors)]

## Visualize ROC curve.
library(animint2)
ggplot()+
  geom_path(aes(
    FPR, TPR),
    data=roc.list$roc)

ggplot()+
  geom_point(aes(
    position, logratio),
    data=profiles.show)+
  geom_rect(aes(
    xmin=min, xmax=max, fill=annotation),
    ymin=-Inf,
    ymax=Inf,
    alpha=0.5,
    data=labels.show)+
  facet_grid(chromosome ~ .)

show.changes.list <- list()
show.roc.list <- list()
thresh.vec <- roc.list$roc[, seq(min(max.thresh), max(min.thresh), l=50) ]
for(thresh.i in seq_along(thresh.vec)){
  thresh <- thresh.vec[[thresh.i]]
  show.roc.list[[paste(thresh.i)]] <- data.table(
    thresh.i,
    roc.list$roc[min.thresh <= thresh & thresh < max.thresh])
  pred.penalty.dt[, new.log.lambda := pred.log.lambda + thresh]
  thresh.selected <- all.model.selection[pred.penalty.dt, on=.(
    profile.id, chromosome,
    min.log.lambda <= new.log.lambda, max.log.lambda > new.log.lambda)]
  thresh.changes <- change.dt[
    thresh.selected, nomatch=0L,
    on=.(profile.id, chromosome, segments)]
  if(nrow(thresh.changes)){
    show.changes.list[[paste(thresh.i)]] <- data.table(
      thresh.i, thresh.changes)
  }
}
show.changes <- do.call(rbind, show.changes.list)
show.roc <- do.call(rbind, show.roc.list)

animint(
  ggplot()+
  geom_path(aes(
    FPR, TPR),
    data=roc.list$roc)+
  geom_point(aes(
    FPR, TPR),
    clickSelects="thresh.i",
    size=5,
    alpha=0.55,
    data=show.roc),
  ggplot()+
  geom_point(aes(
    position, logratio),
    data=profiles.show)+
  geom_tallrect(aes(
    xmin=min, xmax=max, fill=annotation),
    alpha=0.5,
    data=labels.show)+
  geom_vline(aes(
    xintercept=changepoint),
    showSelected="thresh.i",
    data=show.changes)+
  facet_grid(chromosome ~ .)
)  

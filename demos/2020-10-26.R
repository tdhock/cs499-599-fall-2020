## Burns Ch 27 Be Impatient.

## penalized model selection.
data(neuroblastoma, package="neuroblastoma")
library(data.table)
library(ggplot2)

pro.dt <- data.table(neuroblastoma$profiles)[profile.id=="4" & chromosome=="2"]
pro.dt[, index := 1:.N]
ggplot()+
  geom_point(aes(
    position, logratio),
    data=pro.dt)

nstates.segs.list <- list()
loss.dt.list <- list()
set.seed(1)
for(nstates in 1:5){
  hmm <- depmixS4::depmix(logratio ~ 1, pro.dt, nstates)
  hmm.fit <- depmixS4::fit(hmm)
  resp.dt.list <- list()
  for(resp.i in seq_along(hmm.fit@response)){
    param.list <- hmm.fit@response[[resp.i]][[1]]@parameters
    resp.dt.list[[paste(resp.i)]] <- data.table(
      state=resp.i,
      mean=param.list[["coefficients"]],
      sd=param.list[["sd"]])
  }
  resp.dt <- do.call(rbind, resp.dt.list)
  post.dt <- data.table(hmm.fit@posterior)
  state.rle <- rle(post.dt$state)
  segs.dt <- data.table(
    n.data=state.rle[["lengths"]],
    state=state.rle[["values"]])
  segs.dt[, end := cumsum(n.data)]
  segs.dt[, start := c(1, end[-.N]+1)]
  segs.with.params <- resp.dt[segs.dt, on=.(state=state)]
  loss.dt.list[[paste(nstates)]] <- data.table(
    nstates,
    loss=-as.numeric(depmixS4::logLik(hmm.fit)))
  nstates.segs.list[[paste(nstates)]] <- data.table(
    nstates, segs.with.params)
}
nstates.segs <- do.call(rbind, nstates.segs.list)
loss.dt <- do.call(rbind, loss.dt.list)

## Linear penalty for model selection: crit.value = loss + penalty * model size.

## penalty is some non-negative value that controls the selected model
## size. large penalties mean small models are selected, and small
## penalties mean large model are selected.
penalty <- 10
loss.dt[, crit.value := loss + penalty * nstates]
ggplot()+
  geom_line(aes(
    nstates, crit.value),
    data=loss.dt)

ggplot()+
  geom_line(aes(
    nstates, loss),
    data=loss.dt)

nstates.segs.list <- list()
loss.dt.list <- list()

all.profiles <- data.table(neuroblastoma$profiles)
all.labels <- data.table(neuroblastoma$annotations)
label.i.todo <- 1:51
label.i.done <- as.integer(unique(sub(" .*", "", names(loss.dt.list))))
label.i.new <- label.i.todo[! label.i.todo %in% label.i.done]
for(label.i in label.i.new){
  cat(sprintf("label.i=%d\n", label.i))
  one.label <- all.labels[label.i]
  pro.dt <- all.profiles[one.label, on=.(profile.id, chromosome)]
  for(nstates in 1:5){
    set.seed(1)
    hmm <- depmixS4::depmix(logratio ~ 1, pro.dt, nstates)
    hmm.fit <- tryCatch({
      depmixS4::fit(hmm)
    }, error=function(e){
      NULL
    })
    if(!is.null(hmm.fit)){
      resp.dt.list <- list()
      for(resp.i in seq_along(hmm.fit@response)){
        param.list <- hmm.fit@response[[resp.i]][[1]]@parameters
        resp.dt.list[[paste(resp.i)]] <- data.table(
          state=resp.i,
          mean=param.list[["coefficients"]],
          sd=param.list[["sd"]])
      }
      resp.dt <- do.call(rbind, resp.dt.list)
      post.dt <- data.table(hmm.fit@posterior)
      state.rle <- rle(post.dt$state)
      segs.dt <- data.table(
        n.data=state.rle[["lengths"]],
        state=state.rle[["values"]])
      segs.dt[, end := cumsum(n.data)]
      segs.dt[, start := c(1, end[-.N]+1)]
      segs.with.params <- resp.dt[segs.dt, on=.(state=state)]
      loss.dt.list[[paste(label.i, nstates)]] <- data.table(
        one.label,
        nstates,
        loss=-as.numeric(depmixS4::logLik(hmm.fit)))
      nstates.segs.list[[paste(label.i, nstates)]] <- data.table(
        one.label,
        nstates, segs.with.params)
    }
  }
}
nstates.segs <- do.call(rbind, nstates.segs.list)
loss.dt <- do.call(rbind, loss.dt.list)

penalty <- 10
loss.dt[, crit.value := loss + penalty * nstates]
loss.dt[, .(
  selected.states=nstates[which.min(crit.value)]
), by=.(profile.id, chromosome)]

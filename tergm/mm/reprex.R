reprex::reprex({
  library(tergm)
  data(samplk)
  samp.series <- NetSeries(list(samplk1,samplk2,samplk3))

  fit.fd.series <- tergm(
    samp.series ~
      Form(~edges) +
      Diss(~edges),
    estimate = "CMLE")
  
  sep.sim <- simulate(fit.fd.series, nsim = 1, 
                      time.slices = 1000, 
                      monitor = ~edges,
                      stats = T,
                      output = "stats")
  
  colMeans(sep.sim$stats)

})


reprex::reprex({
  library(tergm)
  data(florentine)
  tergm.fit.1 <- tergm(
    flobusiness ~ 
      Cross(~ edges) + 
      Change(~ offset(edges)),
    targets = ~edges,
    offset.coef = -log(9),
    estimate = "EGMME")
})


reprex::reprex({
  library(tergm)
  data(samplk)
  samp.series <- NetSeries(list(samplk1, samplk2))
  sep.fit <- tergm(
    samp.series ~ 
      Form(~ edges) + 
      Diss(~ edges),
    estimate = "CMLE")
  sep.fit$nw.stats # look fine
  sep.sim <- simulate(sep.fit, nsim = 1, 
                      time.slices = 1000, stats=T)
  cbind(obs = sep.fit$nw.stats,
        sim = colMeans(attributes(sep.sim)$stats))
  plot(attributes(sep.sim)$stats) 
  
})

reprex::reprex{(
  library(tergm)
  data(samplk)
  samp.series <- NetSeries(samp.list)
  samp.dyn <- networkDynamic(network.list = samp.list, start=1)
  
  # summary formula operators  correctly displayed
  summary.fp.series <- summary(samp.series ~
                                 Form(~edges) +
                                 Persist(~edges))
  summary.fp.series
  
  summary.fp.nD <- summary(samp.dyn ~
                             Form(~edges) +
                             Persist(~edges),
                           at=0:3)
  summary.fp.nD
  
  #summary formula operators not correctly displayed
  summary.cc.series <- summary(samp.series ~
                                 Cross(~edges) +
                                 Change(~edges))
  summary.cc.series
  
  summary.cc.nD <- summary(samp.dyn ~
                             Cross(~edges) +
                             Change(~edges),
                           at=0:3)
  summary.cc.nD
)}
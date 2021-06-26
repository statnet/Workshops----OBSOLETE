reprex::reprex({
  library(ergm.ego)
  set.seed(1)
  data("faux.mesa.high")
  mesa <- faux.mesa.high
  mesa.ego <- as.egor(mesa)
  summary(mesa.ego ~ degree(0:10), scaleto=100000)
  summary(mesa.ego ~ degree(0:10), scaleto=nrow(mesa.ego)*100)
})

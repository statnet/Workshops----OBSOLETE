## ----setup, include = FALSE----------------------------------------------
library(knitr)
knitr::opts_chunk$set(comment = NA)

## ----eval = FALSE--------------------------------------------------------
## install.packages('statnet')
## install.packages('tsna')
## install.packages('ndtv')
## install.packages('htmlwidgets')

## ----eval = FALSE--------------------------------------------------------
## install.packages('latticeExtra')

## ----results = 'hide', message = FALSE, warning = FALSE------------------
library(statnet)
library(tsna)
library(latticeExtra)
library(ndtv)
library(htmlwidgets)

## ----eval = FALSE--------------------------------------------------------
## sessionInfo()

## ------------------------------------------------------------------------
set.seed(0)

## ------------------------------------------------------------------------
data(samplk)
ls()

## ------------------------------------------------------------------------
samplist <- list(samplk1,samplk2,samplk3)
sampdyn <- networkDynamic(network.list = samplist)

## ------------------------------------------------------------------------
sampdyn # note it says 4 time points
network.collapse(sampdyn, at = 4) # this is an artifact of discrete time

## ---- eval = F-----------------------------------------------------------
## vignette("networkDynamic")

## ------------------------------------------------------------------------
par(mfrow = c(1,3))
plot(network.extract(sampdyn, at = 0), main = "Time 1", displaylabels = T)
plot(network.extract(sampdyn, at = 1), main = "Time2", displaylabels = T)
plot(network.extract(sampdyn, at = 2), main = "Time3", displaylabels = T)

## ------------------------------------------------------------------------
tSnaStats(sampdyn,"degree") # Changes in degree centrality

## ------------------------------------------------------------------------
tErgmStats(sampdyn, "~ edges+triangle") # Notice the increase in triangles

## ------------------------------------------------------------------------
tp <- tPath(sampdyn, 1, direction = 'fwd')
print(tp)
plotPaths(sampdyn, tp)


## ------------------------------------------------------------------------
table(edgeDuration(sampdyn, mode = 'duration', subject = 'spells')) # Note bimodality

## ------------------------------------------------------------------------
library(ndtv)
data(short.stergm.sim)

## ---- eval = FALSE, message = FALSE,results = 'hide',cache = FALSE-------
## render.d3movie(short.stergm.sim,
##                plot.par=list(displaylabels=T))

## ---- message = FALSE,cache = FALSE--------------------------------------
render.d3movie(short.stergm.sim,
               plot.par=list(displaylabels=T),
               output.mode = 'htmlWidget')

## ----message = FALSE,cache = TRUE----------------------------------------
proximity.timeline(short.stergm.sim,default.dist = 6,
          mode = 'sammon',labels.at = 17,vertex.cex = 4)

## ----eval = FALSE--------------------------------------------------------
##         stergm(my.network,                 #do not run this
##             formation =  ~edges+kstar(2),
##             dissolution =  ~edges+triangle,
##             estimate =  `insert method`
##         )

## ----results = "hide", message = FALSE-----------------------------------
samp.fit <- stergm(samplist,
	formation =  ~edges+mutual+cyclicalties+transitiveties,
	dissolution = ~edges+mutual+cyclicalties+transitiveties,
	estimate = "CMLE",
	times = c(1:3)
	)

## ------------------------------------------------------------------------
summary(samp.fit)

## ----results = "hide", message = FALSE-----------------------------------
samp.fit.2 <- stergm(samplist,
  formation =  ~edges+mutual+cyclicalties+transitiveties,
	dissolution = ~edges+mutual+cyclicalties+transitiveties,
	estimate = "CMLE",
  times = 1:2
	)

## ------------------------------------------------------------------------
summary(samp.fit.2)

## ------------------------------------------------------------------------
theta.diss <- log(9)

## ----results = 'hide',message = FALSE,warning = FALSE--------------------
data(florentine)
X11()
stergm.fit.1 <- stergm(flobusiness,
	formation =  ~edges+gwesp(0,fixed = T),
	dissolution = ~offset(edges),
	targets = "formation",
	offset.coef.diss = theta.diss,
	estimate = "EGMME",
	control = control.stergm(SA.plot.progress = TRUE)
	)
dev.off()

## ----eval = FALSE--------------------------------------------------------
## mcmc.diagnostics(stergm.fit.1)

## ------------------------------------------------------------------------
stergm.fit.1
names(stergm.fit.1)
summary(stergm.fit.1)

## ------------------------------------------------------------------------
stergm.sim.1 <- simulate.stergm(stergm.fit.1, nsim = 1, 
    time.slices = 1000)

## ---- message = FALSE, cache = FALSE-------------------------------------
wealthsize <- log(get.vertex.attribute(flobusiness, "wealth")) * 2/3
slice.par = list(start = 0, 
               end = 25, 
               interval = 1, 
               aggregate.dur = 1, 
               rule = "any")
compute.animation(stergm.sim.1, slice.par = slice.par)
render.par = list(tween.frames = 5,
                show.time = T,
                show.stats = "~edges+gwesp(0,fixed = T)")
plot.par = list(edge.col = "darkgray",
              displaylabels = T,
              label.cex = .8,
              label.col = "blue",
              vertex.cex = wealthsize)
render.d3movie(stergm.sim.1,
               render.par = render.par,
               plot.par = plot.par,
               output.mode = 'htmlWidget')

## ------------------------------------------------------------------------
summary(flobusiness~edges+gwesp(0,fixed = T))
colMeans(attributes(stergm.sim.1)$stats)

## ------------------------------------------------------------------------
plot(attributes(stergm.sim.1)$stats)

## ------------------------------------------------------------------------
plot(as.matrix(attributes(stergm.sim.1)$stats))

## ------------------------------------------------------------------------
stergm.sim.1.df <- as.data.frame(stergm.sim.1)
names(stergm.sim.1.df)
stergm.sim.1.df[1,]
mean(stergm.sim.1.df$duration)
mean(edgeDuration(stergm.sim.1, 
                  mode = 'duration', 
                  subject = 'spells'))

## ------------------------------------------------------------------------
theta.diss.100 <- log(99)

## ---- results = "hide", message = FALSE----------------------------------
ergm.fit1 <- ergm(flobusiness ~ edges + gwesp(0, fixed = T))

## ------------------------------------------------------------------------
summary(ergm.fit1)
theta.form <- ergm.fit1$coef 
theta.form

## ------------------------------------------------------------------------
theta.form[1] <- theta.form[1] - theta.diss.100

## ------------------------------------------------------------------------
stergm.sim.2 <- simulate(flobusiness, 
                         formation = ~edges+gwesp(0,fixed = T),
                         dissolution = ~edges, 
                         monitor = "all",
                         coef.form = theta.form, 
                         coef.diss = theta.diss.100,
                         time.slices = 50000)

## ------------------------------------------------------------------------
summary(flobusiness~edges+gwesp(0,fixed = T))
colMeans(attributes(stergm.sim.2)$stats)
stergm.sim.dm.2 <- as.data.frame(stergm.sim.2)
mean(stergm.sim.dm.2$duration)
plot(attributes(stergm.sim.2)$stats)


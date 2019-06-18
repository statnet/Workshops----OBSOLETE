## ----setup, include = FALSE----------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache=F, comment=NA, fig.align='center')


## ----dev, child = '../statnetDevTeam.Rmd'--------------------------------




## ----project, child = '../statnetProject.Rmd'----------------------------




## ----eval = FALSE--------------------------------------------------------
install.packages('statnet')
install.packages('ndtv')
install.packages('htmlwidgets')
install.packages('latticeExtra')


## ----results = 'hide', message = FALSE, warning = FALSE------------------
library(statnet)
library(ndtv)
library(htmlwidgets)
library(latticeExtra)


## ---- cache = FALSE------------------------------------------------------
sessionInfo()


## ------------------------------------------------------------------------
set.seed(0)


## ------------------------------------------------------------------------
data(samplk)
ls()


## ------------------------------------------------------------------------
samplist <- list(samplk1,samplk2,samplk3)
sampdyn <- networkDynamic(network.list = samplist)


## ------------------------------------------------------------------------
sampdyn


## ---- eval = F-----------------------------------------------------------
vignette("networkDynamic")


## ------------------------------------------------------------------------
par(mfrow = c(2,2), oma=c(1,1,1,1), mar=c(4,1,1,1))
plot(network.extract(sampdyn, at = 0), main = "Time 1", 
     displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(network.extract(sampdyn, at = 1), main = "Time2", 
     displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(network.extract(sampdyn, at = 2), main = "Time3", 
     displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(sampdyn, main = "Collapsed", 
     displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)


## ------------------------------------------------------------------------
tSnaStats(sampdyn,"degree") # Changes in degree centrality


## ------------------------------------------------------------------------
tErgmStats(sampdyn, "~ edges+triangle") # Notice the increase in triangles


## ------------------------------------------------------------------------
# who is v1?
get.vertex.attribute(sampdyn, "vertex.names")[1]

# who is in v1's FRS?
tp <- tPath(sampdyn, v=1, direction = 'fwd')
print(tp)
par(mfrow=c(1,2))
coords <- plot(tp, main="Forward Reachable Set from v1", cex.main=.8)
plotPaths(sampdyn, tp, 
          coord = coords,
          main = "Overlaid on collapsed Network",
          label.cex=.8, cex.main=.8)



## ------------------------------------------------------------------------
table(edgeDuration(sampdyn, mode = 'duration', subject = 'spells')) # Note bimodality


## ------------------------------------------------------------------------
library(ndtv)
data(short.stergm.sim)


## ---- eval = FALSE, message = FALSE,results = 'hide',cache = FALSE-------
render.d3movie(short.stergm.sim, 
               plot.par=list(displaylabels=T))


## ---- message = FALSE,cache = FALSE--------------------------------------
render.d3movie(short.stergm.sim,
               plot.par=list(displaylabels=T),
               output.mode = 'htmlWidget') # using htmlwidgets package here


## ----message = FALSE,cache = TRUE----------------------------------------
proximity.timeline(short.stergm.sim,default.dist = 6,
          mode = 'sammon',labels.at = 17,vertex.cex = 4)


## ----eval = FALSE--------------------------------------------------------
        ergm(my.network ~ edges + gwesp(0, fixed=T))     #do not run this
           


## ----eval = FALSE--------------------------------------------------------
        stergm(my.network,                               #do not run this
            formation =  ~ edges + nodefactor('age'),
            dissolution =  ~ edges + gwesp(0, fixed=T),
            estimate =  `insert method`
        )


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
mcmc.diagnostics(stergm.fit.1)


## ------------------------------------------------------------------------
stergm.fit.1
names(stergm.fit.1)
summary(stergm.fit.1)


## ------------------------------------------------------------------------
stergm.sim.1 <- simulate(stergm.fit.1, nsim = 1, 
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
cbind(model = summary(flobusiness~edges+gwesp(0,fixed = T)),
      obs = colMeans(attributes(stergm.sim.1)$stats))


## ------------------------------------------------------------------------
plot(attributes(stergm.sim.1)$stats)


## ------------------------------------------------------------------------
plot(as.matrix(attributes(stergm.sim.1)$stats))


## ------------------------------------------------------------------------
# create dataFrame for direct estimation
stergm.sim.1.df <- as.data.frame(stergm.sim.1)
names(stergm.sim.1.df)
stergm.sim.1.df[1,]

cbind(directEst = mean(stergm.sim.1.df$duration),
      ndEst = mean(edgeDuration(stergm.sim.1, 
                  mode = 'duration', 
                  subject = 'spells')))


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
# first, recovery of the cross sectional statistics in the formation model
cbind(observed = summary(flobusiness~edges+gwesp(0,fixed = T)),
      simulated = colMeans(attributes(stergm.sim.2)$stats))

plot(attributes(stergm.sim.2)$stats)

# second, recovery of the tie duration
stergm.sim.dm.2 <- as.data.frame(stergm.sim.2)
mean(stergm.sim.dm.2$duration)


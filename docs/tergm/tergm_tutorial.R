
## ----install, eval = FALSE---------------------------------------------------------------
## # statnet packages
install.packages('tergm')
install.packages('tsna')
install.packages('ndtv')
#
# # other packages to enhance graphical output
install.packages('htmlwidgets')
install.packages('latticeExtra')


## ----library, message = FALSE, warning = FALSE-------------------------------------------
library(tergm)
library(tsna)
library(ndtv)
library(htmlwidgets)
library(latticeExtra)


## ----info, cache = FALSE-----------------------------------------------------------------
sessionInfo()


## ----seed--------------------------------------------------------------------------------
set.seed(1)


## ----samp--------------------------------------------------------------------------------
data(samplk)
ls()


## ----makeobject--------------------------------------------------------------------------
samp.list <- list(samplk1,samplk2,samplk3)
samp.dyn <- networkDynamic(network.list = samp.list)


## ----------------------------------------------------------------------------------------
samp.dyn


## ---- message=F, warning=F---------------------------------------------------------------
nw3 <- network.extract(samp.dyn, at = 3) 
nw3 # empty
nw1 <- network.extract(samp.dyn, at = 0) 
nw1 # the first network, as expected


## ----ndvignette, eval = F----------------------------------------------------------------
## vignette("networkDynamic")


## ----------------------------------------------------------------------------------------
par(mfrow = c(2,2), oma=c(1,1,1,1), mar=c(4,1,1,1))
plot(network.extract(samp.dyn, at = 0), main = "Time 1", 
     displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(network.extract(samp.dyn, at = 1), main = "Time2", 
     displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(network.extract(samp.dyn, at = 2), main = "Time3", 
     displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(samp.dyn, main = "Collapsed", 
     displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)


## ----tsnastats---------------------------------------------------------------------------
tSnaStats(samp.dyn,"degree") # Changes in degree centrality


## ----tergmstats--------------------------------------------------------------------------
tErgmStats(samp.dyn, "~ edges+triangle") # Notice the increase in triangles


## ----frs---------------------------------------------------------------------------------
# who is v1?
get.vertex.attribute(samp.dyn, "vertex.names")[1]

# who is in v1's FRS?
tp <- tPath(samp.dyn, v=1, direction = 'fwd')
print(tp)
par(mfrow=c(1,2))
coords <- plot(tp, main="Forward Reachable Set from v1", cex.main=.8)
plotPaths(samp.dyn, tp, 
          coord = coords,
          main = "Overlaid on collapsed Network",
          label.cex=.8, cex.main=.8)



## ----edgedur-----------------------------------------------------------------------------
table(edgeDuration(samp.dyn, mode = 'duration', subject = 'spells')) 
# Note bimodality


## ----helptsna----------------------------------------------------------------------------
help(package="tsna")


## ----------------------------------------------------------------------------------------
data(short.stergm.sim)


## ----movie1, eval = FALSE----------------------------------------------------------------
## render.d3movie(short.stergm.sim,
##                plot.par=list(displaylabels=T))


## ----movie2, message = FALSE-------------------------------------------------------------
render.d3movie(short.stergm.sim,
               plot.par=list(displaylabels=T),
               output.mode = 'htmlWidget') # using htmlwidgets package here


## ----proxtl, message = FALSE,cache = TRUE------------------------------------------------
proximity.timeline(short.stergm.sim,default.dist = 6,
          mode = 'sammon',labels.at = 17,vertex.cex = 4)


## ----netseries---------------------------------------------------------------------------
samp.series <- NetSeries(list(samplk1,samplk2,samplk3))


## ---- eval=F-----------------------------------------------------------------------------
## help("ergm-terms", package="tergm")


## ---- message=F, warning=F---------------------------------------------------------------

samp.fit.fp <- tergm(samp.list ~
    Form(~edges+mutual+cyclicalties+transitiveties) +
    Persist(~edges+mutual+cyclicalties+transitiveties),
  estimate = "CMLE",
  times = c(1:3)
	)

summary(samp.fit.fp)


## ----sampfit2, eval=F, message = FALSE---------------------------------------------------
## samp.fit.2 <- tergm(
##   samp.list ~
##     Form(~edges+mutual+cyclicalties+transitiveties) +
##     Diss(~edges+mutual+cyclicalties+transitiveties),
##   estimate = "CMLE",
##   times = c(1:2)
## 	)


## ----------------------------------------------------------------------------------------

samp.fit.cc <- tergm(samp.list ~
    Cross(~edges+mutual+cyclicalties+transitiveties) +
    Change(~edges+mutual+cyclicalties+transitiveties),
  estimate = "CMLE",
  times = c(1:3)
	)

summary(samp.fit.cc)


## ----------------------------------------------------------------------------------------
theta.persist <- log(9)


## ----tergm1, results = 'hide', message = FALSE, warning = FALSE--------------------------
data(florentine)
X11()

set.seed(1)
egmme.fit <- tergm(
  flobusiness ~ 
    Form(~ edges + gwesp(0, fixed=T)) + 
    Persist(~ offset(edges)),
  targets = ~ edges + gwesp(0, fixed=T),
  offset.coef = theta.persist,
  estimate = "EGMME",
  control = control.tergm(SA.plot.progress=TRUE)
  )

dev.off()


## ----mcmcdiag1, eval = FALSE-------------------------------------------------------------
## mcmc.diagnostics(egmme.fit, which="plots") # only returns the plots


## ----exploretergm1-----------------------------------------------------------------------
egmme.fit
names(egmme.fit)
summary(egmme.fit)


## ----tsim1-------------------------------------------------------------------------------
egmme.sim <- simulate(egmme.fit, nsim = 1, 
                        time.slices = 1000)


## ----tsim1viz, message = FALSE-----------------------------------------------------------
wealthsize <- log(get.vertex.attribute(flobusiness, "wealth")) * 2/3

# Set the ndtv animation parameter
slice.par = list(start = 0, 
               end = 25, 
               interval = 1, 
               aggregate.dur = 1, 
               rule = "any")
# Animate
compute.animation(egmme.sim, slice.par = slice.par)

# Set some ndtv rendering parameters
render.par = list(tween.frames = 5,
                show.time = T,
                show.stats = "~edges+gwesp(0,fixed = T)")
plot.par = list(edge.col = "darkgray",
              displaylabels = T,
              label.cex = .8,
              label.col = "blue",
              vertex.cex = wealthsize)

# Show the movie!
render.d3movie(egmme.sim,
               render.par = render.par,
               plot.par = plot.par,
               output.mode = 'htmlWidget')


## ----tsim1stats--------------------------------------------------------------------------
cbind(model = summary(flobusiness ~ edges + gwesp(0, fixed = T)),
      obs = colMeans(attributes(egmme.sim)$stats))


## ----------------------------------------------------------------------------------------
plot(attributes(egmme.sim)$stats)


## ----------------------------------------------------------------------------------------
plot(as.matrix(attributes(egmme.sim)$stats))


## ----tsim1dur----------------------------------------------------------------------------
# create dataFrame for direct estimation
egmme.sim.df <- as.data.frame(egmme.sim)
names(egmme.sim.df)
egmme.sim.df[1,]

cbind(directEst = mean(egmme.sim.df$duration),
      ndEst = mean(edgeDuration(egmme.sim, 
                                mode = 'duration', 
                                subject = 'spells')))


## ----------------------------------------------------------------------------------------
theta.persist.100 <- log(99)
theta.diss.100 <- -theta.persist.100


## ----ergmfit, results = "hide", message = FALSE------------------------------------------
ergm.fit <- ergm(flobusiness ~ edges + gwesp(0, fixed = T))


## ----ergmsummary-------------------------------------------------------------------------
summary(ergm.fit)
theta.form <- coef(ergm.fit)
theta.form


## ----------------------------------------------------------------------------------------
theta.form[1] <- theta.form[1] - log(100)
theta.form


## ----tsim2-------------------------------------------------------------------------------

## Form + Persist
tergm.sim.fp <- simulate(
  flobusiness ~
    Form(~ edges + gwesp(0,fixed=T)) +
    Persist(~ edges),
  monitor = "all",
  coef = c(theta.form, theta.persist.100),
  time.slices = 10000,
  dynamic = TRUE)

## Form + Diss
tergm.sim.fd <- simulate(
  flobusiness ~
    Form(~ edges + gwesp(0,fixed=T)) +
    Diss(~ edges),
  monitor = "all",
  coef = c(theta.form, theta.diss.100),
  time.slices = 10000,
  dynamic = TRUE)



## ----tsim2stats--------------------------------------------------------------------------
## Compare
cbind(Observed = summary(flobusiness ~ edges + gwesp(0,fixed = T)),
      Persist = colMeans(attributes(tergm.sim.fp)$stats)[1:2],
      Diss = colMeans(attributes(tergm.sim.fd)$stats)[1:2]
)



## ---- eval=F, include=F------------------------------------------------------------------
## # Plots the edges graphs twice
## plot(attributes(tergm.sim.fp)$stats)
## plot(attributes(tergm.sim.fd)$stats)


## ---- tiedur-----------------------------------------------------------------------------
cbind(Observed = 100,
      Persist = mean(edgeDuration(tergm.sim.fd, 
                                  mode="duration", 
                                  subject="spells")),
      Diss = mean(edgeDuration(tergm.sim.fp, 
                               mode="duration", 
                               subject="spells"))
)



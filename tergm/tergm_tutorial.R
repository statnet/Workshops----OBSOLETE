## ----setup, include=FALSE------------------------------------------------
library(knitr)
knitr::opts_chunk$set(comment=NA)

## ----eval=FALSE----------------------------------------------------------
install.packages('statnet')
library(statnet)

## ----eval=FALSE----------------------------------------------------------
install.packages('tergm') # will install the network package
install.packages('sna')

## ----eval=FALSE----------------------------------------------------------
update.packages('name.of.package')

## ----eval=FALSE----------------------------------------------------------
install.packages('latticeExtra')

## ----results='hide', message=FALSE---------------------------------------
library(statnet)

## ----results='hide', message=FALSE---------------------------------------
library(tergm)
library(sna)
library(coda)

## ----eval=FALSE----------------------------------------------------------
# latest versions:  tergm 3.4.0, ergm 3.7.1, network 1.13.0, networkDynamic 0.9.0 (as of 7/24/2017)
sessionInfo()

## ------------------------------------------------------------------------
set.seed(0)

## ----results='hide'------------------------------------------------------
data("florentine")
ls()

## ------------------------------------------------------------------------
plot(flobusiness)

## ------------------------------------------------------------------------
summary(flobusiness~edges+gwesp(0, fixed=T))

## ------------------------------------------------------------------------
fit1 <- ergm(flobusiness~edges+gwesp(0,fixed=T))
summary(fit1)

## ------------------------------------------------------------------------
sim1 <- simulate(fit1,nsim=1,
          control=control.simulate.ergm(MCMC.burnin=1000))
plot(sim1)

## ----eval=FALSE----------------------------------------------------------
        stergm(my.network,
            formation= ~edges+kstar(2),
            dissolution= ~edges+triangle
        )

## ------------------------------------------------------------------------
data(samplk)
ls()

## ------------------------------------------------------------------------
samp <- list()
samp[[1]] <- samplk1
samp[[2]] <- samplk2
samp[[3]] <- samplk3

## ------------------------------------------------------------------------
plot(samplk1)

## ----results='hide'------------------------------------------------------
samp.fit <- stergm(samp,
	formation= ~edges+mutual+cyclicalties+transitiveties,
	dissolution = ~edges+mutual+cyclicalties+transitiveties,
	estimate = "CMLE",
  times=1:3
	)

## ------------------------------------------------------------------------
summary(samp.fit)

## ----results='hide'------------------------------------------------------
samp.fit.2 <- stergm(samp,
  formation= ~edges+mutual+cyclicalties+transitiveties,
	dissolution = ~edges+mutual+cyclicalties+transitiveties,
	estimate = "CMLE",
  times=1:2
	)

## ------------------------------------------------------------------------
theta.diss <- log(9)

## ----results='hide',message=FALSE,warning=FALSE--------------------------
X11()
stergm.fit.1 <- stergm(flobusiness,
	formation= ~edges+gwesp(0,fixed=T),
	dissolution = ~offset(edges),
	targets="formation",
	offset.coef.diss = theta.diss,
	estimate = "EGMME",
	control=control.stergm(SA.plot.progress=TRUE)
	)
dev.off()

## ----eval=FALSE----------------------------------------------------------
mcmc.diagnostics(stergm.fit.1)

## ------------------------------------------------------------------------
stergm.fit.1
names(stergm.fit.1)
stergm.fit.1$formation
stergm.fit.1$formation.fit
summary(stergm.fit.1)

## ------------------------------------------------------------------------
stergm.sim.1 <- simulate.stergm(stergm.fit.1, nsim=1, 
    time.slices = 1000)

## ------------------------------------------------------------------------
stergm.sim.1

## ------------------------------------------------------------------------
net500 <- network.collapse(stergm.sim.1,at=500)
net500

## ------------------------------------------------------------------------
plot(net500)

## ------------------------------------------------------------------------
summary(flobusiness~edges+gwesp(0,fixed=T))
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

## ---- eval=FALSE, show=FALSE---------------------------------------------
install.packages('ndtv')
library(ndtv)
stergm.sim.1a <- simulate.stergm(stergm.fit.1, nsim=1, 
    time.slices = 100)
slice.par=list(start = 0, end = 25, interval=1, aggregate.dur=1, rule="any")
compute.animation(stergm.sim.1a, slice.par = slice.par)
render.par=list(tween.frames=5,show.time=T,
    show.stats="~edges+gwesp(0,fixed=T)")
wealthsize <- log(get.vertex.attribute(flobusiness, "wealth")) * 2/3
render.animation(stergm.sim.1a,render.par=render.par,
    edge.col="darkgray",displaylabels=T,
    label.cex=.8,label.col="blue",
    vertex.cex=wealthsize)
x11()
ani.replay()

## ------------------------------------------------------------------------
theta.diss.100 <- log(99)

## ------------------------------------------------------------------------
summary(fit1)
theta.form <- fit1$coef 
theta.form


## ------------------------------------------------------------------------
theta.form[1] <- theta.form[1] - theta.diss.100

## ------------------------------------------------------------------------
stergm.sim.2 <- simulate(flobusiness, formation=~edges+gwesp(0,fixed=T),
	dissolution=~edges, monitor="all",
	coef.form=theta.form, coef.diss=theta.diss.100,
	time.slices=50000)

## ------------------------------------------------------------------------
summary(flobusiness~edges+gwesp(0,fixed=T))
colMeans(attributes(stergm.sim.2)$stats)
stergm.sim.dm.2 <- as.data.frame(stergm.sim.2)
mean(stergm.sim.dm.2$duration)
plot(attributes(stergm.sim.2)$stats)

## ------------------------------------------------------------------------
msm.net <- network.initialize(500, directed=F)	
msm.net %v% 'race' <- c(rep(0,250),rep(1,250))
msm.net

## ------------------------------------------------------------------------
msm.form.formula <- ~edges+nodematch('race')+degree(0)+ concurrent
msm.target.stats <- c(225,187,180,90)

## ------------------------------------------------------------------------
msm.diss.formula <- ~offset(edges)+offset(nodematch("race"))

## ------------------------------------------------------------------------
msm.theta.diss <- c(2.944, -0.747) 

## ----results='hide',warning=FALSE----------------------------------------
X11()
msm.fit <- stergm(msm.net,
	formation= msm.form.formula,
	dissolution= msm.diss.formula,
	targets="formation",
	target.stats= msm.target.stats,
	offset.coef.diss = msm.theta.diss,
	estimate = "EGMME",
	control=control.stergm(SA.plot.progress=TRUE,
	    SA.init.gain=0.005)
)
dev.off()

## ------------------------------------------------------------------------
mcmc.diagnostics(msm.fit)

## ------------------------------------------------------------------------
summary(msm.fit)

## ---- warning=FALSE, message=FALSE---------------------------------------
msm.sim <- simulate(msm.fit,time.slices=1000)

## ------------------------------------------------------------------------
colMeans(attributes(msm.sim)$stats)
msm.target.stats

## ------------------------------------------------------------------------
race <- msm.net %v% 'race'
msm.sim.dm <- as.data.frame(msm.sim)
names(msm.sim.dm)
mean(msm.sim.dm$duration[race[msm.sim.dm$tail] ==  race[msm.sim.dm$head]])
mean(msm.sim.dm$duration[race[msm.sim.dm$tail] !=  race[msm.sim.dm$head]])

## ---- eval=FALSE---------------------------------------------------------
slice.par=list(start = 0, end = 100, interval=1, aggregate.dur=1, rule="any")
compute.animation(msm.sim, slice.par = slice.par)

render.par=list(tween.frames=5,show.time=T)
render.animation(msm.sim,render.par=render.par,
    edge.col="darkgray", displaylabels=F, vertex.col="race")
x11()
ani.replay()


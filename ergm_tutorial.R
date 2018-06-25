## ----setup, include=FALSE------------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache=T, comment=NA)

## ----eval=FALSE----------------------------------------------------------
## install.packages(c('statnet', 'sna'))
## library(statnet)

## ----eval=FALSE----------------------------------------------------------
## install.packages('latticeExtra')
## require(latticeExtra)

## ------------------------------------------------------------------------
sessionInfo()

## ------------------------------------------------------------------------
set.seed(0)

## ------------------------------------------------------------------------
data(package='ergm') # tells us the datasets in our packages

## ------------------------------------------------------------------------
data(florentine) # loads flomarriage and flobusiness data
flomarriage # Look at the flomarriage network properties (uses `network`)
par(mfrow=c(1,2)) # Setup a 2 panel plot
plot(flomarriage, main="Florentine Marriage", 
     cex.main=0.8, 
     label = network.vertex.names(flomarriage)) # Plot the network
wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth
plot(flomarriage, vertex.cex=wealth/25, main="Florentine marriage by wealth", cex.main=0.8) # Plot the network with vertex size proportional to wealth

## ------------------------------------------------------------------------
summary(flomarriage ~ edges) # Look at the $g(y)$ statistic for this model
flomodel.01 <- ergm(flomarriage ~ edges) # Estimate the model 
summary(flomodel.01) # Look at the fitted model object

## ---- message = FALSE----------------------------------------------------
summary(flomarriage~edges+triangle) # Look at the g(y) stats for this model
flomodel.02 <- ergm(flomarriage~edges+triangle) 
summary(flomodel.02)

## ------------------------------------------------------------------------
class(flomodel.02) # this has the class ergm

names(flomodel.02) # the ERGM object contains lots of components.

## ------------------------------------------------------------------------
flomodel.02$coef # you can extract/inspect individual components

## ------------------------------------------------------------------------
summary(wealth) # summarize the distribution of wealth
plot(flomarriage, vertex.cex=wealth/25, main="Florentine marriage by wealth", cex.main=0.8) # network plot with vertex size proportional to wealth
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model
flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))
summary(flomodel.03)

## ------------------------------------------------------------------------
data(faux.mesa.high) 
mesa <- faux.mesa.high

## ------------------------------------------------------------------------
mesa
par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa, vertex.col='Grade')
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

## ------------------------------------------------------------------------
fauxmodel.01 <- ergm(mesa ~edges + nodematch('Grade',diff=T) + nodematch('Race',diff=T))
summary(fauxmodel.01)

## ------------------------------------------------------------------------
table(mesa %v% 'Race') # Frequencies of race
mixingmatrix(mesa, "Race")

## ------------------------------------------------------------------------
summary(mesa ~edges + nodematch('Grade',diff=T) + nodematch('Race',diff=T))

## ------------------------------------------------------------------------
data(samplk) 
ls() # directed data: Sampson's Monks
samplk3
plot(samplk3)
summary(samplk3~edges+mutual)

## ---- message = F--------------------------------------------------------
sampmodel.01 <- ergm(samplk3~edges+mutual)
summary(sampmodel.01)

## ------------------------------------------------------------------------
missnet <- network.initialize(10,directed=F)
missnet[1,2] <- missnet[2,7] <- missnet[3,6] <- 1
missnet[4,6] <- missnet[4,9] <- missnet[5,6] <- NA
summary(missnet)

# plot missnet with missing edge colored red. 
tempnet <- missnet
tempnet[4,6] <- tempnet[4,9] <- tempnet[5,6] <- 1
missnetmat <- as.matrix(missnet)
missnetmat[is.na(missnetmat)] <- 2
plot(tempnet,label = network.vertex.names(tempnet),edge.col = missnetmat)

summary(missnet~edges)
summary(ergm(missnet~edges))

## ------------------------------------------------------------------------
missnet_bad <- missnet
missnet_bad[4,6] <- missnet_bad[4,9] <- missnet_bad[5,6] <- 0
summary(missnet_bad)
summary(ergm(missnet_bad~edges))

## ----eval=FALSE----------------------------------------------------------
## help('ergm-terms')

## ------------------------------------------------------------------------
flomodel.03.sim <- simulate(flomodel.03,nsim=10)
class(flomodel.03.sim) 
summary(flomodel.03.sim)
length(flomodel.03.sim)
flomodel.03.sim[[1]]
plot(flomodel.03.sim[[1]], label= flomodel.03.sim[[1]] %v% "vertex.names")

## ------------------------------------------------------------------------
flomodel.03.gof <- gof(flomodel.03~degree + esp + distance)
flomodel.03.gof
plot(flomodel.03.gof)

## ------------------------------------------------------------------------
mesamodel.02 <- ergm(mesa~edges)
mesamodel.02.gof <- gof(mesamodel.02~degree + esp + distance, 
                        control.gof.formula(nsim=10))
plot(mesamodel.02.gof)


## ---- message = F--------------------------------------------------------
summary(flobusiness~edges+degree(1))
fit <- ergm(flobusiness~edges+degree(1))
mcmc.diagnostics(fit)

## ---- eval=FALSE---------------------------------------------------------
## fit <- ergm(flobusiness~edges+degree(1),
##             control=control.ergm(MCMC.interval=1))

## ------------------------------------------------------------------------
data('faux.magnolia.high')
magnolia <- faux.magnolia.high
plot(magnolia, vertex.cex=.5)
summary(magnolia~edges+triangle)

## ---- eval=F-------------------------------------------------------------
## fit <- ergm(magnolia~edges+triangle)

## ---- eval=T, message=F, warning = F-------------------------------------
fit <- ergm(magnolia~edges+triangle, 
            control=control.ergm(MCMLE.maxit=2))


## ---- eval=T, results='hide', fig.show='asis'----------------------------
mcmc.diagnostics(fit)

## ---- message = F, warning = F-------------------------------------------
fit <- ergm(magnolia~edges+gwesp(0.25,fixed=T),
            verbose=T)
mcmc.diagnostics(fit)

## ------------------------------------------------------------------------
fit <- ergm(magnolia~edges+gwesp(0.25,fixed=T)+nodematch('Grade')+
              nodematch('Race')+nodematch('Sex'),
            control = control.ergm(MCMC.samplesize=50000,
                                   MCMC.interval=1000),
            verbose=T)

## ------------------------------------------------------------------------
mcmc.diagnostics(fit)


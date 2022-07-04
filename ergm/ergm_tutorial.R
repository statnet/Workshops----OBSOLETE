## ----setup, include=FALSE------------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache=T, comment=NA)


## ----dev, child = 'common/statnet-dev-team.md'---------------------------------------




## ----project, child = 'common/statnet-project.md'------------------------------------




## ----install-ergm,eval=FALSE---------------------------------------------------------
install.packages('ergm')


## ----loadPackage---------------------------------------------------------------------
library(ergm)


## ----version-------------------------------------------------------------------------
packageVersion("ergm")


## ----homophily-terms-search----------------------------------------------------------
search.ergmTerms(keyword='curved')


## ----pajek---------------------------------------------------------------------------
?read.paj
?read.paj.simplify
?loading.attributes


## ----network-help--------------------------------------------------------------------
?network


## ----ergm-datasets-------------------------------------------------------------------
data(package='ergm') # tells us the datasets in our packages


## ----florentine-plots----------------------------------------------------------------
set.seed(123) # The plot.network function uses random values
data(florentine) # loads flomarriage and flobusiness data
flomarriage # Equivalent to print.network(flomarriage): Examine properties
par(mfrow=c(1,2)) # Set up a 2-column (and 1-row) plot area
plot(flomarriage, 
     main="Florentine Marriage", 
     cex.main=0.8, 
     label = network.vertex.names(flomarriage)) # Equivalent to plot.network(...)
wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth
plot(flomarriage, 
     vertex.cex=wealth/25, # Make vertex size proportional to wealth attribute
     main="Florentine marriage by wealth", cex.main=0.8) 


## ----flomarriage-edges---------------------------------------------------------------
summary(flomarriage ~ edges) # Calculate the edges statistic for this network
flomodel.01 <- ergm(flomarriage ~ edges) # Estimate the model 
summary(flomodel.01) # Look at the fitted model object


## ----flomarriage-triangle, message = FALSE-------------------------------------------
set.seed(321)
summary(flomarriage~edges+triangle) # Look at the g(y) statistics for this model
flomodel.02 <- ergm(flomarriage~edges+triangle) # Estimate the theta coefficients
summary(flomodel.02)


## ----ilogit--------------------------------------------------------------------------
plogis(coef(flomodel.02)[[1]] + (0:2) * coef(flomodel.02)[[2]])


## ----flomarriage-ergm-object---------------------------------------------------------
class(flomodel.02) # this has the class ergm

names(flomodel.02) # the ERGM object contains lots of components.


## ----ergm-object-coef----------------------------------------------------------------
coef(flomodel.02) # you can extract/inspect individual components


## ----flomarriage-wealth--------------------------------------------------------------
summary(wealth) # summarize the distribution of wealth
# plot(flomarriage, 
#      vertex.cex=wealth/25, 
#      main="Florentine marriage by wealth", 
#      cex.main=0.8) # network plot with vertex size proportional to wealth
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model
flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))
summary(flomodel.03)


## ----faux-mesa-high------------------------------------------------------------------
data(faux.mesa.high) 
mesa <- faux.mesa.high


## ----plot-mesa-----------------------------------------------------------------------
set.seed(1)
mesa
par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa, vertex.col='Grade')
legend('bottomleft',fill=7:12,
       legend=paste('Grade',7:12),cex=0.75)


## ----mesa-ergm-----------------------------------------------------------------------
fauxmodel.01 <- ergm(mesa ~edges + 
                       nodefactor('Grade') + nodematch('Grade',diff=T) +
                       nodefactor('Race') + nodematch('Race',diff=T))
summary(fauxmodel.01)


## ----mesa-mixingmatrix---------------------------------------------------------------
table(mesa %v% 'Race') # Frequencies of race
mixingmatrix(mesa, "Race")


## ----mesa-summary--------------------------------------------------------------------
summary(mesa ~edges  + 
          nodefactor('Grade') + nodematch('Grade',diff=T) +
          nodefactor('Race') + nodematch('Race',diff=T))


## ----samplk--------------------------------------------------------------------------
set.seed(2)
data(samplk) # directed data: Sampson's Monks
ls() 
samplk3
plot(samplk3)
summary(samplk3~edges+mutual)


## ----samplk3-ergm, message = F-------------------------------------------------------
set.seed(3)
sampmodel.01 <- ergm(samplk3~edges+mutual)
summary(sampmodel.01)


## ----missing-data--------------------------------------------------------------------
set.seed(4)
missnet <- network.initialize(10,directed=F) # initialize an empty net with 10 nodes
missnet[1,2] <- missnet[2,7] <- missnet[3,6] <- 1 # add a few ties
missnet[4,6] <- missnet[4,9] <- missnet[5,6] <- NA # mark a few dyads missing
summary(missnet)

# plot missnet with missing dyads colored red. 
tempnet <- missnet
tempnet[4,6] <- tempnet[4,9] <- tempnet[5,6] <- 1
missnetmat <- as.matrix(missnet)
missnetmat[is.na(missnetmat)] <- 2
plot(tempnet,label = network.vertex.names(tempnet),
     edge.col = missnetmat)

# fit an ergm to the network with missing data identified
summary(missnet~edges)
summary(ergm(missnet~edges))


## ----missnet-mod---------------------------------------------------------------------
missnet_bad <- missnet # create network with missing dyads set to 0
missnet_bad[4,6] <- missnet_bad[4,9] <- missnet_bad[5,6] <- 0

# fit an ergm to the network with missing dyads set to 0
summary(missnet_bad)
summary(ergm(missnet_bad~edges))


## ----flobusiness, message = F--------------------------------------------------------
set.seed(314159)
summary(flobusiness~edges+degree(1))
fit <- ergm(flobusiness~edges+degree(1))
summary(fit)
mcmc.diagnostics(fit)


## ----flobusiness-MCMCinterval, eval=FALSE--------------------------------------------
set.seed(271828)
fit <- ergm(flobusiness~edges+degree(1),
            control=snctrl(MCMC.interval=1))


## ----flomarriage-simulate------------------------------------------------------------
set.seed(101)
flomodel.03.sim <- simulate(flomodel.03,nsim=10)
class(flomodel.03.sim) # Reveal the class of the object created
summary(flomodel.03.sim) # quick summary of a network.list object
attributes(flomodel.03.sim) # Reveal the various attributes of this network.list


## ----flomarriage-simulate2-----------------------------------------------------------
rbind("obs"=summary(flomarriage~edges+nodecov("wealth")),
      "sim mean"=colMeans(attr(flomodel.03.sim, "stats"))) 


## ----flomarriage-simulate3-----------------------------------------------------------
# we can also plot individual simulations
flomodel.03.sim[[7]]
plot(flomodel.03.sim[[7]], 
     label= flomodel.03.sim[[7]] %v% "vertex.names",
     vertex.cex = (flomodel.03.sim[[7]] %v% "wealth")/25)


## ----flomarriage-gof-----------------------------------------------------------------
set.seed(54321) # The gof function uses random values
flomodel.03.gof <- gof(flomodel.03)
flomodel.03.gof
plot(flomodel.03.gof)


## ----mesa-gof------------------------------------------------------------------------
set.seed(12345)
mesamodel.02 <- ergm(mesa~edges)
mesamodel.02.gof <- gof(mesamodel.02~degree + esp + distance, 
                        control = snctrl(nsim=10))
plot(mesamodel.02.gof)


## ----magnolia------------------------------------------------------------------------
set.seed(10)
data('faux.magnolia.high')
magnolia <- faux.magnolia.high
magnolia
plot(magnolia, vertex.cex=.5)
summary(magnolia~edges+triangle) # Simple model for triad closure


## ----magnolia-triangle, eval=F-------------------------------------------------------
set.seed(100)
fit <- ergm(magnolia~edges+triangle,
            control=snctrl(MCMLE.effectiveSize=NULL))


## ----magnolia-triangle-halted, eval=T, message=F, warning = F------------------------
set.seed(1000)
fit <- ergm(magnolia~edges+triangle, 
            control=snctrl(MCMLE.maxit=2,MCMLE.effectiveSize=NULL))


## ----mcmc-diagnostics, eval=T, results='hide', fig.show='asis'-----------------------
mcmc.diagnostics(fit)


## ----gwesp, message = F, warning = F-------------------------------------------------
set.seed(10101)
fit <- ergm(magnolia~edges+gwesp(0.25, fixed=T), 
            control=snctrl(MCMC.interval = 10000),
            verbose=T)


## ----gwesp-diagnostics---------------------------------------------------------------
mcmc.diagnostics(fit)


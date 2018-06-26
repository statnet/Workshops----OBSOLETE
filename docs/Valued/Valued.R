## ----install packages----------------------------------------
install.packages("ergm.count")
install.packages("ergm.rank")
install.packages("latentnet")
update.packages()

## ----load libraries----------
library(ergm.count)
library(ergm.rank)
library(latentnet)

## ----constructing from a sociomatrix-------------------------------------
data(samplk)
ls()
as.matrix(samplk1)[1:5,1:5]
# Create a sociomatrix totaling the nominations.
samplk.tot.m<-as.matrix(samplk1)+as.matrix(samplk2)+as.matrix(samplk3)
samplk.tot.m[1:5,1:5]

# Create a network where the number of nominations becomes an attribute of an edge.
samplk.tot <- as.network(samplk.tot.m, directed=TRUE, matrix.type="a", 
                           ignore.eval=FALSE, names.eval="nominations" # Important!
                           )
# Add vertex attributes.  (Note that names were already imported!)
samplk.tot %v% "group" <- samplk1 %v% "group" # Groups identified by Sampson
samplk.tot %v% "group" 

# We can view the attribute as a sociomatrix.
as.matrix(samplk.tot,attrname="nominations")[1:5,1:5]

# Also, note that samplk.tot now has an edge if i nominated j *at least once*.
as.matrix(samplk.tot)[1:5,1:5]

## ----constructing from an edgelist---------------------------------------
samplk.tot.el <- as.matrix(samplk.tot, attrname="nominations", 
                           matrix.type="edgelist")
samplk.tot.el[1:5,]
# and an initial empty network.
samplk.tot2 <- samplk1 # Copy samplk1
delete.edges(samplk.tot2, seq_along(samplk.tot2$mel)) # Empty it out
samplk.tot2  #We could also have used network.initialize(18)

samplk.tot2[samplk.tot.el[,1:2], names.eval="nominations", add.edges=TRUE] <- 
  samplk.tot.el[,3]
as.matrix(samplk.tot2,attrname="nominations")[1:5,1:5]

## ----plotting via matrix-------------------------------------------------
par(mar=rep(0,4))
samplk.ecol <- 
  matrix(gray(1 - (as.matrix(samplk.tot, attrname="nominations")/3)),
         nrow=network.size(samplk.tot))
plot(samplk.tot, edge.col=samplk.ecol, usecurve=TRUE, edge.curve=0.0001, 
     displaylabels=TRUE, vertex.col=as.factor(samplk.tot%v%"group"))

## ----plotting via edge values--------------------------------------------
par(mar=rep(0,4))
valmat<-as.matrix(samplk.tot,attrname="nominations") #Pull the edge values
samplk.ecol <- 
  matrix(rgb(0,0,0,valmat/3),
         nrow=network.size(samplk.tot))
plot(samplk.tot, edge.col=samplk.ecol, usecurve=TRUE, edge.curve=0.0001, 
     displaylabels=TRUE, vertex.col=as.factor(samplk.tot%v%"group"),
     edge.lwd=valmat^2)

## ----zach plot-----------------------------------------------------------
data(zach)
zach.ecol <- gray(1 - (zach %e% "contexts")/8)
zach.vcol <- rainbow(5)[zach %v% "faction.id"+3]
par(mar=rep(0,4))
plot(zach, edge.col=zach.ecol, vertex.col=zach.vcol, displaylabels=TRUE)

## ----valued ERGM summary error-------------------------------
summary(samplk.tot~sum)

## ----valued ERGM summary-------------------------------------------------
summary(samplk.tot~sum, response="nominations")

## ----help ergm-references-------------------------------------
## help("ergm-references")

## ----DiscUnif reference-------------------------------------
y <- network.initialize(2,directed=FALSE) # A network with one dyad!
## Discrete Uniform reference
# 0 coefficient: discrete uniform
sim.du3<-simulate(y~sum, coef=0, reference=~DiscUnif(0,3),
                  response="w",statsonly=TRUE,nsim=1000)
# Negative coefficient: truncated geometric skewed to the right 
sim.trgeo.m1<-simulate(y~sum, coef=-1, reference=~DiscUnif(0,3),
                       response="w",statsonly=TRUE,nsim=1000)
# Positive coefficient: truncated geometric skewed to the left 
sim.trgeo.p1<-simulate(y~sum, coef=+1, reference=~DiscUnif(0,3),
                      response="w",statsonly=TRUE,nsim=1000)
# Plot them:
par(mfrow=c(1,3))
hist(sim.du3,breaks=diff(range(sim.du3))*4)
hist(sim.trgeo.m1,breaks=diff(range(sim.trgeo.m1))*4)
hist(sim.trgeo.p1,breaks=diff(range(sim.trgeo.p1))*4)

## ----Binomial reference-------------------------------------
## Binomial reference
# 0 coefficient: Binomial(3,1/2)
sim.binom3<-simulate(y~sum, coef=0, reference=~Binomial(3),
                     response="w",statsonly=TRUE,nsim=1000)
# -1 coefficient: Binomial(3, exp(-1)/(1+exp(-1)))
sim.binom3.m1<-simulate(y~sum, coef=-1, reference=~Binomial(3),
                        response="w",statsonly=TRUE,nsim=1000)
# +1 coefficient: Binomial(3, exp(1)/(1+exp(1)))
sim.binom3.p1<-simulate(y~sum, coef=+1, reference=~Binomial(3),
                        response="w",statsonly=TRUE,nsim=1000)
# Plot them:
par(mfrow=c(1,3))
hist(sim.binom3,breaks=diff(range(sim.binom3))*4)
hist(sim.binom3.m1,breaks=diff(range(sim.binom3.m1))*4)
hist(sim.binom3.p1,breaks=diff(range(sim.binom3.p1))*4)

## ----unbounded references------------------------------------------------
sim.geom<-simulate(y~sum, coef=log(2/3), reference=~Geometric,
                   response="w",statsonly=TRUE,nsim=1000)
mean(sim.geom)
sim.pois<-simulate(y~sum, coef=log(2), reference=~Poisson,
                   response="w",statsonly=TRUE,nsim=1000)
mean(sim.pois)

## ----unbounded histograms----------------------------------
par(mfrow=c(1,2))
hist(sim.geom,breaks=diff(range(sim.geom))*4)
hist(sim.pois,breaks=diff(range(sim.pois))*4)

## ----outside parameter space-------------------------------
par(mfrow=c(1,1))
sim.geo0<-simulate(y~sum, coef=0, reference=~Geometric,
                    response="w",statsonly=TRUE,nsim=100,
                    control=control.simulate(MCMC.burnin=0,MCMC.interval=1))
mean(sim.geo0)
plot(c(sim.geo0),xlab="MCMC iteration",ylab="Value of the tie")

## ----help ergm-terms-----------------------------------------
help("ergm-terms")

## ----monks total fit-----
samplk.tot.nm <- 
  ergm(samplk.tot~sum + nodematch("group",diff=TRUE,form="sum"), 
       response="nominations", reference=~Binomial(3)) 
mcmc.diagnostics(samplk.tot.nm)

## ----monks total fit summ------------------------------------------------
summary(samplk.tot.nm)

## ----create leader attribute---------------------------------------------
unique(zach %v% "role")
# Vertex attr. "leader" is TRUE for Hi and John, FALSE for others.
zach %v% "leader" <- zach %v% "role" != "Member" 

## ----zach sum leader fit----
zach.lead <- 
  ergm(zach~sum + nodefactor("leader"), 
       response="contexts", reference=~Poisson) 
mcmc.diagnostics(zach.lead)

## ----zach sum leader fit summ--------------------------------------------
summary(zach.lead)

## ----zach sum nonzero group fit----
samplk.tot.nm.nz <- 
  ergm(samplk.tot~sum + nonzero + nodematch("group",diff=TRUE,form="sum"), 
       response="nominations", reference=~Binomial(3))
mcmc.diagnostics(samplk.tot.nm.nz)

## ----zach sum nonzero group fit summ-------------------------------------
summary(samplk.tot.nm.nz)

## ----monks big fit-------
samplk.tot.ergm <- 
  ergm(samplk.tot ~ sum + nonzero + mutual("min") +
       transitiveweights("min","max","min") +
       cyclicalweights("min","max","min"),
       reference=~Binomial(3), response="nominations")
mcmc.diagnostics(samplk.tot.ergm)

## ----monks big fit summ--------------------------------------------------
summary(samplk.tot.ergm)

## ----zach big model------------------------------------------------------
summary(zach~sum+nonzero+nodefactor("leader")+absdiffcat("faction.id")+
        nodesqrtcovar(TRUE), response="contexts")

## ----zach big fit--------
zach.pois <- 
  ergm(zach~sum+nonzero+nodefactor("leader")+absdiffcat("faction.id")+
       nodesqrtcovar(TRUE),
       response="contexts", reference=~Poisson,
       control=control.ergm(MCMLE.trustregion=100, MCMLE.maxit=50), verbose=TRUE)
mcmc.diagnostics(zach.pois)

## ----zach big fit summ---------------------------------------------------
summary(zach.pois)

## ----zach big fit sim------------------------------------
# Simulate from model fit:
zach.sim <- 
  simulate(zach.pois, monitor=~transitiveweights("geomean","sum","geomean"),
           nsim = 1000, statsonly=TRUE)

## ----zach big fit sim summ-----------------------------------------------
# What have we simulated?
colnames(zach.sim)

# How high is the transitiveweights statistic in the observed network?
zach.obs <- summary(zach ~ transitiveweights("geomean","sum","geomean"), 
                    response="contexts")
zach.obs

## ----zach big fit sim plot----------------------------------
par(mar=c(5, 4, 4, 2) + 0.1)
# 9th col. = transitiveweights
plot(density(zach.sim[,9]))
abline(v=zach.obs)

## ----zach big fit sim test-----------------------------------------------
# Where does the observed value lie in the simulated?
# This is a p-value for the Monte-Carlo test:
min(mean(zach.sim[,9]>zach.obs), mean(zach.sim[,9]<zach.obs))*2

## ----setup ergm.rank----------------------------------------
library(ergm.rank)

## ----references ergm.rank------------------------------------
help("ergm-references", "ergm.rank")

## ----newcomb data--------------------------------------------------------
data(newcomb)
as.matrix(newcomb[[1]], attrname="rank")
as.matrix(newcomb[[1]], attrname="descrank")

## ----newcomb fit1----------------------------------------
newc.fit1<- ergm(newcomb[[1]]~rank.nonconformity+rank.nonconformity("localAND")+rank.deference,response="descrank",reference=~CompleteOrder,control=control.ergm(MCMLE.trustregion=1000, MCMC.burnin=4096, MCMC.interval=32, CD.conv.min.pval=0.05),eval.loglik=FALSE)

## ----newcomb fit1 summ---------------------------------------------------
summary(newc.fit1)

## ----newcomb fit1 mcmc----
mcmc.diagnostics(newc.fit1)

## ----newcomb fit15---------------------------------------
newc.fit15 <- ergm(newcomb[[15]]~rank.nonconformity+rank.nonconformity("localAND")+rank.deference,response="descrank",reference=~CompleteOrder,control=control.ergm(MCMLE.trustregion=1000, MCMC.burnin=4096, MCMC.interval=32, CD.conv.min.pval=0.05),eval.loglik=FALSE)

## ----newcomb fit15 summ--------------------------------------------------
summary(newc.fit15)

## ----newcomb fit15 mcmc----
mcmc.diagnostics(newc.fit15)

## ----monks nodematch mle fit-----------------------------
samplk.nm.l <- ergmm(samplk.tot~nodematch("group",diff=TRUE),tofit="mle", verbose=TRUE)

## ----monks nodematch ergm fit----------------------------
samplk.nm.e <- ergm(samplk.tot~edges+nodematch("group",diff=TRUE))

## ----monks nodematch compare---------------------------------------------
summary(samplk.nm.l, point.est="mle", se=TRUE)
summary(samplk.nm.e)

## ----monks latent fit------------------------------------
samplk.d2G3<-ergmm(samplk.tot~euclidean(d=2,G=3), verbose=TRUE)

## ----monks latent re fit----
samplk.d2G3r<-ergmm(samplk.tot~euclidean(d=2,G=3)+rreceiver, verbose=TRUE)
mcmc.diagnostics(samplk.d2G3r)

## ----monks latent re fit plot------------------------------
par(mfrow=c(1,2))
# Extract a clustering
Z.K.ref <- summary(samplk.d2G3,point.est="pmean")$pmean$Z.K 
# Plot one model, saving positions, using Z.K.ref to set reference clustering.
Z.ref <- plot(samplk.d2G3, pie=TRUE, Z.K.ref=Z.K.ref)
# Plot the other model, using Z.ref and Z.K.ref to ensure similar
# orientation and coloring.
plot(samplk.d2G3r, rand.eff="receiver", pie=TRUE, Z.ref=Z.ref, Z.K.ref=Z.K.ref)

## --------------------------------------------------------------
? families.ergmm

## ----monks count latent fit-------------------------------
samplk.ct.d2G3 <- 
  ergmm(samplk.tot~euclidean(d=2,G=3),response="nominations",
        family="binomial",fam.par=list(trials=3), verbose=TRUE)

## ----monks count latent fit plot---------------------------
# Plot them side-by-side, using edge.col argument:
par(mfrow=c(1,2))
plot(samplk.d2G3,pie=TRUE, Z.ref=Z.ref, Z.K.ref=Z.K.ref)
plot(samplk.ct.d2G3,pie=TRUE, Z.ref=Z.ref, Z.K.ref=Z.K.ref, edge.col=samplk.ecol)

## ----zach count latent fit----
zach.d2G2S <- ergmm(zach~nodefactor("leader")+euclidean(d=2,G=2)+rsociality,
                    response="contexts",family="Poisson",
                    verbose=TRUE)
mcmc.diagnostics(zach.d2G2S)

## ----zach count latent fit summ plot-------------------------------------
summary(zach.d2G2S)
par(mar=c(5, 4, 4, 2) + 0.1)
plot(zach.d2G2S, rand.eff="sociality", edge.col=zach.ecol, labels=TRUE)


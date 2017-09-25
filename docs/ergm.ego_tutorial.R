
## ----eval=FALSE----------------------------------------------------------
install.packages('ergm.ego')

## ----eval=FALSE----------------------------------------------------------
library(help='ergm.ego')

## ----eval=FALSE----------------------------------------------------------
help('ergm.ego-terms')

## ------------------------------------------------------------------------
library(ergm.ego)

## ----eval=FALSE----------------------------------------------------------
sessionInfo()

## ----cache=FALSE---------------------------------------------------------
set.seed(1)

## ------------------------------------------------------------------------
data("faux.mesa.high")
mesa <- faux.mesa.high

## ------------------------------------------------------------------------
plot(mesa, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

## ------------------------------------------------------------------------
mesa.ego <- as.egodata(mesa) # Warning because there were no vertex names in the first place.

## ------------------------------------------------------------------------
str(mesa.ego)
summary(mesa.ego)
head(mesa.ego$egos)
head(mesa.ego$alters)

## ------------------------------------------------------------------------
write(t(mesa.ego$egos), file="mesa.ego.table.csv", ncol=dim(mesa.ego$egos)[2], sep=",")
write(t(mesa.ego$alters), file="mesa.alter.table.csv", ncol=dim(mesa.ego$alters)[2], sep=",")

## ------------------------------------------------------------------------
mesa.egos <- read.csv("mesa.ego.table.csv")
head(mesa.egos)
mesa.alters <- read.csv("mesa.alter.table.csv")
head(mesa.alters)

## ------------------------------------------------------------------------
test <- as.egodata(mesa.egos,alters=mesa.alters,egoIDcol="egoID")
head(test$egos)
head(test$alters)

## ------------------------------------------------------------------------
table(mesa.ego$egos$Sex, exclude=NULL)
table(mesa.ego$egos$Race, exclude=NULL)
barplot(table(mesa.ego$egos$Grade), 
        main="Mesa Grade Distribution", ylab="frequency")

# compare egos and alters...

par(mfrow=c(1,2))
barplot(table(mesa.ego$egos$Race, exclude=NULL)/nrow(mesa.ego$egos),
        main="Ego Race Distn", ylab="percent",
        ylim = c(0,0.5))
barplot(table(mesa.ego$alters$Race, exclude=NULL)/nrow(mesa.ego$alters),
        main="Alter Race Distn", ylab="percent",
        ylim = c(0,0.5))

## ------------------------------------------------------------------------
# to get the crosstabulated counts of ties:

mixingmatrix.egodata(mesa.ego,"Grade")

# contrast with the network's mixmatrix:

mixingmatrix(mesa, "Grade")

# to get the row conditional probabilities:

mixingmatrix.egodata(mesa.ego, "Grade", rowprob=T)
mixingmatrix.egodata(mesa.ego, "Race", rowprob=T)

## ------------------------------------------------------------------------
# ties
nrow(mesa.ego$alters)

# mean degree
nrow(mesa.ego$alters)/nrow(mesa.ego$egos)


## overall degree distribution
summary(mesa.ego ~ degree(0:20))

## by sex, and race
summary(mesa.ego ~ degree(0:13, by="Sex"))
summary(mesa.ego ~ degree(0:13, by="Race"))

## ------------------------------------------------------------------------
summary(mesa.ego ~ degree(0:10), scaleto=100000)
summary(mesa.ego ~ degree(0:10), scaleto=nrow(mesa.ego$egos)*100)


## ------------------------------------------------------------------------
# to get the frequency counts

degreedist.egodata(mesa.ego)
degreedist.egodata(mesa.ego, by="Sex")

# to get the proportion at each degree level

degreedist.egodata(mesa.ego, by="Sex", prob=T)

## ------------------------------------------------------------------------
degreedist.egodata(mesa.ego, brg=T)

degreedist.egodata(mesa.ego, by="Sex", prob=T, brg=T)

## ------------------------------------------------------------------------
?control.ergm.ego

## ------------------------------------------------------------------------
fit.edges <- ergm.ego(mesa.ego ~ edges)
summary(fit.edges)

## ------------------------------------------------------------------------
names(fit.edges)
fit.edges$ppopsize
fit.edges$popsize

## ---- echo=F, eval=F-----------------------------------------------------
summary(ergm.ego(mesa.ego ~ edges,
                 control = control.ergm.ego(ppopsize=1000)))

## ------------------------------------------------------------------------
mcmc.diagnostics(fit.edges)

## ------------------------------------------------------------------------
plot(gof(fit.edges, GOF="model"))

## ------------------------------------------------------------------------
plot(gof(fit.edges, GOF="degree"))


## ----cache=FALSE---------------------------------------------------------
set.seed(1)

## ------------------------------------------------------------------------
fit.deg0 <- ergm.ego(mesa.ego ~ edges + degree(0), control=control.ergm.ego(ppopsize=1000))
summary(fit.deg0)

## ----eval=FALSE----------------------------------------------------------
## mcmc.diagnostics(fit.deg0)

## ------------------------------------------------------------------------
plot(gof(fit.deg0, GOF="model"))
plot(gof(fit.deg0, GOF="degree"))

## ------------------------------------------------------------------------
fit.full <- ergm.ego(mesa.ego ~ edges + degree(0:1) 
                      + nodefactor("Sex")
                      + nodefactor("Race", base=2)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))
summary(fit.full)

## ----eval=FALSE----------------------------------------------------------
mcmc.diagnostics(fit.full)

## ------------------------------------------------------------------------
plot(gof(fit.full, GOF="model"))
plot(gof(fit.full, GOF="degree"))

## ------------------------------------------------------------------------
sim.full <- simulate(fit.full)
summary(mesa.ego ~ edges + degree(0:1)
                      + nodefactor("Sex")
                      + nodefactor("Race", base=2)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))

summary(sim.full ~ edges + degree(0:1)
                      + nodefactor("Sex")
                      + nodefactor("Race", base=2)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))
plot(sim.full, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

## ------------------------------------------------------------------------
sim.full2 <- simulate(fit.full, popsize=network.size(mesa)*2)
summary(mesa~edges + degree(0:1)
                      + nodefactor("Sex")
                      + nodefactor("Race", base=2)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))*2

summary(sim.full2~edges + degree(0:1)
                      + nodefactor("Sex")
                      + nodefactor("Race", base=2)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))


## ------------------------------------------------------------------------
data(faux.magnolia.high)
faux.magnolia.high -> fmh
(N <- network.size(fmh))

## ------------------------------------------------------------------------
fit.ergm <- ergm(fmh~edges+degree(0:3)+nodefactor("Race")+nodematch("Race")
                    +nodefactor("Sex")+nodematch("Sex")+absdiff("Grade"))

## ------------------------------------------------------------------------
fmh.ego <- as.egodata(fmh)
head(fmh.ego)

egofit <- ergm.ego(fmh.ego~edges+degree(0:3)+nodefactor("Race")+nodematch("Race")
                  +nodefactor("Sex")+nodematch("Sex")+absdiff("Grade"), popsize=N,
                  ppopsize=N)

modse <- function(fit) sqrt(diag(vcov(fit, sources="model"))) # A convendience function.

# Parameters recovered:
param.compare <- cbind(coef(fit.ergm), coef(egofit)[-1], (coef(fit.ergm)-coef(egofit)[-1])/modse(egofit)[-1])
colnames(param.compare) <- c("Full nw est", "Ego census est", "diff Z")
round(param.compare, 3)

## ----eval=FALSE----------------------------------------------------------
## # MCMC diagnostics.
mcmc.diagnostics(egofit)

## ----eval=FALSE----------------------------------------------------------
## # Check whether the model converged to the right statistics:
plot(gof(egofit, GOF="model"))

## ------------------------------------------------------------------------
plot(gof(egofit, GOF="degree"))

## ----cache=FALSE---------------------------------------------------------
set.seed(1)

## ------------------------------------------------------------------------
fmh.egosampN <- sample(fmh.ego, N, replace=TRUE)
egofitN <- ergm.ego(fmh.egosampN ~ edges+degree(0:3)+nodefactor("Race")
                                 +nodematch("Race")+nodefactor("Sex")+nodematch("Sex")
                                 +absdiff("Grade"),popsize=N)

# compare the coef
param.compare <- cbind(coef(fit.ergm), coef(egofit)[-1], coef(egofitN)[-1])
colnames(param.compare) <- c("Full nw est", "Ego census est", "Sample N est")
round(param.compare, 3)
                 
# compare the s.e.'s
se.compare <- cbind(modse(fit.ergm), modse(egofit)[-1], modse(egofitN)[-1])
colnames(se.compare) <- c("Full nw SE", "Ego census SE", "Sample N SE")
round(se.compare, 3)

## ------------------------------------------------------------------------
fmh.egosampN4 <- sample(fmh.ego, round(N/4), replace=TRUE)

egofitN4 <- ergm.ego(fmh.egosampN4~edges+degree(0:3)+nodefactor("Race")
                     +nodematch("Race")+nodefactor("Sex")+nodematch("Sex")
                     +absdiff("Grade"), popsize=N)
# compare the coef
param.compare <- cbind(coef(fit.ergm), coef(egofit)[-1], coef(egofitN)[-1], coef(egofitN4)[-1])
colnames(param.compare) <- c("Full nw est", "Ego census est", "Sample N est", "Sample N/4 est")
round(param.compare, 3)
                 
# compare the s.e.'s
se.compare <- cbind(modse(fit.ergm), modse(egofit)[-1], modse(egofitN)[-1], modse(egofitN4)[-1])
colnames(se.compare) <- c("Full nw SE", "Ego census SE", "Sample N SE", "Sample N/4 SE")
round(se.compare, 3)

## ------------------------------------------------------------------------
w <- 1+3*((fmh %v% "Race")!="White")
fmh.egosampN4w <- sample(fmh.ego, round(N/4), replace=TRUE, prob=w)

head(fmh.egosampN4w)
egofitN4w<-ergm.ego(fmh.egosampN4w~edges+degree(0:3)+nodefactor("Race")+nodematch("Race")+nodefactor("Sex")+nodematch("Sex")+absdiff("Grade"), popsize=N)

## ------------------------------------------------------------------------
# compare the coef

param.compare <- cbind(coef(fit.ergm), coef(egofitN4)[-1], coef(egofitN4w)[-1])
colnames(param.compare) <- c("Full nw est", "Sample N/4 est", "Oversampled")
round(param.compare, 3)
                 
# compare the s.e.'s

se.compare <- cbind(modse(fit.ergm),  modse(egofitN4)[-1], modse(egofitN4w)[-1])
colnames(se.compare) <- c("Full nw SE", "Sample N/4 SE", "Oversampled SE")
round(se.compare, 3)



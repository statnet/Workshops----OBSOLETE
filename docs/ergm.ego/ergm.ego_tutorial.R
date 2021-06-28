
## ---- eval=F--------------------------------------------------------------------------------
install.packages('ergm.ego')
#install.packages('ergm.ego', repos='https://statnet.r-universe.dev')


## -------------------------------------------------------------------------------------------
library('ergm.ego')
packageVersion('ergm.ego')


## ---- eval=F--------------------------------------------------------------------------------
example(sample.egor)


## ----eval=FALSE-----------------------------------------------------------------------------
library(help='ergm.ego')


## ----eval=FALSE-----------------------------------------------------------------------------
?as.egor


## ----eval=FALSE-----------------------------------------------------------------------------
help('ergm.ego-terms')


## ---- message=F-----------------------------------------------------------------------------
library(ergm.ego)


## ----eval=FALSE-----------------------------------------------------------------------------
sessionInfo()


## ----cache=FALSE----------------------------------------------------------------------------
set.seed(1)


## ---- message=F-----------------------------------------------------------------------------
data(faux.mesa.high)
mesa <- faux.mesa.high


## ---- message=F-----------------------------------------------------------------------------
plot(mesa, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)


## ---- message=F-----------------------------------------------------------------------------
mesa.ego <- as.egor(mesa) 


## ---- message=F-----------------------------------------------------------------------------
names(mesa.ego) # what are the components of this object?
mesa.ego # prints a few lines for each component
#View(mesa.ego) # opens the component in the Rstudio source window
class(mesa.ego) # what type of "object" is this?



## ---- message=F-----------------------------------------------------------------------------
class(mesa.ego$ego) # and what type of objects are the components?
class(mesa.ego$alter)
class(mesa.ego$aatie)


## ---- message=F-----------------------------------------------------------------------------
mesa.ego$ego # first few rows of the ego table


## ---- message=F-----------------------------------------------------------------------------
mesa.ego$alter # first few rows of the alter table

# ties show up twice, but alter info is linked to .altID
mesa.ego$alter %>% filter((.altID==1 & .egoID==25) | (.egoID==1 & .altID==25))


## ---- message=F-----------------------------------------------------------------------------
mesa.ego$aatie # first few rows of the alter table


## ---- message=F-----------------------------------------------------------------------------
# egos
write.csv(mesa.ego$ego, file="mesa.ego.table.csv", row.names = F)

# alters
write.csv(mesa.ego$alter[,-1], file="mesa.alter.table.csv", row.names = F)


## ---- message=F-----------------------------------------------------------------------------
mesa.egos <- read.csv("mesa.ego.table.csv")
head(mesa.egos)
mesa.alts <- read.csv("mesa.alter.table.csv")
head(mesa.alts)


## ---- message=F-----------------------------------------------------------------------------
my.egodata <- egor(egos = mesa.egos, 
                   alters = mesa.alts, 
                   ID.vars = list(ego = ".egoID"))
my.egodata


## ----message=F, eval=F----------------------------------------------------------------------
example("egor")


## ---- message=F-----------------------------------------------------------------------------
# to reduce typing, we'll pull the ego and alter data frames
egos <- mesa.ego$ego
alters <- mesa.ego$alter

table(egos$Sex, exclude=NULL)
table(egos$Race, exclude=NULL)
barplot(table(egos$Grade), ylab="frequency")

# compare egos and alters...

par(mfrow=c(1,2))
barplot(table(egos$Race)/nrow(egos),
        main="Ego Race Distn", ylab="percent",
        ylim = c(0,0.5))
barplot(table(alters$Race)/nrow(alters),
        main="Alter Race Distn", ylab="percent",
        ylim = c(0,0.5))


## ---- message=F-----------------------------------------------------------------------------
# to get the crosstabulated counts of ties:
mixingmatrix(mesa.ego,"Grade")

# contrast with the original network crosstab:
mixingmatrix(mesa, "Grade")



## ---- message=F-----------------------------------------------------------------------------
# to get the row conditional probabilities:

round(mixingmatrix(mesa.ego, "Grade", rowprob=T), 2)
round(mixingmatrix(mesa.ego, "Race", rowprob=T), 2)


## ---- message=F-----------------------------------------------------------------------------
# first, using the original network
network.edgecount(faux.mesa.high)

# compare to the egodata
# note that the ties are double counted, so we need to divide by 2.
nrow(mesa.ego$alter)/2

# mean degree -- here we want to count each "stub", so we don't divide by 2
nrow(mesa.ego$alter)/nrow(mesa.ego$ego)


# overall degree distribution
summary(mesa.ego ~ degree(0:20))

# and stratified by sex
summary(mesa.ego ~ degree(0:13, by="Sex"))


## ---- message=F-----------------------------------------------------------------------------
summary(mesa.ego ~ degree(0:10), scaleto=100000)
summary(mesa.ego ~ degree(0:10), scaleto=nrow(mesa.ego$ego)*100)



## ---- message=F-----------------------------------------------------------------------------
# to get the frequency counts

degreedist(mesa.ego, plot=T)
degreedist(mesa.ego, by="Sex", plot=T)

# to get the proportion at each degree level

degreedist(mesa.ego, by="Sex", plot=T, prob=T)


## ---- message=F-----------------------------------------------------------------------------
degreedist(mesa.ego, brg=T)

degreedist(mesa.ego, by="Sex", prob=T, brg=T)


## ---- message=F, eval=FALSE-----------------------------------------------------------------
?control.ergm.ego


## ---- message=F-----------------------------------------------------------------------------
fit.edges <- ergm.ego(mesa.ego ~ edges)
summary(fit.edges)


## ---- message=F-----------------------------------------------------------------------------
names(fit.edges)
fit.edges$ppopsize
fit.edges$popsize


## ---- message=F-----------------------------------------------------------------------------
summary(ergm.ego(mesa.ego ~ edges, 
                 control = control.ergm.ego(ppopsize=1000)))


## ---- message=F-----------------------------------------------------------------------------
mcmc.diagnostics(fit.edges, which ="plots")


## ---- message=F-----------------------------------------------------------------------------
plot(gof(fit.edges, GOF="model"))


## ---- message=F-----------------------------------------------------------------------------
plot(gof(fit.edges, GOF="degree"))



## ----cache=FALSE----------------------------------------------------------------------------
set.seed(1)


## ---- message=F-----------------------------------------------------------------------------
fit.deg0 <- ergm.ego(mesa.ego ~ edges + degree(0), control=control.ergm.ego(ppopsize=1000))
summary(fit.deg0)


## ----eval=FALSE-----------------------------------------------------------------------------
mcmc.diagnostics(fit.deg0, which = "plots")


## ---- message=F-----------------------------------------------------------------------------
plot(gof(fit.deg0, GOF="model"))
plot(gof(fit.deg0, GOF="degree"))


## ---- message=F-----------------------------------------------------------------------------
fit.full <- ergm.ego(mesa.ego ~ edges + degree(0:1) 
                     + nodefactor("Sex")
                     + nodefactor("Race", levels = -LARGEST)
                     + nodefactor("Grade")
                     + nodematch("Sex") 
                     + nodematch("Race") 
                     + nodematch("Grade"))
summary(fit.full)


## ----eval=FALSE-----------------------------------------------------------------------------
mcmc.diagnostics(fit.full, which = "plots")


## ---- message=F-----------------------------------------------------------------------------
plot(gof(fit.full, GOF="model"))
plot(gof(fit.full, GOF="degree"))


## ---- message=F-----------------------------------------------------------------------------
sim.full <- simulate(fit.full)
summary(mesa.ego ~ edges + degree(0:1)
                      + nodefactor("Sex")
                      + nodefactor("Race", levels = -LARGEST)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))

summary(sim.full ~ edges + degree(0:1)
                      + nodefactor("Sex")
                      + nodefactor("Race", levels = -LARGEST)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))
plot(sim.full, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)


## ---- message=F-----------------------------------------------------------------------------
sim.full2 <- simulate(fit.full, popsize=network.size(mesa)*2)
summary(mesa~edges + degree(0:1)
                      + nodefactor("Sex")
                      + nodefactor("Race", levels = -LARGEST)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))*2

summary(sim.full2~edges + degree(0:1)
                      + nodefactor("Sex")
                      + nodefactor("Race", levels = -LARGEST)
                      + nodefactor("Grade")
                      + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))



## ---- message=F-----------------------------------------------------------------------------
data(faux.magnolia.high)
faux.magnolia.high -> fmh
N <- network.size(fmh)


## ---- message=F-----------------------------------------------------------------------------
fit.ergm <- ergm(fmh ~ degree(0:3) 
                 + nodefactor("Race", levels=TRUE) + nodematch("Race")
                 + nodefactor("Sex") + nodematch("Sex") 
                 + absdiff("Grade"))
round(coef(fit.ergm), 3)


## ---- message=F-----------------------------------------------------------------------------
fmh.ego <- as.egor(fmh)
head(fmh.ego)

egofit <- ergm.ego(fmh.ego ~ degree(0:3) 
                   + nodefactor("Race", levels=TRUE) + nodematch("Race")
                   + nodefactor("Sex") + nodematch("Sex") 
                   + absdiff("Grade"), popsize=N,
				  control=control.ergm.ego(ppopsize=N))

# A convenience function.
model.se <- function(fit) sqrt(diag(vcov(fit)))

# Parameters recovered:
coef.compare <- data.frame(
  "NW est" = coef(fit.ergm), 
  "Ego Cen est" = coef(egofit)[-1],
  "diff Z" = (coef(fit.ergm)-coef(egofit)[-1])/model.se(egofit)[-1])

round(coef.compare, 3)


## ----eval=FALSE-----------------------------------------------------------------------------
# MCMC diagnostics. 
mcmc.diagnostics(egofit, which="plots")


## ----eval=FALSE-----------------------------------------------------------------------------
plot(gof(egofit, GOF="model"))


## ---- message=F-----------------------------------------------------------------------------
plot(gof(egofit, GOF="degree"))


## ----cache=FALSE----------------------------------------------------------------------------
set.seed(1)


## ---- message=F-----------------------------------------------------------------------------
fmh.egosampN <- sample(fmh.ego, N, replace=TRUE)
egofitN <- ergm.ego(fmh.egosampN ~ degree(0:3) 
                    + nodefactor("Race", levels=TRUE) + nodematch("Race") 
                    + nodefactor("Sex") + nodematch("Sex")
                    + absdiff("Grade"),
                    popsize=N)

# compare the coef
coef.compare <- data.frame(
  "NW est" = coef(fit.ergm), 
  "Ego SampN est" = coef(egofitN)[-1],
  "diff Z" = (coef(fit.ergm)-coef(egofitN)[-1])/model.se(egofitN)[-1])

round(coef.compare, 3)
                 
# compare the s.e.'s
se.compare <- data.frame(
  "NW SE" = model.se(fit.ergm), 
  "Ego census SE" =model.se(egofit)[-1], 
  "Ego SampN SE" = model.se(egofitN)[-1])

round(se.compare, 3)


## ---- message=F-----------------------------------------------------------------------------
set.seed(0) # Some samples have different sets of alter levels from ego levels.

fmh.egosampN4 <- sample(fmh.ego, round(N/4), replace=TRUE)

egofitN4 <- ergm.ego(fmh.egosampN4 ~ degree(0:3) 
                    + nodefactor("Race", levels=TRUE) + nodematch("Race") 
                    + nodefactor("Sex") + nodematch("Sex")
                    + absdiff("Grade"),
                    popsize=N)

# compare the coef
coef.compare <- data.frame(
  "NW est" = coef(fit.ergm), 
  "Ego SampN4 est" = coef(egofitN4)[-1],
  "diff Z" = (coef(fit.ergm)-coef(egofitN4)[-1])/model.se(egofitN4)[-1])

round(coef.compare, 3)

# compare the s.e.'s
se.compare <- data.frame(
  "NW SE" = model.se(fit.ergm), 
  "Ego census SE" =model.se(egofit)[-1], 
  "Ego SampN SE" = model.se(egofitN)[-1],
  "Ego Samp4 SE" = model.se(egofitN4)[-1])

round(se.compare, 3)


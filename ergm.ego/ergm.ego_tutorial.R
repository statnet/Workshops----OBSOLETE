## ----setup, cache=FALSE, include=FALSE-----------------------------------
library(knitr)
knitr::opts_chunk$set(cache=T, comment=NA, fig.align='center')
options(width=100)

## ----include=FALSE-------------------------------------------------------
SHOW_MSG = FALSE


## ----eval=FALSE----------------------------------------------------------
install.packages('ergm.ego')


## ---- eval=FALSE---------------------------------------------------------
install.packages("dplyr")
install.packages("tibble")


## ----cache=FALSE, message=FALSE------------------------------------------
library(ergm.ego)
library(dplyr)
library(tibble)


## ------------------------------------------------------------------------
# Rounding of numeric columns only in dataframes 
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

# extract se's for estimates from fit object
modse <- function(fit) {sqrt(diag(vcov(fit, sources="model")))}



## ----cache=FALSE---------------------------------------------------------
sessionInfo()


## ----eval=FALSE----------------------------------------------------------
help(package="ergm.ego")


## ----eval=FALSE----------------------------------------------------------
help('ergm.ego-terms')


## ----cache=FALSE---------------------------------------------------------
set.seed(1)


## ------------------------------------------------------------------------
data("faux.mesa.high")
mesa <- faux.mesa.high


## ------------------------------------------------------------------------
plot(mesa, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)


## ------------------------------------------------------------------------
mesa.ego <- as.egodata(mesa) # Generates warning because there are no vertex IDs.


## ------------------------------------------------------------------------
str(mesa.ego)
summary(mesa.ego)
head(mesa.ego$egos)
head(mesa.ego$alters)


## ------------------------------------------------------------------------
write.csv(mesa.ego$egos, file="mesa.ego.table.csv", row.names=FALSE)
write.csv(mesa.ego$alters, file="mesa.alter.table.csv", row.names=FALSE)


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

mixingmatrix(mesa.ego,"Grade")

# contrast with the original network crosstab:

mixingmatrix(mesa, "Grade")

# to get the row conditional probabilities:

mixingmatrix(mesa.ego, "Grade", rowprob=T)
mixingmatrix(mesa.ego, "Race", rowprob=T)


## ------------------------------------------------------------------------
# ties
nrow(mesa.ego$alters)

# mean degree
nrow(mesa.ego$alters)/nrow(mesa.ego$egos)


# overall degree distribution
summary(mesa.ego ~ degree(0:20))

# and stratified by sex
summary(mesa.ego ~ degree(0:13, by="Sex"))


## ------------------------------------------------------------------------
summary(mesa.ego ~ degree(0:10), scaleto=100000)
summary(mesa.ego ~ degree(0:10), scaleto=nrow(mesa.ego$egos)*100)



## ------------------------------------------------------------------------
# to get the frequency counts

degreedist(mesa.ego)
degreedist(mesa.ego, by="Sex")

# to get the proportion at each degree level

degreedist(mesa.ego, by="Sex", prob=T)


## ------------------------------------------------------------------------
degreedist(mesa.ego, brg=T)

degreedist(mesa.ego, by="Sex", prob=T, brg=T)


## ----eval=FALSE----------------------------------------------------------
?control.ergm.ego


## ----message=SHOW_MSG----------------------------------------------------
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

## ----message=SHOW_MSG----------------------------------------------------
fit.deg0 <- ergm.ego(mesa.ego ~ edges + degree(0), control=control.ergm.ego(ppopsize=1000))
summary(fit.deg0)

## ----eval=FALSE----------------------------------------------------------
mcmc.diagnostics(fit.deg0)

## ------------------------------------------------------------------------
plot(gof(fit.deg0, GOF="model"))
plot(gof(fit.deg0, GOF="degree"))


## ----cache=FALSE---------------------------------------------------------
set.seed(1)

## ---- message=SHOW_MSG---------------------------------------------------
fit.full <- ergm.ego(mesa.ego ~ edges + degree(0:1) 
                      + nodefactor("Sex")
                      + nodefactor("Race", levels=-2)
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

# compare values of the observed to the simulated statistics
cbind( original = summary(mesa ~ edges + degree(0:1)
                          + nodefactor("Sex") + nodefactor("Race", levels=-2) + nodefactor("Grade")
                          + nodematch("Sex") + nodematch("Race") + nodematch("Grade")),
       simulated = summary(sim.full ~ edges + degree(0:1)
                           + nodefactor("Sex") + nodefactor("Race", levels=-2) + nodefactor("Grade")
                           + nodematch("Sex") + nodematch("Race") + nodematch("Grade")),
       difference = summary(mesa ~ edges + degree(0:1)
                            + nodefactor("Sex") + nodefactor("Race", levels=-2) + nodefactor("Grade")
                            + nodematch("Sex") + nodematch("Race") + nodematch("Grade")) -
                    summary(sim.full ~ edges + degree(0:1)
                            + nodefactor("Sex") + nodefactor("Race", levels=-2) + nodefactor("Grade")
                            + nodematch("Sex") + nodematch("Race") + nodematch("Grade")))

# eyeball the plot of the simulated data
plot(sim.full, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)


## ------------------------------------------------------------------------
sim.full2 <- simulate(fit.full, popsize=network.size(mesa)*2)

cbind( original = summary(mesa ~ edges + degree(0:1)
                          + nodefactor("Sex") + nodefactor("Race", levels=-2) + nodefactor("Grade")
                          + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))*2,
       simulated = summary(sim.full2 ~ edges + degree(0:1)
                           + nodefactor("Sex") + nodefactor("Race", levels=-2) + nodefactor("Grade")
                           + nodematch("Sex") + nodematch("Race") + nodematch("Grade")),
       difference = summary(mesa ~ edges + degree(0:1)
                            + nodefactor("Sex") + nodefactor("Race", levels=-2) + nodefactor("Grade")
                            + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))*2 -
                    summary(sim.full2 ~ edges + degree(0:1)
                            + nodefactor("Sex") + nodefactor("Race", levels=-2)
                            + nodefactor("Grade") + nodematch("Sex") + nodematch("Race") + nodematch("Grade")))



## ------------------------------------------------------------------------
data(faux.magnolia.high)
faux.magnolia.high -> fmh
(N <- network.size(fmh))


## ---- message=SHOW_MSG---------------------------------------------------
fit <- ergm(fmh ~ edges+degree(0) 
                       + nodefactor("Race") + nodefactor("Sex") 
                       + nodematch("Race") + nodematch("Sex") + absdiff("Grade"))
plot(gof(fit))


## ------------------------------------------------------------------------
fmh.ego <- as.egodata(fmh)
head(fmh.ego)

## ---- message=SHOW_MSG---------------------------------------------------
fit.ego <- ergm.ego(fmh.ego ~ edges + degree(0) 
                   + nodefactor("Race") + nodefactor("Sex") 
                   + nodematch("Race") + nodematch("Sex") + absdiff("Grade"), 
                   popsize=N, ppopsize=N)

# Parameters recovered (we're using utilities from the tidyverse here)
# Compare fit to egocentric census:
coefs1 <- tibble::rownames_to_column(data.frame(coef(fit))) %>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.ego)))) %>%
  mutate(Zdiff = (coef.fit. - coef.fit.ego.)/sqrt(modse(fit)^2 + modse(fit.ego)[-1]^2)) # note use of modse function we defined

round_df(coefs1, 3) # note use of round_df function we defined


## ----eval=FALSE----------------------------------------------------------
# MCMC diagnostics. 
mcmc.diagnostics(fit.ego)

## ----eval=FALSE----------------------------------------------------------
# Check whether the model converged to the right statistics:
plot(gof(fit.ego, GOF="model"))


## ------------------------------------------------------------------------
plot(gof(fit.ego, GOF="degree"))


## ----cache=FALSE---------------------------------------------------------
set.seed(1)

## ---- message=SHOW_MSG---------------------------------------------------
fmh.egosampN <- sample(fmh.ego, N, replace=TRUE)
fit.ego.sampN <- ergm.ego(fmh.egosampN ~ edges + degree(0) 
                    + nodefactor("Race") + nodefactor("Sex") 
                    + nodematch("Race") + nodematch("Sex") + absdiff("Grade"),
                    popsize=N)

# compare the coef: fit to 100% sample
coefs2 <- tibble::rownames_to_column(data.frame(coef(fit))) %>%
#  left_join(tibble::rownames_to_column(data.frame(coef(fit.ego))))  %>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.ego.sampN)))) %>%
  mutate(Zdiff = (coef.fit. - coef.fit.ego.sampN.)/sqrt(modse(fit)^2 + modse(fit.ego.sampN)[-1]^2))

round_df(coefs2, 3) # note use of round_df function we defined


# compare the s.e.'s
se2 <- tibble::rownames_to_column(data.frame(modse(fit))) %>%
  left_join(tibble::rownames_to_column(data.frame(modse(fit.ego.sampN)))) %>%
  mutate(se.ratio = modse.fit.ego.sampN./modse.fit.)

round_df(se2, 3)


## ------------------------------------------------------------------------
cbind("census" = table(fmh.ego$egos$Race),
      "sample" = table(fmh.egosampN$egos$Race))


## ----cache=FALSE---------------------------------------------------------
set.seed(1)

## ---- message=SHOW_MSG---------------------------------------------------
fit.egosamp.25 <- sample(fmh.ego, round(0.25*N), replace=TRUE)

fit.ego.samp.25 <- ergm.ego(fit.egosamp.25 ~ edges + degree(0) + nodefactor("Race")
                      + nodematch("Race") + nodefactor("Sex") + nodematch("Sex")
                      + absdiff("Grade"), popsize=N)

# this time we build the s.e. matrix first, because sampling may lead to unobserved race group(s)
se3 <- tibble::rownames_to_column(data.frame(modse(fit)))  %>%
  left_join(tibble::rownames_to_column(data.frame(modse(fit.ego.samp.25)))) %>%
  mutate(se.ratio = modse.fit.ego.samp.25./modse.fit.)

# compute se for Zdiff(coef)
zdiffse <- sqrt(se3$modse.fit.^2 + se3$modse.fit.ego.samp.25.^2)
                
# compare the coef: fit to 25% sample
coefs3 <- tibble::rownames_to_column(data.frame(coef(fit))) %>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.ego.samp.25)))) %>%
  mutate(Zdiff = (coef.fit. - coef.fit.ego.samp.25.)/zdiffse)

round_df(coefs3, 3)

round_df(se3, 3)


## ---- message=SHOW_MSG---------------------------------------------------
w <- 1 + 3*((fmh %v% "Race")!="White") # set up the stratification design

fit.egosamp.25strat <- sample(fmh.ego, round(N/4), replace=TRUE, prob=w)

# look at the egodata object, see the egoWt component:
head(fit.egosamp.25strat)

fit.ego.samp.25strat <- ergm.ego(fit.egosamp.25strat ~ edges + degree(0) 
                      + nodefactor("Race") + nodefactor("Sex") 
                      + nodematch("Race") + nodematch("Sex") + absdiff("Grade"), 
                      popsize=N)


## ------------------------------------------------------------------------
# again we create the s.e. matrix first, using the original fit to define the rows/terms
se4 <- tibble::rownames_to_column(data.frame(modse(fit)))  %>%
  left_join(tibble::rownames_to_column(data.frame(modse(fit.ego.samp.25))))  %>%
  left_join(tibble::rownames_to_column(data.frame(modse(fit.ego.samp.25strat)))) %>%
  mutate(se.ratio = modse.fit.ego.samp.25strat./modse.fit.ego.samp.25.) %>%
  select(-modse.fit.)

# compute se for Zdiff(coef)
zdiffse <- sqrt(se4$modse.fit.ego.samp.25.^2 + se4$modse.fit.ego.samp.25strat.^2)

# compare the coef: fit to stratified samp
coefs4 <- tibble::rownames_to_column(data.frame(coef(fit)))%>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.ego.samp.25))))  %>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.ego.samp.25strat)))) %>%
  mutate(Zdiff = (coef.fit. - coef.fit.ego.samp.25strat.)/zdiffse) %>%
  select(-coef.fit.)

round_df(coefs4, 3)

round_df(se4, 3)



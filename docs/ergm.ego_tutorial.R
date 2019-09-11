## ----setup, cache=FALSE, include=FALSE-----------------------------------
library(knitr)
knitr::opts_chunk$set(cache=T, comment=NA, fig.align='center')
options(width=100)

## ----include=FALSE-------------------------------------------------------
SHOW_MSG = FALSE


## ----eval=FALSE----------------------------------------------------------
## install.packages('ergm.ego')


## ---- eval=FALSE---------------------------------------------------------
## install.packages("dplyr")
## install.packages("tibble")


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
se.coef <- function(fit,src) sqrt(diag(vcov(fit, source=src)))



## ----cache=FALSE---------------------------------------------------------
sessionInfo()


## ----eval=FALSE----------------------------------------------------------
## help(package="ergm.ego")


## ----eval=FALSE----------------------------------------------------------
## help('ergm.ego-terms')


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
## ?control.ergm.ego


## ----message=SHOW_MSG----------------------------------------------------
fit.edges <- ergm.ego(mesa.ego ~ edges)
summary(fit.edges)


## ------------------------------------------------------------------------
names(fit.edges)
fit.edges$ppopsize
fit.edges$popsize


## ---- echo=F, eval=F-----------------------------------------------------
## summary(ergm.ego(mesa.ego ~ edges,
##                  control = control.ergm.ego(ppopsize=1000)))


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
## mcmc.diagnostics(fit.deg0)

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
## mcmc.diagnostics(fit.full)

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
fit.net <- ergm(fmh ~ edges+degree(0) 
                       + nodefactor("Race", levels= -6) + nodefactor("Sex") 
                       + nodematch("Race") + nodematch("Sex") + absdiff("Grade"))
plot(gof(fit.net))


## ---- census-------------------------------------------------------------
fmh.ego <- as.egodata(fmh)
head(fmh.ego)

## ---- message=SHOW_MSG---------------------------------------------------
fit.census <- ergm.ego(fmh.ego ~ edges + degree(0) 
                   + nodefactor("Race", levels= -6) + nodefactor("Sex") 
                   + nodematch("Race") + nodematch("Sex") + absdiff("Grade"), 
                   popsize=N)
summary(fit.census)


## ---- message=SHOW_MSG---------------------------------------------------
# note use of se.coef function we defined up top
se.coef.net <- se.coef(fit.net, src = "estimation")
se.coef.census <- se.coef(fit.census, src = "estimation")[-1]

se.census <- tibble::rownames_to_column(data.frame(se.coef.net))  %>%
  left_join(tibble::rownames_to_column(data.frame(se.coef.census))) %>%
  mutate(se.ratio = se.coef.census/se.coef.net)
round_df(se.census, 3)



## ---- message=SHOW_MSG---------------------------------------------------
# Parameters recovered (we're using utilities from the tidyverse here)
# Compare fit to egocentric census:
se.diff = sqrt(se.coef.net^2 + se.coef.census^2)

coefs1 <- tibble::rownames_to_column(data.frame(coef(fit.net))) %>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.census)))) %>%
  mutate(Zdiff = (coef.fit.net. - coef.fit.census.)/se.diff)

round_df(coefs1, 3) # note use of round_df function we defined


## ----eval=FALSE----------------------------------------------------------
## # MCMC diagnostics.
## mcmc.diagnostics(fit.census)

## ----eval=FALSE----------------------------------------------------------
## # Check whether the model converged to the right statistics:
## plot(gof(fit.census, GOF="model"))


## ------------------------------------------------------------------------
plot(gof(fit.census, GOF="degree"))


## ----sampN, cache=FALSE--------------------------------------------------
set.seed(1)

## ---- message=SHOW_MSG---------------------------------------------------
fmh.egosampN <- sample(fmh.ego, N, replace=TRUE)
fit.sampN <- ergm.ego(fmh.egosampN ~ edges + degree(0) 
                          + nodefactor("Race", levels= -6) + nodefactor("Sex") 
                          + nodematch("Race") + nodematch("Sex") + absdiff("Grade"),
                          popsize=N)
summary(fit.sampN)


## ---- message=SHOW_MSG---------------------------------------------------
# this time, use the se from src=all for the sampled net
se.coef.sampN <- se.coef(fit.sampN, src = "all")[-1]

# compare the s.e.'s
se.sampN <- tibble::rownames_to_column(data.frame(se.coef.net)) %>%
  left_join(tibble::rownames_to_column(data.frame(se.coef.sampN))) %>%
  mutate(se.ratio = se.coef.sampN/se.coef.net)

round_df(se.sampN, 3)



## ------------------------------------------------------------------------
cbind("census" = table(fmh.ego$egos$Race),
      "sampleN" = table(fmh.egosampN$egos$Race))


## ---- message=SHOW_MSG---------------------------------------------------
se.diff = sqrt(se.coef.net^2 + se.coef.sampN^2) 
# compare the coef
coefs2 <- tibble::rownames_to_column(data.frame(coef(fit.net))) %>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.sampN)))) %>%
  mutate(Zdiff = (coef.fit.net. - coef.fit.sampN.)/se.diff)

round_df(coefs2, 3) # note use of round_df function we defined


## ----cache=FALSE---------------------------------------------------------
set.seed(2)

## ----samp25, message=SHOW_MSG--------------------------------------------
fmh.egosamp25 <- sample(fmh.ego, round(0.25*N), replace=TRUE)


## ------------------------------------------------------------------------
cbind("census" = table(fmh.ego$egos$Race),
      "sample25%" = table(fmh.egosamp25$egos$Race))


## ---- message=SHOW_MSG---------------------------------------------------
fit.samp25 <- ergm.ego(fmh.egosamp25 ~ edges + degree(0) 
                          + nodefactor("Race", levels= -6) + nodefactor("Sex") 
                          + nodematch("Race") + nodematch("Sex") + absdiff("Grade"),
                          popsize=N)

# this time we build the s.e. matrix first, because sampling may lead to 
# unobserved race group(s), and check the ratios of the se's

se.coef.samp25 <- se.coef(fit.samp25, src = "all")[-1]

se.samp25 <- tibble::rownames_to_column(data.frame(se.coef.sampN))  %>%
  left_join(tibble::rownames_to_column(data.frame(se.coef.samp25))) %>%
  mutate(se.ratio = se.coef.samp25/se.coef.sampN)
round_df(se.samp25, 3)


## ---- message=SHOW_MSG---------------------------------------------------
# compute se for Zdiff(coef)
se.diff <- sqrt(se.coef.net^2 + se.coef.samp25^2)
                
# compare the coef: fit vs. 25% sample
coefs3 <- tibble::rownames_to_column(data.frame(coef(fit.net))) %>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.samp25)))) %>%
  mutate(Zdiff = (coef.fit.net. - coef.fit.samp25.)/se.diff)

round_df(coefs3, 3)



## ---- message=SHOW_MSG---------------------------------------------------
w <- 1 + 3*((fmh %v% "Race")!="White") # set up the weighting design

fmh.egosamp25wtd <- sample(fmh.ego, round(N/4), replace=TRUE, prob=w)

# look at the egodata object, see the egoWt component:
head(fmh.egosamp25wtd)


## ------------------------------------------------------------------------
cbind("sample25%" = table(fmh.egosamp25$egos$Race),
      "sampleWtd25%" = table(fmh.egosamp25wtd$egos$Race))


## ---- message=SHOW_MSG---------------------------------------------------
fit.samp25wtd <- ergm.ego(fmh.egosamp25wtd ~ edges + degree(0) 
                          + nodefactor("Race", levels= -6) + nodefactor("Sex") 
                          + nodematch("Race") + nodematch("Sex") + absdiff("Grade"),
                          popsize=N)


## ------------------------------------------------------------------------
# again we create the s.e. matrix first, using the original fit to define the rows/terms
se.coef.samp25wtd <- se.coef(fit.samp25wtd, src = "all")[-1]

se.samp25wtd <- tibble::rownames_to_column(data.frame(se.coef.samp25))  %>%
  left_join(tibble::rownames_to_column(data.frame(se.coef.samp25wtd))) %>%
  mutate(se.ratio = se.coef.samp25wtd/se.coef.samp25)
round_df(se.samp25wtd, 3)


## ---- message=SHOW_MSG---------------------------------------------------
# compute se for Zdiff(coef)
se.diff <- sqrt(se.coef.samp25^2 + se.coef.samp25wtd^2)

# compare the coef: fit to weighted samp
coefs4 <- tibble::rownames_to_column(data.frame(coef(fit.net)))%>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.samp25))))  %>%
  left_join(tibble::rownames_to_column(data.frame(coef(fit.samp25wtd)))) %>%
  mutate(Zdiff.25wtd.net = (coef.fit.samp25wtd. - coef.fit.net.)/se.diff) 

round_df(coefs4, 3)



## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ---- latentnet, include=FALSE------------------------------------------------------------------------
library(latentnet)


## ---- southernWomen, echo=TRUE, tidy=FALSE------------------------------------------------------------
require(latentnet) # Load latentent (which depends on ergm, network)
data(davis) # Load southern women dataset
davis


## ---- southernWomenPlot, echo=TRUE, tidy=FALSE, fig.cap="The bipartite network dataset known as southern women is part of the latentnet package", out.width=130----
set.seed(123)
plot(davis, displaylabels=TRUE, vertex.col = c(rep("Red", 18), rep("Green", 14)),
     vertex.sides = 4*c(rep(10, 18), rep(1, 14)), vertex.cex = c(rep(3, 18), rep(3, 14)), 
     label.cex = 0.4, label.pos = 5)


## ---- include=TRUE, echo=TRUE-------------------------------------------------------------------------
davis_A <- as.matrix.network.adjacency(davis)
davis_A


## ---- hypernetwork, echo=TRUE-------------------------------------------------------------------------
# Obtain women's names from network using %v% operator
names <- (davis %v% "vertex.names")[1:18]
# Create list where each item is an "edge"; look at first 5 of them
apply(davis_A, 2, function(event) names[event==1])[1:5]


## ---- ErdosRenyi, echo=TRUE, message=FALSE------------------------------------------------------------
# Full coefficient information for Erdos-Renyi fit:
coef(summary(ergm(davis ~ edges)))


## ---- LogOdds, echo=TRUE------------------------------------------------------------------------------
# log-odds of p/n equals log(p / (n-p))
log(89 / (252 - 89))


## ---- sociality summary, echo=TRUE--------------------------------------------------------------------
# summary with an ergm formula reports the network's observed statistics
summary(davis ~ sociality(nodes = 1:18))


## ---- sociality model, echo=TRUE, message=FALSE-------------------------------------------------------
ergm(davis ~ sociality(nodes = 1:18))$coef


## ---- include=TRUE, echo=TRUE, tidy=FALSE, fig.cap="Projection of the southern women bipartite network onto the nodes representing women", out.width=130----
P <- davis_A %*% t(davis_A)
women <- as.network(P, directed=FALSE)
set.seed(123)
plot(women, displaylabels=TRUE, vertex.col = c(rep("Red", 18)),
     vertex.cex = c(rep(2.5, 18)), label.cex = 0.3, label.pos = 5)


## ---- include=TRUE, echo=TRUE, tidy=FALSE, fig.cap="Projection of the southern women bipartite network onto the nodes representing events", out.width=130----
G <- t(davis_A) %*% davis_A
events <- as.network(G, directed=FALSE)
set.seed(456)
plot(events, displaylabels=TRUE, vertex.col = c(rep("Green", 14)), vertex.cex = c(rep(4, 14)), 
     label.cex = 0.4, label.pos = 5, vertex.sides = 4 * c(rep(1, 14)))


## ---- include=TRUE, echo=TRUE, tidy=FALSE, fig.cap="Weighted projection of the southern women bipartite network onto the nodes representing women", out.width=125----
#list of the edges in the women one-mode projection
edges_women <- as.edgelist(women) 
#retrieve the weights from the projection matrix 
weights_women <- apply(edges_women, 1, function(row) P[row[1],row[2]])
#plot the network with the edges labelled by the weights
set.seed(123)
plot(women, displaylabels=TRUE, vertex.col = c(rep("Red", 18)), vertex.cex = c(rep(4, 18)), 
label.cex = 0.4, label.pos = 5, edge.label = weights_women)


## ---- include=TRUE, echo=TRUE, tidy=FALSE, fig.cap="Weighted projection of the southern women bipartite network onto the nodes representing women", out.width=130, warning=FALSE----
#list of the edges in the events one-mode projections
edges_events <- as.edgelist(events)
#retrieve the weights from the projection matrix 
weights_events <- apply(edges_events, 1, function(row) G[row[1],row[2]])
#plot the network with the edges labelled by the weights
set.seed(456)
plot(events, displaylabels=TRUE, vertex.col = c(rep("Green", 14)), vertex.cex = c(rep(4, 14)), 
     label.cex = 0.4, label.pos = 5, vertex.sides = 4*c(rep(1, 14)), edge.label = weights_events, 
     edge.col = "Red")


## ---- DDTermsPlot, echo=TRUE, tidy=FALSE, fig.cap="A 3-b2star (left), a 4-cycle (right), and a 2-b2star (bottom)", out.width=60----
# Define a subset of nodes that contains a 3-star, a 4-cycle, and a 2-star.  Then plot.
subset <- c(1:2, 11:13, 17:20, 28:29)
set.seed(4) # plot.network uses randomness in placing nodes, so set.seed will keep same arrangement
plot(davis %s% subset, displaylabels=TRUE, vertex.col = c(rep("Red", 7), rep("Green", 4)),
     vertex.sides = 4*c(rep(10, 7), rep(1, 4)), vertex.cex = c(rep(3, 7), rep(3, 4)), 
     label.cex = 2, label.pos = 1)


## ---- DDterms, echo=TRUE, tidy=FALSE, message=FALSE---------------------------------------------------
summary(davis ~ b2star(3) + cycle(4) + b2star(2) + b1star(2))


## ---- DDterms2, echo=TRUE, tidy=FALSE, message=FALSE--------------------------------------------------
# Summing below- (or above-)diagonal entries of a projection adjacency matrix gives 2-star counts 
c(sum(P[lower.tri(P)]), sum(G[lower.tri(G)])) 


## ---- b2starModel, echo=TRUE, tidy=FALSE, message=FALSE, cache=TRUE-----------------------------------
fit1 <- ergm(davis ~ sociality(nodes = 1:18) + b2star(3))
tail(coefficients(summary(fit1)))


## ---- echo=TRUE, tidy=FALSE, cache=TRUE---------------------------------------------------------------
#load the raw data from www.stat.cmu.edu
magact96 = read.delim(
  "http://www.stat.cmu.edu/~brian//780/stanford%20social%20network%20labs/00%20data/mag_act96.txt", 
                      na.strings = "na", check.names = FALSE)
#save the attribute data (ID #, gender, grade, race)
magattrib = magact96[,1:4]
head(magattrib)[1:5,]
#create adjacency matrix with students as rows and clubs as columns
g96 <- as.matrix(magact96[,-(1:4)]); row.names(g96) = magact96[,1]
head(g96)[1:5,1:5]


## ---- echo=TRUE, tidy=FALSE, cache=TRUE---------------------------------------------------------------
#create m96 object
g96a <- g96

#remove missing data 
NArows <- which(is.na(magattrib[,2])) #remove NA missing values
NArows <- union(NArows, which(is.na(magattrib[,3])))
NArows <- union(NArows, which(is.na(magattrib[,4])))
NArows <- union(NArows, which(magattrib[,2]=='.')) #remove "." missing values
NArows <- union(NArows, which(magattrib[,3]=='.'))
NArows <- union(NArows, which(magattrib[,4]=='.'))
NArows <- union(NArows, which(magattrib[,2]=='')) #remove " " missing values
NArows <- union(NArows, which(magattrib[,3]==''))
NArows <- union(NArows, which(magattrib[,4]==''))
NArows <- union(NArows, which(magattrib[,2]=='3')) #remove observations with a gender value of 3

g96a <- g96[-NArows,] #remove missing data from the adjacency matrix 
gendera<- as.numeric(as.character(magattrib[-NArows,2])) #create gender attribute  
gradea<- as.numeric(as.character(magattrib[-NArows,3])) #create grade attribute 
racea<- as.numeric(as.character(magattrib[-NArows,4])) #create race attribute 

#create network object
Schoolnw <- network(g96a, undirected=T, bipartite=1262) 
Schoolnw %v% 'gender' <- c(gendera, rep(NA,91))
Schoolnw %v% 'grade' <- c(gradea, rep(NA,91))
Schoolnw %v% 'race' <- c(racea, rep(NA,91))


## ---- echo=TRUE, tidy=FALSE---------------------------------------------------------------------------
Schoolnw


## ---- schoolclubsPlot, echo=TRUE, tidy=FALSE, fig.cap="Students (left) who join extracurricular clubs (right)", out.width=100----
set.seed(123)
a <- structure(c(sample(c(1:100), 1262, replace=TRUE),rep(300, 91), 
                 sample(c(1:364), 1262, replace=TRUE), seq(to = 1, from = 364, by = -4)), 
               .Dim = c(1353L, 2L), .Dimnames = list(NULL, c("cx", "cy")))
node_colors <- c((Schoolnw %v% "grade")[1:1262] - 5 , rep("Green1", 91))
plot.network(Schoolnw, displayisolates = FALSE, coord=a, edge.col="gray", 
             vertex.cex = 0.5, vertex.col = node_colors, 
             vertex.sides = c((Schoolnw %v% "gender")[1:1262]*16-12, rep(4, 91)))


## ---- MainEffects, echo=TRUE, message=FALSE-----------------------------------------------------------
coefficients(summary(ergm(Schoolnw ~ edges + b1factor("gender") + b1cov("grade"))))


## ---- nodematch, echo=TRUE, message=FALSE-------------------------------------------------------------
data(faux.mesa.high)
coefficients(summary(ergm(faux.mesa.high ~ nodematch("Grade", diff=TRUE))))


## ---- twopaths, echo=TRUE, out.width=50, fig.cap="Red nodes are either solid or dashed. There are 4 two-paths connecting matching red nodes."----
edglst <- cbind(c(1, 1, 2, 3, 3, 4, 5), c(6, 7, 6, 7, 6, 7, 6))
smnw <- as.network(edglst, bipartite = 5, directed = FALSE)
mode <- rep(1:2, c(5, 2))
coord <- cbind( x =14 * mode - 13, y = c(29, 23, 17, 11, 5, 23, 11))
plot(smnw, coord = coord, jitter = FALSE, label = c(letters[1:5], 1:2), label.cex = 1.2, 
     label.pos = 5, vertex.col = mode + 1, vertex.cex = 6, vertex.sides = 44 - 20 * mode,
     edge.lty = 2, vertex.lwd = 6, vertex.lty = c(1, 1, 1, 3, 3, 1, 1), ylim=c(1, 33))


## ---- b1nodematchsummary, message=FALSE, cache=TRUE, echo=TRUE, tidy=FALSE----------------------------
summary(Schoolnw ~ b1nodematch("grade", diff=TRUE))
summary(Schoolnw ~ b1nodematch("grade", alpha=0)) 
summary(Schoolnw ~ b1nodematch("grade", beta=0))


## ---- b1nodematchfit, message=FALSE, cache=TRUE, echo=TRUE--------------------------------------------
coefficients(summary(ergm(Schoolnw ~ edges + b1factor("grade") + b1nodematch("grade", beta=0))))


## ---- MiniSeattle, echo=TRUE--------------------------------------------------------------------------
load("Snw.RData") ; print(Snw)


## ---- MiniSeattlePlot, warning=FALSE, out.width=150, fig.cap="Bipartite network where all nodes are people "----
sexIDval <- as.integer(as.factor(Snw %v% "sex.ident"))
plot(Snw, vertex.col = 1+sexIDval, vertex.sides = 1+sexIDval+12*(sexIDval==1))
legend("topleft", legend = c("F", "MSF", "MSMF"), fill=2:4, pch=c(1, 2, 5))


## ---- monogamous, message=FALSE, warning=FALSE, echo=TRUE, cache=TRUE---------------------------------
fit2 <- ergm(Snw ~ edges + nodematch("race"),
             constraints = ~bd(maxout = 1) + blocks(attr = ~sex, levels2 = diag(TRUE, 2)))
coefficients(summary(fit2))


## ---- monogamous2, message=FALSE, warning=FALSE, echo=TRUE, cache=TRUE--------------------------------
fit3 <- ergm(Snw ~ edges + nodematch("race") + 
                     offset(concurrent) + offset(nodematch("sex")), offset.coef = c(-Inf, -Inf))
coefficients(summary(fit3))
coefficients(summary(fit2)) # This is the previous fit 


## ---- monogamous3, message=FALSE, warning=FALSE, echo=TRUE--------------------------------------------
fit4 <- ergm(Snw ~ edges + nodematch("race"),
             constraints = ~blocks(attr = ~sex, levels2 = diag(TRUE, 2)))
coefficients(summary(fit4))


## ---- monogamous3AltCalc, echo=TRUE-------------------------------------------------------------------
table(Snw %v% "sex", Snw %v% "race")
summary(Snw ~ edges + nodematch("race"))


## ---- southernWomenPlotRevisited, echo=FALSE, out.width=130-------------------------------------------
set.seed(123)
plot(davis, displaylabels=TRUE, vertex.col = c(rep("Red", 18), rep("Green", 14)),
     vertex.sides = 4*c(rep(10, 18), rep(1, 14)), vertex.cex = c(rep(3, 18), rep(3, 14)), 
     label.cex = 0.4, label.pos = 5)


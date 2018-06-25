## 1 Getting Started
## if you haven't installed the relevant packages, do so with the following code:
## install.packages("network")
## install.packages("statnet")
## install.packages("sna")
## install.packages("numderiv")

###########################################################
###########################################################
##2 A Brief R Tutorial
##2.1 Introduction to basic R syntax
1 + 3 # evaluation
a <- 3 # assignment
a # evaluation
a<-3 # spacing does not matter
a <- 3 # spacing does not matter
a

sqrt(a) # use the square root function
b <- sqrt(a) # use function and save result
b

d # evaluate something that is not there
a == b # is a equivalent to b?
a != b # is a not equal to b?
ls() # list objects in the global environment
help.start() # get help with R generally
?sqrt # get specific help for a function
??sqrt # looking for help pertaining to sqrt
apropos("sq") # it's on the tip of my tongue...
rm(a) # remove a single object
ls()
rm(list=ls()) # remove everything from the environment
ls()

###########################################################
##2.2 Vectors and matrices in R
#Creating vectors using the "combine" operator
c
?c
a <- c(1,3,5) # create a vector by combining values
a
a[2] # select the second element
b <- c("one","three","five") # also works with strings
b
b[2]
a <- c(a,a) # can apply recursively
a
a <- c(a,b) # mixing types---what happens?
a # all converted to the same type

#Sequences and replication
a <- seq(from=1,to=5,by=1) # from 1 to 5 the slow way
b <- 1:5 # a shortcut!
a==b # all TRUE
rep(1,times=5) # a lot of ones
rep(1:5,times=2) # repeat an entire sequence
rep(1:5,each=2) # same, but element-wise
rep(1:5,times=5:1) # can vary the count of each element

#any, all, and which (with vectors)
a <- -1:4 # create a vector
a
a>2 # some TRUE, some FALSE
any(a>2) # are any elements TRUE?
all(a>2) # are all elements TRUE?
which(a>2) # which indices are TRUE?

#From vectors to matrices
a <- matrix(data=1:25, nrow=5, ncol=5) # create a matrix the "formal" way
a
a[1,2] # select a matrix element (two dimensions)
a[1,] # just the first row
all(a[1,]==a[1,1:5]) # show the equivalence
a[,2] # can also perform for columns
a[2:3,3:5] # select submatrices
a[-1,] # nice trick: negative numbers omit cells!
a[-2,-2] # get rid of row two, column two

b <- cbind(1:5,1:5) # another way to create matrices
b
d <- rbind(1:5,1:5) # can perform with rows, too
d
cbind(b,d) # no go: must have compatible dimensions!
dim(b) # what were those dimensions, anyway?
dim(d)
nrow(b)
ncol(b)
cbind(b,b) # combining two matrices

t(b) # can transpose b
cbind(t(b),d) # now it works
rbind(t(b),d) # now it works

###########################################################
##2.3 Element-wise operations
a <- 1:5
a + 1 # addition
a * 2 # multiplication
a / 3 # division
a - 4 # subtraction
a ^ 5 # you get the idea...

a + a # also works on pairs of vectors
a * a
a + 1:6 # problem: need same length

a <- rbind(1:5,2:6) # same principles apply to matrices
b <- rbind(3:7,4:8)
a + b
a / b
a %*% t(b) # matrix multiplication

#logical operators (generally) work like arithmetic ones
a > 0 # each value greater than zero?
a == b # corresponding values equivalent?
a != b # corresponding values not equivalent?
!(a == b) # same as above
(a>2) | (b>4) # the OR operator
(a>2) & (b>4) # the AND operator
(a>2) || (b>4) # beware the "double-pipe"!
(a>2) && (b>4) # (and the "double-ampersand"!)

#ditto for many other basic transformations
log(a)
exp(b)
sqrt(a+b) # note that we can nest statements!
log((sqrt(a+b)+a)*b) # as recursive as we wanna be

###########################################################
##2.4 Data Frames
d <- data.frame(income=1:5,sane=c(T,T,T,T,F),name=LETTERS[1:5])
d
d[1,2] # acts a lot like a matrix!
d[,1]*5
d[-1,]
d$sane # can use dollar sign notation
d$sane[3]<-FALSE # making changes
d
d[2,3] # shows factors for string values

#if you want to do without factors
d$name <- LETTERS[1:5] # eliminate evil factors by overwriting
d[2,3]
d <- data.frame(income=1:5,sane=c(T,T,T,T,F),name=LETTERS[1:5],stringsAsFactors=FALSE)
d

d <- as.data.frame(cbind(1:5,2:6)) # can create from matrices
d
is.data.frame(d) # how can we tell it's not a matrix?
is.matrix(d) # the truth comes out

###########################################################
##2.5 Finding built-in data sets
#Many packages have built-in data for testing and educational purposes
data() # lists them all
?USArrests # get help on a data set
data(USArrests) # load the data set
USArrests # view the object

##2.6 Elementary visualization
#R's workhorse is the "plot" command
plot(USArrests$Murder,USArrests$UrbanPop) # using dollar sign notation
plot(USArrests$Murder,USArrests$UrbanPop,log="xy") # log-log scale

#Adding plot title and axis labels
plot(USArrests$Murder,USArrests$Assault,xlab="Murder",ylab="Assault",main="USArrests")

#Can also add text
plot(USArrests$Murder,USArrests$Assault,xlab="Murder",ylab="Assault", main="USArrests",type="n")
text(USArrests$Murder,USArrests$Assault,rownames(USArrests))

#Histograms and boxplots are often helpful
hist(USArrests$Murder)
boxplot(USArrests)

###########################################################
###########################################################
##3 network objects: importing, exploring, and manipulating network data

##3.1 Importing Relational Data
#Be sure to be in the directory where you stored the data for the workshop. If you've not set the working directory, you must do so now. See Section 1.7 for how to do this.
#You can use setwd() to change it, or platform specific shortcuts
getwd() # Check what directory you're in
list.files() # Check what's in the working directory

#Read an adjacency matrix (R stores it as a data frame by default)
relations <- read.csv("relationalData.csv",header=FALSE,stringsAsFactors=FALSE)
relations
#Here's a case where matrix format is preferred
relations <- as.matrix(relations) # convert to matrix format

#Read in some vertex attribute data (okay to leave it as a data frame)
nodeInfo <- read.csv("vertexAttributes.csv",header=TRUE,stringsAsFactors=FALSE)
nodeInfo

#Since our relational data has no row/column names, let's set them now
rownames(relations) <- nodeInfo$name
colnames(relations) <- nodeInfo$name
relations

#Why did we set stringsAsFactors=FALSE?
relations_wFactors<-read.csv("vertexAttributes.csv",header=T,stringsAsFactors=TRUE)
relations_wFactors[1,2]<-"Derek"

numFactor<-as.factor(c(1,3,5))
numFactor+1
as.numeric(numFactor)

#For more information. . .
?list.files
?read.csv
?as.matrix
?rownames

###########################################################
##3.2 Creating network objects, working with edgelists

nrelations<-network(relations,directed=FALSE) # Create a network object based on relations
nrelations # Get a quick description of the data
nempty <- network.initialize(5) # Create an empty graph with 5 vertices
nempty # Compare with nrelations

edgelist<-read.csv("edgeList.csv",header=T,stringsAsFactors=F)
head(edgelist)

nedge<-network(edgelist,matrix.type="edgelist")
nedge
nedge[,]
plot(nedge,displaylabels=T)

nedge<-network(edgelist,matrix.type="edgelist",ignore.eval=F,names.eval="weight")
nedge
nedge[,]
as.matrix(nedge,"weight")

plot(nedge,displaylabels=T,edge.lwd=as.sociomatrix(nedge,"weight")*5)

#For more information. . .
?network 
?as.network.matrix

###########################################################
##3.3 Description and Visualization

summary(nrelations) # Get an overall summary
network.dyadcount(nrelations) # How many dyads in nflo?
network.edgecount(nrelations) # How many edges are present?
network.size(nrelations) # How large is the network?
as.sociomatrix(nrelations) # Show it as a sociomatrix
nrelations[,] # Another way to do it

plot(nrelations,displaylabels=T) # Plot with names
plot(nrelations,displaylabels=T,mode="circle") # A less useful layout...

library(sna) # Load the sna library
gplot(nrelations) # Requires sna
gplot(relations) # gplot Will work with a matrix object too

#For more information. . .
?summary.network
?network.dyadcount
?network.edgecount
?as.sociomatrix
?as.matrix.network
?is.directed

###########################################################
##3.4 Working With Edges
#Adding Edges
g <- network.initialize(5) # Create an empty graph
g[1,2] <- 1 # Add an edge from 1 to 2
g[2,] <- 1 # Add edges from 2 to everyone else
g # Examine the result
m <- matrix(0, nrow=5, ncol=5) # Create an adjacency matrix
m[3,4:5] <- 1 # Add entries from 3 to 4 and 5
g[m>0] <- 1 # Add more entries
g

#Delete edges
g[3,5] <- 0 # Remove the edge from 3 to 5
g # Its gone!
g[,] <- 0 # Remove all edges
g # Now, an empty graph

#Testing adjacency
nrelations[4,5] # Emma to Sarah?
nrelations[4,] # Entire Emma row
nrelations[1:4,5:8] # Subsets are possible
nrelations[-9,-9] # Leaving Gil out of the picture

#Setting edge values
m<-nrelations[,] # copy over the relational structure
m[upper.tri(m)>0]<-rep(1:3,times=3) # give different edge values
m<-symmetrize(m,rule="upper")
m

nrelations %e% "strength" <- m # Add the valued ties back in

#Retrieving edge values
list.edge.attributes(nrelations) # See whats available
nrelations %e% "strength" # Use the %e% operator
as.sociomatrix(nrelations,attrname="strength") # Can also do it this way
nrelations[,] # Original tie structure is preserved

#For more information. . .
?network.extraction
?add.edge
?delete.edges
?delete.vertices
?get.edges
?upper.tri

###########################################################
##3.5 Network and Vertex Attributes 
#Add some attributes
nrelations %v% "id" <- nodeInfo$id # Add in our vertex attributes
nrelations %v% "age" <- nodeInfo$age
nrelations %v% "sex" <- nodeInfo$sex
nrelations %v% "handed" <- nodeInfo$handed
nrelations %v% "lastDocVisit" <- nodeInfo$lastDocVisit

#Listing attributes
list.vertex.attributes(nrelations) # List all vertex attributes
list.network.attributes(nrelations) # List all network attributes

#Retrieving attributes
nrelations %v% "age" # Retrieve vertex ages
nrelations %v% "id" # Retrieve vertex ids

#For more information. . .
?attribute.methods

###########################################################
###########################################################
##4 Classical Network Analysis with the SNA package
##4.1 Getting started
library(sna) # Load the sna library
library(help="sna") # See a list of help pages for sna package
load("IntroToSNAinR.Rdata") # Load supplemental workshop data

###########################################################
##4.2 Network data in SNA
#The sna package can handle network data in many forms. For instance, the function gden() calculates network density; we can use it on a network object, an adjacency matrix, a list of such matrices, etc.

data(flo)
flo # Adjacency form
gden(flo)
nflo<-network(flo,directed=FALSE) # Network form
gden(nflo)

#The sna package also supports a special kind of matrix called an \sna edgelist." These are three-column matrices, each row of which represents an edge (via its sender, recipient, and value, respectively). These sna edgelists" have special attributes that indicate their size, vertex names (if any), and bipartite status (if applicable).
eflo<-as.edgelist.sna(flo) # Coerce flo to an sna edgelist
eflo
attr(eflo,"n") # How many vertices are there?
attr(eflo,"vnames") # Are there vertex names?
as.sociomatrix.sna(eflo) # Can transform back w/ as.sociomatrix.sna 

#For more information. . .
?as.edgelist.sna
?as.sociomatrix.sna
?attr
?sna

###########################################################
##4.3 Network visualization with gplot()
#Plotting the data we imported earlier
gplot(nrelations)
gplot(nrelations,displaylabels=TRUE) # This time with labels
#Let's color the nodes in sex-stereotypic colors
nodeColors<-ifelse(nodeInfo$sex=="F","hotpink","dodgerblue")
gplot(relations,gmode="graph",displaylabels=TRUE,vertex.col=nodeColors)

#Using data we just loaded in, plot the contiguity among nations in 1993 (from the Correlates of War (CoW)1 project)
gplot(contig_1993) # The default visualization
gplot(contig_1993, usearrows=FALSE) # Turn off arrows manually
gplot(contig_1993, gmode="graph") # Can also tell gplot the data is undirected

#We can add labels to the vertices
gplot(contig_1993, gmode="graph",displaylabels=TRUE,label.cex=0.5,label.col="blue")

#Other ways to specify the labeling
gplot(contig_1993,gmode="graph",label=network.vertex.names(contig_1993),label.cex=0.5,label.col="blue")

#Here's an example of directed data|militarized interstate disputes (MIDs) for 1993
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE)

#All those isolates can get in the way|we can suppress them using displayisolates
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE,displayisolates=FALSE)

#The default layout algorithm is that of Frutchterman-Reingold (1991), can use others
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE,displayisolates=FALSE,mode="circle") # The infamous circle

#or perhaps
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE,displayisolates=FALSE,mode="mds") # MDS of position similarity

#When a layout is generated, the results can be saved for later reuse:
coords <- gplot(contig_1993,gmode="graph",label=colnames(contig_1993[,]),label.cex=0.5,label.col="blue") # Capture the magic of the moment
coords # Show the vertex coordinates

#Saved (or a priori) layouts can be used via the coord argument
gplot(mids_1993,gmode="graph",label=colnames(contig_1993[,]),label.cex=0.5,label.col="blue",coord=coords)

#When the default settings are insuficient, interactive mode allows for tweaking
coords <- gplot(contig_1993, interactive=TRUE) # Modify and save
gplot(contig_1993,coord=coords,displaylabels=TRUE,gmode="graph",label.cex=0.5,label.col="blue") # Should reproduce the modified layout

#For more information. . .
?gplot ?gplot.layout

###########################################################
##4.4 Basic centrality indices: degree, betweenness, and closeness
#We begin with the simplest case: degree centrality

#Component information can be obtained in various ways
components(mids_1993) # Strong component count
components(mids_1993, connected="weak") # Weak component count
cd <- component.dist(mids_1993, connected="weak") # Get weak components
cd

#Component sizes
plot(1:length(cd$cdist),cd$cdist,xlab="Size",ylab="Frequency")

#Who's in the largest component?
cl <- component.largest(mids_1993, connected="weak")
cl

#Plot the largest weak component
gplot(mids_1993[cl,cl], 
      boxed.lab=FALSE, 
      label.cex=0.5,
      label.col=4,
      label=network.vertex.names(mids_1993)[cl])

#Likewise, many routines exist for handling isolates
is.isolate(mids_1993, 3) # Is the third vertex (BHM) an isolate?
is.isolate(mids_1993, which(mids_1993%v%"vertex.names"=="BHM")) # The peaceful islands?
is.isolate(mids_1993, which(mids_1993%v%"vertex.names"=="USA")) # Perhaps less so....
isol <- isolates(mids_1993) # Get the entire list of isolates
isol
network.vertex.names(mids_1993)[isol] # Which countries are isolates?

#Another way to remove isolates from sociograms
gplot(mids_1993[-isol,-isol], 
      label.cex=0.5,
      label.col=4,
      label=network.vertex.names(mids_1993)[-isol])

#Geodesic distances
contig.dist<-geodist(contig_1993)

#look at the resulting object
attributes(contig.dist)
contig.dist$gdist
contig.dist$counts


###########################################################
##4.2 Basic centrality indices: degree, betweenness, and closeness
#We begin with the simplest case: degree centrality
degree(mids_1993) # Default: total degree
ideg <- degree(mids_1993, cmode="indegree") # Indegree for MIDs
odeg <- degree(mids_1993, cmode="outdegree") # Outdegree for MIDs
all(degree(mids_1993) == ideg+odeg) # In + out = total?

#Once centrality scores are computed, we can handle them using standard R methods:
plot(ideg, 
     odeg, 
     type="n", 
     xlab="Incoming MIDs", 
     ylab="Outgoing MIDs") # Plot ideg by odeg

abline(0, 1, lty=3)

text(jitter(ideg), 
     jitter(odeg), 
     network.vertex.names(contig_1993), 
     cex=0.75, 
     col=2)

#Plot simple histograms of the degree distribution:
par(mfrow=c(2,2)) # Set up a 2x2 display

hist(ideg, 
     xlab="Indegree",
     main="Indegree Distribution", 
     prob=TRUE)

hist(odeg, 
     xlab="Outdegree", 
     main="Outdegree Distribution", 
     prob=TRUE)

hist(ideg+odeg, 
     xlab="Total Degree", 
     main="Total Degree Distribution", 
     prob=TRUE)

par(mfrow=c(1,1)) # Restore display

#Centrality scores can also be used with other sna routines, e.g., gplot()
gplot(mids_1993, 
      vertex.cex=(ideg+odeg)^0.5/2, 
      vertex.sides=50,
      label.cex=0.4,
      vertex.col=rgb(odeg/max(odeg),0,ideg/max(ideg)),
      displaylabels=TRUE)

#Betweenness and closeness are also popular measures
bet <- betweenness(contig_1993, 
                   gmode="graph") # Geographic betweenness

bet

gplot(contig_1993, 
      vertex.cex=sqrt(bet)/25, 
      gmode="graph") # Use w/gplot

clo <- closeness(contig_1993) # Geographic closeness
clo # A large world after all?

#Can use sna routines to explore alternatives to the common measures. . .
contig_1993_gdist<-geodist(contig_1993)$gdist # matrix of geodesic distances

contig_1993_gdist
1/sum(contig_1993_gdist[1,2:186]) # calculate closeness for country 1

which(contig_1993_gdist[,1]=="Inf") # disconnected matrix

sum(1/contig_1993_gdist[1,2:186]) # alternate closeness for country 1

closeness2 <- function(x){ # Create an alternate closeness function!
  geo <- 1/geodist(x)$gdist # Get the matrix of 1/geodesic distance
  diag(geo) <- 0 # Define self-ties as 0
  apply(geo, 1, sum) # Return sum(1/geodist) for each vertex
}

clo2 <- closeness2(contig_1993) # Use our new function on contiguity data
clo2

hist(clo2, xlab="Alt. Closeness", prob=TRUE) # Much better behaved!
cor(clo2, bet) # Correlate with betweenness
plot(clo2, bet) # Plot the bivariate relationship
all(clo2/185==closeness(contig_1993,cmode="suminvundir")) # Actually, we support this in sna!

#For more information. . .
?betweenness
?bonpow
?closeness
?degree
?evcent
?graphcent
?infocent
?prestige
?stresscent

###########################################################
##4.3 From centrality to centralization
centralization(mids_1993, degree, cmode="indegree") # Do MIDs concentrate?
centralization(contig_1993, evcent) # Eigenvector centralization

#For more information. . .
?centralization
###########################################################
##4.4 Elementary graph-level indices
gden(mids_1993) # Density
grecip(mids_1993) # Dyadic reciprocity
grecip(mids_1993, measure="edgewise") # Edgewise reciprocity
gtrans(mids_1993) # Transitivity

#For more information. . .
?gden 
?grecip 
?gtrans

###########################################################
##4.5 Subgraph census routines, isolates, clusters, and geodesic distance

dyad.census(mids_1993) # M,A,N counts
dyad.census(contig_1993) # No As in undirected graphs
triad.census(mids_1993) # Directed triad census
triad.census(contig_1993, mode="graph") # Undirected triad census

###########################################################
##4.6 Brokerage

#look at the trade data
class(trade)
dim(trade)

gplot(trade[1,,])

tradeat
table(tradeat[,1])

gplot(trade[1,,],vertex.col=rainbow(5)[tradeat[,1]+1]) #color by pop growth
tradeBro<-brokerage(trade[1,,],tradeat[,1])
summary(tradeBro)

############################################################
#4.7 Matrix Algebra
davis

gplot(davis,label=c(rownames(davis),colnames(davis)),vertex.col=c(rep("red",18),rep("blue",14)))

nDavis<-network(davis,bipartite=T)
nDavis ##what does bipartite number indicate?

nDavis[,]  ##doesn't return the original bipartite adjacency matrix

##That nasty repeating vector thing -- beware when assigning vertex attributes for 2-mode networks
nDavis%v%"num"<-1:18
nDavis%v%"num"

##Back to matrix multiplication
t(davis) ##take the transpose
davis%*%t(davis)  ##here's a case where the function demands matrix format
davis<-as.matrix(davis)

women_coattendence<-davis%*%t(davis)  ##womenXwomen projection
diag(women_coattendence)  ## what does the diagonal mean now?

event_coattendence<-t(davis)%*%davis  ##eventXevent projection

gplot(women_coattendence,edge.lwd=women_coattendence)
gplot(event_coattendence,edge.lwd=event_coattendence)


#Geography and MDS
city
cityNames<-rownames(city)

city<-as.matrix(city)
city<-symmetrize(city,rule="upper")

XYCoords<-cmdscale(city)  ##this will 'throw' an error because of NAs
which(is.na(city),arr.ind=T) ##where are the NAs?
diag(city) ##another way to find them

diag(city)<-0 ##why is the diagonal zero?

XYCoords<-cmdscale(city)
plot(XYCoords,type="n")
text(XYCoords,label=cityNames)

#Correspondence analysis -- think two-mode MDS
library(MASS)
plot(corresp(davis,nf=2))

###########################################################
##4.8 Elementary random graph generation

rgraph(10) # A uniform random digraph of order 10
rgraph(10, tprob=3/9) # Homogeneous Bernoulli w/mean degree 3
rgraph(10, tprob=3/9, mode="graph") # As above, but undirected
rgnm(1, 10, 20) # Uniform conditional on order, edges
rguman(1, 10, mut=0.5, asym=0.25, null=0.25) # Homogeneous multinomial on dyad census
rguman(1, 10, mut=0, asym=1, null=0) # An extreme case: random tournament
gplot(rgws(1,50,1,2,0)) # A Watts-Strogatz process - baseline
gplot(rgws(1,50,1,2,0.05)) # ...with rewiring probability 0.05
gplot(rgws(1,50,1,2,0.2)) # ...with rewiring probability 0.2

##now let's re-create the plot from Watts and Strogatz
##a list is a new type of data structure...
wgNets<-list()
temp<-seq(-3.2,0,.2)
temp
length(temp)

p_vals<-10^(temp)
p_vals

for(i in 1:17){
  wgNets[[i]]<-rgws(n=1,nv=50,d=1,z=2,p=p_vals[i])
}

##lists use 'double-bracket' notation
gplot(wgNets[[1]])
gplot(wgNets[[17]])

l_vals<-vector(length=17)
geodist(wgNets[[1]])$gdist
geodist(wgNets[[17]])$gdist
for(i in 1:17){
  temp<-geodist(wgNets[[i]])$gdist
  temp[which(temp=="Inf")]<-0
  l_vals[i]<-max(temp)
}
l_vals
l_vals<-l_vals/l_vals[1]

##now for the clustering coefficient
triad.census(wgNets[[1]],mode="graph")
c_vals<-vector(length=17)
for(i in 1:17){
  temp<-triad.census(wgNets[[i]],mode="graph")
  c_vals[i]<-temp[4]/(3*temp[4]+temp[3])
}
c_vals
c_vals<-c_vals/c_vals[1]

plot(p_vals,
     l_vals,log="x",
     ylim=c(0,1),
     xlab="Re-Wiring Level",
     ylab="Fraction of Initial Value")

lines(p_vals,c_vals,type="p",pch=2)

##always add legends!
legend("bottomleft",legend=c("Diameter","Clustering Coefficient"),pch=c(1,2))

#For more information. . .
?rgbn
?rgmn
?rgraph
?rguman
?rgws
?list
?lines
?legend


###########################################################
###########################################################
##5 Moving Beyond Descriptives
##5.1 The basic permutation test


data(emon) # Load Drabek et al. data

# Begin by examining the 'emon' dataset
?emon
class(emon)
class(emon[[1]])
emon[[1]]
emon[[1]]%v%"vertex.names" # Display vertex names

# Extract ties from the Cheyenne EMON communicating at least "every few hours"
g<-as.sociomatrix(emon[[1]],"Frequency") # Need to get the frequency info
g<-symmetrize((g>0)&(g<4)) # Note the reverse coding!

#Get some potential covariates
drs<-emon[[1]]%v%"Decision.Rank.Score" # Get decision rank (see man page)
crs<-emon[[1]]%v%"Command.Rank.Score" # Get command rank

# Plot Cheyenne EMON communications
gplot(emon[[1]])
gplot(emon[[1]], label.cex=0.5, label.col=4, label=network.vertex.names(emon[[1]])) # Basic display, with labels

#Calculate some basic centrality measures
deg<-degree(g,gmode="graph")
bet<-betweenness(g,gmode="graph")
clo<-closeness(g,gmode="graph")

#Raw correlations
cor(cbind(deg,bet,clo),cbind(drs,crs))

#Classical tests (using asymptotic t distribution)
cor.test(deg,drs)
cor.test(bet,drs)
cor.test(clo,drs)

###########################################################
#5.2 Testing correlations
#Lets build a permutation test
deg
drs
c.obs<-cor(deg,drs)
c.obs

#Permute one of the data sets
sample(drs)
cor(deg,sample(drs))

#write a for loop to permute one of the data sets many times
c.rep<-vector(length=1000)
for(n in 1:1000){
  c.rep[n]<-cor(deg,sample(drs))
}

#look at what we've created
c.rep
hist(c.rep)

#compare to empirical data
abline(v=c.obs,col="red")

#put it all into a function!
perm.cor.test<-function(x,y,n=1000){ #Define a simple test function
  c.obs<-cor(x,y)
  c.rep<-vector()
  for(i in 1:n)
    c.rep[i]<-cor(x,sample(y))
  c.rep
}

#look at the results
new.c.rep<-perm.cor.test(deg,drs)
hist(new.c.rep)
abline(v=c.obs,col="red")

#calculate quantiles
mean(new.c.rep>=c.obs)
mean(new.c.rep<=c.obs)
mean(abs(new.c.rep)>=abs(c.obs))

#make the output display these quantiles
perm.cor.test<-function(x,y,niter=1000){ #Define a simple test function
  c.obs<-cor(x,y)
  c.rep<-vector()
  for(i in 1:niter)
    c.rep[i]<-cor(x,sample(y),use="complete.obs")
  cat("Vector Permutation Test:\n\tObserved correlation: ",c.obs,"\tReplicate quantiles
      (number of iterations = ",n,")\n",sep="")
  cat("\t\tPr(rho>=obs):",mean(c.rep>=c.obs),"\n")
  cat("\t\tPr(rho<=obs):",mean(c.rep<=c.obs),"\n")
  cat("\t\tPr(|rho|>=|obs|):",mean(abs(c.rep)>=abs(c.obs)),"\n")
  invisible(list(obs=c.obs,rep=c.rep))
}

#examine the fancy output
fancy<-perm.cor.test(deg,drs)
attributes(fancy)
fancy$obs
fancy$rep

#For more information....
?cor.test
?t.test
?sample

###########################################################
#5.3 Using NLIs as regression covariates

pstaff<-emon[[1]]%v%"Paid.Staff" # Get more EMON covariates
vstaff<-emon[[1]]%v%"Volunteer.Staff"
govt<-((emon[[1]]%v%"Sponsorship")!="Private")

#Simple model: decision rank is linear in size, degree, and govt status
mod<-lm(drs~deg+pstaff+vstaff+govt)
summary(mod)
anova(mod) #Some useful lm tools
AIC(mod)

#Try with alternative measures....
mod2<-lm(drs~bet+pstaff+vstaff+govt) #Betweenness
summary(mod2)
mod3<-lm(drs~clo+pstaff+vstaff+govt) #Closeness
summary(mod3)
AIC(mod,mod2,mod3) #Closeness wins!

#For more information....
?lm
?anova
?AIC

###########################################################
#5.4 Graph correlation and bivariate QAP
# Remember the Florentine families data
data(florentine)
gplot(flobusiness) # Examine business ties
gplot(flomarriage) # Examine marriage ties

# Could the two be related?
par(mfrow=c(1,2))
coords<-plot(flobusiness,label=flobusiness%v%"vertex.names",label.cex=.5,pad=1)
title("Business Ties")
plot(flomarriage,label=flomarriage%v%"vertex.names",label.cex=.5,pad=1,coord=coords)
title("Marriage Ties")
par(mfrow=c(1,1))

# Let's try a graph correlation
gcor(flobusiness,flomarriage)

# Why can't we use our previous permutation test?
# instead, use rmperm
# take a look
rmperm

par(mfrow=c(1,2))
gplot(flobusiness[,],label=flobusiness%v%"vertex.names",label.cex=.5,pad=1,coord=coords)
title("Business Ties")
gplot(rmperm(flobusiness),label=flobusiness%v%"vertex.names",label.cex=.5,pad=1,coord=coords)
title("Permuted Business Ties")
par(mfrow=c(1,1))

#now look at qaptest
qaptest

#and use it
qt<-qaptest(list(flobusiness,flomarriage),gcor,g1=1,g2=2)
summary(qt) # Examine the results
plot(qt) # Plot the QAP distribution

# Testing against covariate effects
wealth<-sapply(flomarriage%v%"wealth",rep,network.size(flomarriage))
wealth
wealthdiff<-abs(outer(flomarriage%v%"wealth",flomarriage%v%"wealth","-"))
wealthdiff
qt1<-qaptest(list(flomarriage,wealth),gcor,g1=1,g2=2)
qt2<-qaptest(list(flomarriage,wealthdiff),gcor,g1=1,g2=2)
summary(qt1) # Do wealthy families have more ties?
summary(qt2) # Is there a wealth difference effect?

# For more information....
?qaptest
?gcor
?outer
?sapply
?rep

###########################################################
#5.5 Network Regression

# Combine the previous tests (takes a while to perform QAP test)
marriagefit<-netlm(flomarriage,list(flobusiness,wealth,wealthdiff))
summary(marriagefit) # Examine the results

# Another example: we begin by preparing the response variable. We will use the Cheyenne
# EMON in valued form, but need to recode the frequency data
Y<-as.sociomatrix(emon[[1]], "Frequency") # Extract frequencies
Y[Y>0]<-5-Y[Y>0] # Now, higher -> more frequent

# Extract some vertex attributes
crk<-emon[[1]]%v% "Command.Rank.Score" # Command rank
spon<-emon[[1]]%v%"Sponsorship" # Type of organization

# Form some predictor matrices (X variables)
Xcr<-sapply(crk,rep,length(crk)) # Column effects for command rank
Xcr
Xsp<-outer(spon,spon,"!=") # Dyadic effect for type difference
Xsp

# Fit the model (takes a while to perform QAP test)
cmfit<-netlm(Y,list(Xcr,Xsp))
summary(cmfit) # Examine the results


cmfitB<-netlm(Y,list(Xcr,Xsp),nullhyp="classical")
summary(cmfitB) # In this example, pretty similar

# For more information....
?outer
?netlm

###########################################################
#5.6 Network Autocorrelation Models

rownames(parishNet)
which(parishNet["Rapides",]==1)

#Examine the OLS regression predicting percentage vote for Obama by percentage Black residents and average weekly wage

modLM<-lm(parishAttr[,1]~parishAttr[,2]+parishAttr[,3])
summary(modLM)

#create some simple weight matrices
wMat<-apply(parishNet,1,function(x){x/sum(x)})
wRandMat<-rgraph(64)

#Run the different models with the empirical weights and look at the results 

modAR<-lnam(parishAttr[,1],x=parishAttr[,2:3],W1=wMat)
summary(modAR)

modMA<-lnam(parishAttr[,1],x=parishAttr[,2:3],W2=wMat)
summary(modMA)

modARMA<-lnam(parishAttr[,1],x=parishAttr[,2:3],W1=wMat,W2=wMat)
summary(modARMA)

modRandAR<-lnam(parishAttr[,1],x=parishAttr[,2:3],W1=wRandMat)
summary(modRandAR)

modWIntercept<-lnam(parishAttr[,1],x=cbind(1,parishAttr[,2:3]),W1=wMat)

###########################################################
###########################################################
##6 Baseline Models

class(ctrade)
dim(ctrade)
isSymmetric(ctrade)

class(trade)
dim(trade)
which(trade[2,,]!=ctrade)

class(tradeat)
head(tradeat)

#Simple univariate conditional uniform graph tests
# The cug.test function provides a simple interface for univariate CUG tests.
# Let's try testing some data on trade in complex manufactured goods to see if overall
# activity (density) is greater then would be expected from size alone.
?cug.test
cug.test(ctrade,gden) # Call cug.test with the gden (density) function

# Is there more reciprocity than density would suggest? Let's see.
cug.test(ctrade,grecip,cmode="edges") # Conditioning on edges, calling grecip

# Given biases in density and reciprocity, we might look to see if there is a
# bias in transitivity, too. This time, let's condition on all of the above.
cug.test(ctrade,gtrans,cmode="dyad") # Conditioning on dyad census

# We are not limited to simple commands. Let's try indegree centralization:
ct<-cug.test(ctrade,centralization,cmode="dyad",FUN.arg=list(FUN=degree, cmode="indegree")) 
# Note that we here must pass not only arguments to
# cug.test, but also to centralization and degree!

ct # Print the result
attributes(ct) # lots o info!
ct$obs.stat
ct$reps.stat
plot(ct) # Can also plot it!

###############################
##6.1 Using CUG to produce reference distributions

mod1<-netlm(ctrade,list(trade[1,,],trade[3,,],trade[4,,],trade[5,,]),nullhyp = "classical")

mod2<-netlm(ctrade,list(trade[1,,],trade[3,,],trade[4,,],trade[5,,]),nullhyp = "qap")

mod3<-netlm(ctrade,list(trade[1,,],trade[3,,],trade[4,,],trade[5,,],gnp_receiver,gnp_sender,gnp_diff),nullhyp = "cugden")
###########################################################
#7 Studying Qualitative Dynamics with Network MDS
# Johnson collected 'liking' scores for researchers working in the South Pole on a monthly basis over two years
class(johnsonPolarY1)
dim(johnsonPolarY1)
johnsonPolarY1[1,,]

# Visualize the data using weight or color to aid interpretation
gplot(johnsonPolarY1)
gplot(johnsonPolarY1[1,,],edge.lwd=johnsonPolarY1[1,,])
coords<-gplot(johnsonPolarY1[1,,],edge.col=rainbow(10)[johnsonPolarY1[1,,]], vertex.col="black")
par(ask=TRUE)
for(i in 1:8)
  gplot(johnsonPolarY1[i,,],edge.col=rainbow(10)[johnsonPolarY1[i,,]],
        vertex.col="black",coord=coords)
par(ask=FALSE)

# Calculate Hamming distances for each of the two Johnson data sets
jp1d <- hdist(johnsonPolarY1)
jp2d <- hdist(johnsonPolarY2)

# Now try an MDS solution
jp1mds<-cmdscale(jp1d)
jp1mds
plot(jp1mds,type="l",lty=2) # Plot the results
text(jp1mds,label=rownames(johnsonPolarY1),font=2)
jp2mds<-cmdscale(jp2d)
jp2mds
plot(jp2mds,type="l",lty=2) # Plot the results
text(jp2mds,label=rownames(johnsonPolarY2),font=2)



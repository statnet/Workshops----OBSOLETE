## ----setup, include=FALSE, cache = FALSE--------------------------------------
library(knitr)
requireNamespace("sessioninfo")
requireNamespace("network")
requireNamespace("sna")
requireNamespace("igraph")
requireNamespace("tidygraph")
requireNamespace("ggraph")
requireNamespace("graph")
requireNamespace("htmltools")

# remotes::install_github("mbojan/isnar")
requireNamespace("isnar")


knitr::opts_chunk$set(
  cache = FALSE, 
  comment = NA
)


## ----install-packages, eval=FALSE---------------------------------------------
## install.packages(c("network", "sna", "igraph", "tidygraph", "ggraph",
##                    "intergraph", "remotes"))


## ----install-graph, eval=FALSE------------------------------------------------
## remotes::install_bioc("graph")


## ----install-statnet, eval=FALSE----------------------------------------------
## install.packages('statnet')


## ----data-classroom, include=FALSE--------------------------------------------
# Create three CSV data files based on isnar::IBE121 that are used in the
# examples.
dl <- igraph::as_data_frame(isnar::IBE121, what = "both")
write.csv(
  subset(dl$vertices, select = -wraven),
  file="classroom-nodes.csv", row.names=FALSE)
de <- subset(dl$edges, question == "play", select = -question)
set.seed(666)
de$liking <- sample(1:5, nrow(de), replace=TRUE)
write.csv(de, file="classroom-edges.csv", row.names=FALSE)
# Adjacency matrix
write.csv(
  igraph::as_adjacency_matrix(
    igraph::delete_edges(isnar::IBE121, igraph::E(isnar::IBE121)[question != "play"]),
    sparse = FALSE
  ),
  file = "classroom-adjacency.csv"
)
rm(dl, de) # Clean-up


## ----data-make-a-zip, include=FALSE-------------------------------------------
# Create a ZIP file with all data files
zip(
  "intro-sna-data",
  c("classroom-edges.csv", "classroom-nodes.csv", "classroom-adjacency.csv",
    "introToSNAinR.Rdata")
)


## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics("rstudio-wd.png")


## ---- eval=FALSE--------------------------------------------------------------
## setwd("path/to/folder/with/workshop/files")


## -----------------------------------------------------------------------------
getwd() # Check what directory you're in
list.files() # Check what's in the working directory


## ----conflicts, echo = FALSE--------------------------------------------------
pkgs <- c("network", "sna", "igraph", "tidygraph")
o <- lapply(pkgs, function(p) ls(envir = asNamespace(p)))
names(o) <- pkgs
d <- data.frame(
  name = unique(unlist(o))
)
d$network <- d$name %in% o$network
d$sna <- d$name %in% o$sna
d$igraph <- d$name %in% o$igraph
d$tidygraph <- d$name %in% o$tidygraph
# subset(
#   with(d, as.data.frame(table(network, sna, igraph, tidygraph))),
#   Freq > 0
# )


## ----conflicts-network-igraph, echo=FALSE-------------------------------------
with(d, name[network & igraph])


## ----conflicts-sna-igraph, echo=FALSE-----------------------------------------
with(d, name[sna & igraph])


## ----conflicts-cleanup, include=FALSE-----------------------------------------
rm(d, o, pkgs)


## ---- cache=FALSE-------------------------------------------------------------
library(network)
library(sna)


## -----------------------------------------------------------------------------
relations <- read.csv("classroom-adjacency.csv",header=T,row.names=1,stringsAsFactors=FALSE)
relations[1:10,1:10] #look at a subgraph using bracket notation


## -----------------------------------------------------------------------------
relations <- as.matrix(relations) # convert to matrix format
isSymmetric(relations)


## -----------------------------------------------------------------------------
colnames(relations) <- rownames(relations)


## -----------------------------------------------------------------------------
nodeInfo <- read.csv("classroom-nodes.csv",header=TRUE,stringsAsFactors=FALSE)
head(nodeInfo)


## -----------------------------------------------------------------------------
nrelations <- network(relations)
summary(nrelations) # Get an overall summary


## -----------------------------------------------------------------------------
list.vertex.attributes(nrelations)
nrelations%v%"vertex.names"


## -----------------------------------------------------------------------------
nrelations%v%"vertex.names" <- nodeInfo$name
nrelations%v%"vertex.names"


## -----------------------------------------------------------------------------
edgelist<-read.csv("classroom-edges.csv",header=T,stringsAsFactors = F)
head(edgelist)
edgeNet<-network(edgelist,matrix.type="edgelist")
edgeNet


## -----------------------------------------------------------------------------
edgeNet[,] ##what's missing?


## -----------------------------------------------------------------------------
list.edge.attributes(edgeNet)
edgeNet %e% "liking"
as.sociomatrix.sna(edgeNet, "liking")


## ---- cache=FALSE-------------------------------------------------------------
# Detaching the packages
detach(package:sna)
detach(package:network)


## ---- cache=FALSE-------------------------------------------------------------
library(igraph)


## ----igraph-makegraph1--------------------------------------------------------
make_graph( c(1,2, 2,3, 3,4), directed=FALSE)


## ----igraph-makegraph2--------------------------------------------------------
g1 <- make_graph(~ A - B, B - C:D:E)
g2 <- make_graph(~ A --+ B, B +-- C, A --+ D:E, B --+ A)
g2


## ----igraph-adjacency---------------------------------------------------------
graph_from_adjacency_matrix(relations, mode="directed")


## ----igraph-edgelist1---------------------------------------------------------
edgelist_matrix <- as.matrix(edgelist[,1:2])
head(edgelist_matrix)


## ----igraph-edgelist2---------------------------------------------------------
graph_from_edgelist(edgelist_matrix, directed=TRUE)


## ----igraph-edgelist3---------------------------------------------------------
edgelist_matrix_ch <- as.character(edgelist_matrix)
dim(edgelist_matrix_ch) <- dim(edgelist_matrix)
graph_from_edgelist(edgelist_matrix_ch, directed=TRUE)


## ----graph_data_frame---------------------------------------------------------
classroom_kids <- read.csv("classroom-nodes.csv", header=TRUE, colClasses=c(name = "character"))
head(classroom_kids)
classroom_play <- read.csv("classroom-edges.csv", header=TRUE, colClasses = c(from="character", to="character"))
head(classroom_play)

classroom <- graph_from_data_frame(classroom_play, vertices=classroom_kids,
                                   directed=TRUE)
classroom


## ---- cache=FALSE-------------------------------------------------------------
detach(package:igraph)


## ---- cache=FALSE-------------------------------------------------------------
library(tidygraph)


## ----tidygraph-create1--------------------------------------------------------
tg_classroom <- tbl_graph(nodes = classroom_kids, edges = classroom_play, 
                          directed = TRUE)
tg_classroom


## ----tidygraph-create2--------------------------------------------------------
# From igraph object created earlier
tg_classroom2 <- as_tbl_graph(classroom)
tg_classroom2

# From network object created earlier
tg_net <- as_tbl_graph(edgeNet)
tg_net


## ----tidygraph-mutate---------------------------------------------------------
tg_classroom %>%
  activate(nodes) %>%
  mutate(
    status = pmin(isei08_m, isei08_f, na.rm=TRUE)
  )


## ----tidygraph-pipe-----------------------------------------------------------
tg_classroom %>%
  activate(nodes) %>%
  mutate(
    status = pmin(isei08_m, isei08_f, na.rm=TRUE)
  ) %>%
  activate(edges) %>%
  mutate(
    like5 = liking == 5  # TRUE if liking is 5
  )


## -----------------------------------------------------------------------------
tg_classroom %>%
  activate(edges) %>%
  mutate(
    # Add edge attribute which is TRUE if gender of ego and alter match
    sex_match = .N()$female[from] == .N()$female[to]
  )


## ----tidygraph-filter-nodes---------------------------------------------------
tg_classroom %>%
  activate(nodes) %>%
  filter(female)


## ----tidygraph-filter-edges---------------------------------------------------
tg_classroom %>%
  activate(edges) %>%
  filter(liking >= 3)


## ---- cache=FALSE-------------------------------------------------------------
detach(package:tidygraph)


## ---- cache=FALSE-------------------------------------------------------------
library(graph)


## ----graph-adjacency----------------------------------------------------------
gr1 <- graphAM(relations, edgemode = "directed")
gr1


## ----graph-create-adjlist-----------------------------------------------------
adjlist <- apply(relations, 1, function(r) rownames(relations)[which(r == 1)])
head(adjlist) # initial elements of the adj. list


## ----graph-edgelist-----------------------------------------------------------
gr2 <- graphNEL(
  nodes = classroom_kids$name, # names of the nodes
  edgeL = adjlist, # adjacency list of node names
  edgemode = "directed"
)
gr2


## ----graph-vattr--------------------------------------------------------------
# Set the default value, say FALSE
nodeDataDefaults(gr2, attr="female") <- FALSE
# Assign the values
nodeData(gr2, n = classroom_kids$name, attr="female") <- classroom_kids$female


## ----graph-eattr--------------------------------------------------------------
edgeDataDefaults(gr2, attr = "liking") <- as.numeric(NA)
edgeData(gr2, 
         from = classroom_play$from, 
         to = classroom_play$to, 
         attr = "liking") -> classroom_play$liking


## ---- cache=FALSE-------------------------------------------------------------
detach(package:graph)


## ----intergraph---------------------------------------------------------------
# igraph -> network
classroom_network <- intergraph::asNetwork(classroom)

# network -> igraph
classroom_igraph <- intergraph::asIgraph(classroom_network)

classroom_network
classroom_igraph


## ---- echo=FALSE--------------------------------------------------------------
htmltools::includeHTML("captab.html")


## ---- cache=FALSE-------------------------------------------------------------
library(network)
library(sna)


## -----------------------------------------------------------------------------
gplot(relations) # Requires sna


## -----------------------------------------------------------------------------
plot(nrelations,displaylabels=T) # Plot with names
plot(nrelations,displaylabels=T,mode="circle") # A less useful layout...


## -----------------------------------------------------------------------------
gplot(relations,mode="target")


## -----------------------------------------------------------------------------
nodeColors<-ifelse(nodeInfo$female,"hotpink","dodgerblue")
plot(nrelations,displaylabels=T,vertex.col=nodeColors,vertex.cex=3)


## -----------------------------------------------------------------------------
plot(edgeNet,displaylabels=T) ##what's missing?
plot(edgeNet,displaylabels=T,edge.lwd=5*edgeNet%e%"question")


## -----------------------------------------------------------------------------
load("introToSNAinR.Rdata")
gplot(contig_1993) # The default visualization
gplot(contig_1993, usearrows=FALSE) # Turn off arrows manually


## -----------------------------------------------------------------------------
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE)


## -----------------------------------------------------------------------------
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE,displayisolates=FALSE)


## -----------------------------------------------------------------------------
coords <- gplot(contig_1993,gmode="graph",label=colnames(contig_1993[,]),label.cex=0.5,label.col="blue") # Capture the magic of the moment
head(coords) # Show the vertex coordinates


## -----------------------------------------------------------------------------
gplot(mids_1993,gmode="graph",label=colnames(contig_1993[,]),label.cex=0.5,label.col="blue",coord=coords)


## ----eval=FALSE---------------------------------------------------------------
## coords <- gplot(contig_1993, interactive=TRUE) # Modify and save
## gplot(contig_1993,coord=coords,displaylabels=TRUE,gmode="graph",label.cex=0.5,label.col="blue") # Should reproduce the modified layout


## ---- cache=FALSE-------------------------------------------------------------
# Detaching the packages
detach(package:sna)
detach(package:network)


## ---- cache=FALSE-------------------------------------------------------------
library("igraph")


## ---- echo=2:3----------------------------------------------------------------
layout(matrix(1:2, 1, 2))
plot(g1)
plot(g2)
layout(1)


## ----igraph-plot1-------------------------------------------------------------
plot(
  classroom, 
  layout=layout_with_fr,
  vertex.color="white",
  vertex.size=15,
  edge.arrow.size=0.5,
  vertex.label.color="black",
  vertex.label.family="sans",
  vertex.label=ifelse(V(classroom)$female, "F", "M") 
)


## ----igraph-plot2-------------------------------------------------------------
plot(
  classroom, 
  layout=layout_with_fr,
  vertex.label=NA,
  vertex.size=scales::rescale(degree(classroom, mode="in"), c(5, 25)),
  edge.arrow.size=0.5,
  vertex.color=ifelse(V(classroom)$female, "pink", "lightskyblue") 
)


## ---- cache=FALSE-------------------------------------------------------------
detach(package:igraph)


## ---- cache=FALSE-------------------------------------------------------------
library(network)
library(sna)


## -----------------------------------------------------------------------------
network.dyadcount(nrelations) # How many dyads?
network.edgecount(nrelations) # How many edges are present?
network.size(nrelations) # How large is the network?


## -----------------------------------------------------------------------------
degree(mids_1993) # Default: total degree
ideg <- degree(mids_1993, cmode="indegree") # Indegree for MIDs
odeg <- degree(mids_1993, cmode="outdegree") # Outdegree for MIDs
all(degree(mids_1993) == ideg+odeg) # In + out = total?


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
hist(ideg, 
     xlab="Indegree",
     main="Indegree Distribution", 
     prob=TRUE)

hist(odeg, 
     xlab="Outdegree", 
     main="Outdegree Distribution", 
     prob=TRUE)


## -----------------------------------------------------------------------------
gplot(mids_1993, 
      vertex.cex=(ideg+odeg)^0.5, 
      vertex.sides=50,
      label.cex=0.4,
      vertex.col=rgb(odeg/max(odeg),0,ideg/max(ideg)),
      displaylabels=TRUE,
      displayisolates=FALSE)


## -----------------------------------------------------------------------------
bet <- betweenness(contig_1993, 
                   gmode="graph") # Geographic betweenness

bet


## -----------------------------------------------------------------------------
gplot(contig_1993, 
      vertex.cex=sqrt(bet)/25, 
      gmode="graph") # Use w/gplot

clo <- closeness(contig_1993) # Geographic closeness
clo # A large world after all?

closeness(contig_1993,cmode="suminvundir") 


## -----------------------------------------------------------------------------
centralization(mids_1993, degree, cmode="indegree") # Do MIDs concentrate?
centralization(contig_1993, evcent) # Eigenvector centralization


## -----------------------------------------------------------------------------
gden(mids_1993) # Density


## -----------------------------------------------------------------------------
dyad.census(mids_1993)
dyad.census(contig_1993)


## -----------------------------------------------------------------------------
grecip(mids_1993) # Dyadic reciprocity
grecip(mids_1993, measure="edgewise") # Edgewise reciprocity


## -----------------------------------------------------------------------------
gtrans(mids_1993) # Transitivity


## ---- cache=FALSE-------------------------------------------------------------
# Detaching the packages
detach(package:sna)
detach(package:network)


## ---- cache=FALSE-------------------------------------------------------------
library("igraph")


## ----igraph-basics------------------------------------------------------------
summary(g1)
ecount(g1)      # number of edges
vcount(g1)      # number of vertices
is.directed(g1) # is the network directed?


## ----igraph-density-----------------------------------------------------------
edge_density(classroom)


## ----igraph-resiprocity, echo=-4----------------------------------------------
g <- make_graph(c(1,2, 2,3, 3,2), n = 3)
reciprocity(g)
reciprocity(g, mode="ratio")
plot(g)


## ----igraph-degree------------------------------------------------------------
degree(classroom)
degree(classroom, mode="in")
degree(classroom, mode="out")


## ----igraph-degree-distribution-----------------------------------------------
degree_distribution(classroom)


## ----igraph-centrality--------------------------------------------------------
betweenness(classroom)
closeness(classroom)


## ---- cache=FALSE-------------------------------------------------------------
detach(package:igraph)


## ----session-info, echo = FALSE-----------------------------------------------
sessioninfo::session_info()


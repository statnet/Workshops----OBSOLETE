set.seed(9)
#data(cohab) # alternatively, load("cohab.RData") using separate cohab.RData file
load("cohab.Rdata")
cohab_PopWts$race[cohab_PopWts$race=="B"] <- "Black"
cohab_PopWts$race[cohab_PopWts$race=="BI"] <- "BlackImm"
cohab_PopWts$race[cohab_PopWts$race=="H"] <- "Hisp"
cohab_PopWts$race[cohab_PopWts$race=="HI"] <- "HispImm"
cohab_PopWts$race[cohab_PopWts$race=="W"] <- "White"
net_size <- 200
nw <- network.initialize(net_size, directed = FALSE)
inds <- sample(seq_len(NROW(cohab_PopWts)), net_size, TRUE, cohab_PopWts$weight)
set.vertex.attribute(nw, names(cohab_PopWts)[-c(1, 7, 8)], cohab_PopWts[inds,-c(1, 7, 8)])
ff <- nw ~ edges + 
  nodefactor("sex.ident", levels = 3) +
  nodecov("age") +
  nodefactor("race", levels = -5) +
  nodefactor("deg.pers.c", levels = -1) +
  nodematch("race", diff = TRUE) 
Snw <- san(ff, 
                   target.stats=cohab_TargetStats[-c(4,15)]/5e4*net_size, 
                   constraints = ~bd(maxout = 1) + blocks(attr = ~sex, levels2 = diag(TRUE, 2))
                   )
save(Snw, file="Snw.RData")

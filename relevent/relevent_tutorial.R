## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache=F, comment=NA)
# knitr::opts_chunk$set(tidy = "styler")
knitr::opts_chunk$set(tidy.opts=list(indent=3, width.cutoff=60))


## ----outhook, include=FALSE---------------------------------------------------
local({
  hook_output <- knitr::knit_hooks$get('output')
  knitr::knit_hooks$set(output = function(x, options) {
    if (!is.null(options$max.height)) options$attr.output <- c(
      options$attr.output,
      sprintf('style="max-height: %s;"', options$max.height)
    )
    hook_output(x, options)
  })
})


## ----dev, child = '../statnetDevTeam.Rmd'-------------------------------------




## ----project, child = '../statnetProject.Rmd'---------------------------------




## ----install-relevent,eval=FALSE----------------------------------------------
## install.packages(c("relevent","sna"))


## ----loadPackage--------------------------------------------------------------
library(relevent)
library(sna)


## ----version------------------------------------------------------------------
packageVersion("relevent")


## ----relevent-load, tidy=TRUE, eval=TRUE, tidy.opts=list(indent=3, width.cutoff=80)----
library(relevent)                     #Load the relevent library
load("relevent_workshop.Rdata")       #Load the workshop data - may need to change directory!


## ----head-police, tidy=TRUE, eval=TRUE, max.height="200px"--------------------
WTCPoliceCalls


## ----plot-police, tidy=TRUE, eval=TRUE----------------------------------------
WTCPoliceNet <- as.sociomatrix.eventlist(WTCPoliceCalls,37)
gplot(WTCPoliceNet,edge.lwd=WTCPoliceNet^0.75,vertex.col=2+WTCPoliceIsICR,vertex.cex=1.25)


## ----wtcfit1, tidy=TRUE, eval=TRUE--------------------------------------------
#First ICR effect - total interaction
wtcfit1<-rem.dyad(WTCPoliceCalls,n=37,effects=c("CovInt"),covar=list(CovInt=WTCPoliceIsICR), hessian=TRUE)
summary(wtcfit1)


## ----icrrate, tidy=TRUE, eval=TRUE--------------------------------------------
exp(wtcfit1$coef)      #Relative hazard for a non-ICR/ICR vs. a non-ICR/non-ICR event
exp(2*wtcfit1$coef)    #Relative hazard for an ICR/ICR vs. a non-ICR/non-ICR event


## ----wtcfit2, tidy=TRUE, eval=TRUE--------------------------------------------
wtcfit2<-rem.dyad(WTCPoliceCalls,n=37,effects=c("CovSnd","CovRec"), covar=list(CovSnd=WTCPoliceIsICR,CovRec=WTCPoliceIsICR),hessian=TRUE)
summary(wtcfit2)


## ----wtc2v1-bic, tidy=TRUE, eval=TRUE-----------------------------------------
wtcfit1$BIC-wtcfit2$BIC       #Model 1 a bit lower - we prefer it


## ----wtcfit2-wald, tidy=TRUE, eval=TRUE---------------------------------------
wtcfit2$coef          #Extract the coefficients
wtcfit2$cov           #Likewise, the posterior covariance matrix
#Heuristic Wald test of equality (not Bayesian, but whatever)
z<-diff(wtcfit2$coef)/sqrt(sum(diag(wtcfit2$cov))-2*wtcfit2$cov[1,2])
z
2*(1-pnorm(abs(z)))   #Not conventionally significant - not strongly detectable


## ----wtcfit3, tidy=TRUE, eval=TRUE--------------------------------------------
wtcfit3<-rem.dyad(WTCPoliceCalls, n=37, effects=c("CovInt","PSAB-BA"), covar=list(CovInt=WTCPoliceIsICR), hessian=TRUE)
summary(wtcfit3)             #Looks like a strong effect...
wtcfit1$BIC-wtcfit3$BIC      #We prefer model 3 to model 1 - reciprocity is in!
exp(wtcfit3$coef["PSAB-BA"]) #Reciprocating events are >1500 times as likely


## ----wtcfit4, tidy=TRUE, eval=TRUE--------------------------------------------
wtcfit4<-rem.dyad(WTCPoliceCalls, n=37, effects=c("CovInt","PSAB-BA","PSAB-BY","PSAB-AY"), covar=list(CovInt=WTCPoliceIsICR), hessian=TRUE)
summary(wtcfit4)          #Seems like the effects are present, but let's test GOF...
wtcfit3$BIC-wtcfit4$BIC   #Yes, definite improvement


## ----wtcfit5, tidy=TRUE, eval=TRUE--------------------------------------------
wtcfit5<-rem.dyad(WTCPoliceCalls, n=37, effects=c("CovInt","PSAB-BA","PSAB-BY", "PSAB-AY","RRecSnd","RSndSnd"), covar=list(CovInt=WTCPoliceIsICR), hessian=TRUE)
summary(wtcfit5)          #Looks good; note that AB-BA is much smaller than before
wtcfit4$BIC-wtcfit5$BIC   #Substantial improvement


## ----wtcfit6, tidy=TRUE, eval=TRUE--------------------------------------------
wtcfit6<-rem.dyad(WTCPoliceCalls, n=37, effects=c("CovInt","PSAB-BA","PSAB-BY", "PSAB-AY","RRecSnd","RSndSnd","NTDegRec"), covar=list(CovInt=WTCPoliceIsICR), hessian=TRUE)
summary(wtcfit6)           #PA is drawing from recency, ICR effect, but not P-shifts
wtcfit5$BIC-wtcfit6$BIC    #Model is preferred


## ----wtc-deviance, tidy=TRUE, eval=TRUE---------------------------------------
nullresid<- 2*log(37*36)   #What would be the deviance residual for the null?
hist(wtcfit6$residuals)    #Deviance residuals - most well-predicted, some around chance levels
abline(v=nullresid,col=2)
mean(wtcfit6$residuals<nullresid) #Beating chance on almost all...
mean(wtcfit6$residuals<3)         #Upper limit of lower cluster is about 3


## ----wtc-guess, tidy=TRUE, eval=TRUE------------------------------------------
quantile(exp(wtcfit6$residuals/2))   #"Random guessing equivalent" (ref is 1332)


## ----wtc-quantres, tidy=TRUE, eval=TRUE---------------------------------------
quantile(exp(wtcfit1$residuals/2))  #By comparison, first model much worse!


## ----wtc-surprise, tidy=TRUE, eval=TRUE, max.height="200px"-------------------
cbind(WTCPoliceCalls,wtcfit6$residuals>nullresid) #Which are the more surprising cases?


## ----wtc-surprise-net, tidy=TRUE, eval=TRUE-----------------------------------
surprising<-as.sociomatrix.eventlist(WTCPoliceCalls[wtcfit6$residuals>nullresid,],37)
gplot(surprising)  #Plot in network form


## ----wtc-surprise-net2, tidy=TRUE, eval=TRUE----------------------------------
#Can also superimpose on the original network (coloring edges by fraction surprising)
edgecol<-matrix(rgb(surprising/(WTCPoliceNet+0.01),0,0),37,37)   #Color me surprised
gplot(WTCPoliceNet,edge.col=edgecol,edge.lwd=WTCPoliceNet^0.75,vertex.col=2+WTCPoliceIsICR)


## ----wtc-recall, tidy=TRUE, eval=TRUE, max.height="200px"---------------------
hist(wtcfit6$observed.rank)
cbind(WTCPoliceCalls,wtcfit6$observed.rank)  #Histogram of ranks
#Rank on a per-event basis (low is good)
#Sometimes useful to plot the ECDF of the observed ranks....
plot(ecdf(wtcfit6$observed.rank/(37*36)), xlab="Prediction Threshold (Fraction of Possible Events)", ylab="Fraction of Observed Events Covered",main="Classification Accuracy")
abline(v=c(0.05,0.1,0.25),col=2)


## ----wtc-predmatch, tidy=TRUE, eval=TRUE, max.height="200px"------------------
wtcfit6$predicted.match #Exactly correct src/target 
mean(apply(wtcfit6$predicted.match,1,any)) #Fraction for which something is right
mean(apply(wtcfit6$predicted.match,1,all)) #Fraction entirely right
colMeans(wtcfit6$predicted.match) #Fraction src/target, respectively


## ----tidy=TRUE, eval=FALSE----------------------------------------------------
## simulate(object, nsim = object$m, seed = NULL, coef = NULL, covar = NULL, verbose = FALSE, ...)


## ----wtcsim, tidy=TRUE, eval=TRUE, max.height="200px"-------------------------
set.seed(1331)
simwtc<-simulate(wtcfit6, covar=list(CovInt=WTCPoliceIsICR), verbose=TRUE)


## ----wtcsimhead, tidy=TRUE, eval=TRUE, max.height="200px"---------------------
simwtc


## ----wtc-knockout, tidy=TRUE, eval=TRUE---------------------------------------
set.seed(1331)
reps<-6                      #Number of replicate series to take
kocoef<-wtcfit6$coef         #Knock-out coefs
kocoef["PSAB-BA"]<-0
ICRBetCor<-matrix(nrow=reps,ncol=3)
for(i in 1:reps){
  print(i)
  simwtc<-simulate(wtcfit1, covar=list(CovInt=WTCPoliceIsICR))               #ICR only
  ICRBetCor[i,1]<-cor(betweenness(as.sociomatrix.eventlist(simwtc, 37)), WTCPoliceIsICR)
  simwtc<-simulate(wtcfit6, covar=list(CovInt=WTCPoliceIsICR))               #Final
  ICRBetCor[i,2]<-cor(betweenness(as.sociomatrix.eventlist(simwtc, 37)), WTCPoliceIsICR)
  simwtc<-simulate(wtcfit6, covar=list(CovInt=WTCPoliceIsICR), coef=kocoef)  #Knockout
  ICRBetCor[i,3]<-cor(betweenness(as.sociomatrix.eventlist(simwtc, 37)), WTCPoliceIsICR)
}
boxplot(ICRBetCor, names=c("ICROnly", "Full", "NoABBA"))
abline(h=cor(betweenness(as.sociomatrix.eventlist(WTCPoliceCalls, 37)), WTCPoliceIsICR),col=2)


## ----class, tidy=TRUE, eval=TRUE----------------------------------------------
head(Class)
tail(Class)


## ----class-net, tidy=TRUE, eval=TRUE------------------------------------------
ClassNet<-as.sociomatrix.eventlist(Class,20)
gplot(ClassNet, vertex.col=4-2*ClassIsFemale, vertex.sides=3+ClassIsTeacher, vertex.cex=2, edge.lwd=ClassNet^0.75)


## ----classfit1, tidy=TRUE, eval=TRUE------------------------------------------
classfit1<-rem.dyad(Class, n=20, effects=c("CovSnd"), covar=list(CovSnd=ClassIntercept), ordinal=FALSE, hessian=TRUE)

summary(classfit1)


## ----class-timing, tidy=TRUE, eval=TRUE---------------------------------------
(classfit1$m-1)/max(Class[,1])    #Events per minute (on average)
20*19*exp(classfit1$coef)         #Predicted events per minute (matches well!)


## ----classfit2, tidy=TRUE, eval=TRUE------------------------------------------
classfit2<-rem.dyad(Class, n=20, effects=c("CovSnd", "CovRec"), covar=list(CovSnd=cbind(ClassIntercept, ClassIsTeacher, ClassIsFemale), CovRec=cbind(ClassIsTeacher, ClassIsFemale)), ordinal=FALSE, hessian=TRUE)
summary(classfit2)
classfit1$BIC-classfit2$BIC   #Model is preferred


## ----classfit3, tidy=TRUE, eval=TRUE------------------------------------------
classfit3<-rem.dyad(Class, n=20, effects=c("CovSnd", "CovRec"), covar=list(CovSnd=cbind(ClassIntercept, ClassIsTeacher), CovRec=cbind(ClassIsTeacher, ClassIsFemale)), ordinal=FALSE, hessian=TRUE)
summary(classfit3)
classfit2$BIC-classfit3$BIC    #Reduced model is indeed preferred


## ----classfit4, tidy=TRUE, eval=TRUE, tidy.opts=list(indent=3, width.cutoff=60)----
classfit4<-rem.dyad(Class, n=20, effects=c("CovSnd", "CovRec", "RRecSnd", "RSndSnd"), covar=list(CovSnd=cbind(ClassIntercept, ClassIsTeacher), CovRec=cbind(ClassIsTeacher, ClassIsFemale)), ordinal=FALSE, hessian=TRUE)
summary(classfit4)
classfit3$BIC-classfit4$BIC   #Enhanced model is preferred


## ----classfit5, tidy=TRUE, eval=TRUE------------------------------------------
classfit5<-rem.dyad(Class, n=20, effects=c("CovSnd", "CovRec", "RRecSnd", "RSndSnd", "PSAB-BA", "PSAB-AY", "PSAB-BY"), covar=list(CovSnd=cbind(ClassIntercept, ClassIsTeacher), CovRec=cbind(ClassIsTeacher, ClassIsFemale)), ordinal=FALSE, hessian=TRUE)
summary(classfit5)
classfit4$BIC-classfit5$BIC    #Enhanced model is again preferred


## ----classfit6, tidy=TRUE, eval=TRUE------------------------------------------
classfit6<-rem.dyad(Class, n=20, effects=c("CovSnd", "CovRec", "RRecSnd", "RSndSnd", "PSAB-BA", "PSAB-AY", "PSAB-BY"), covar=list(CovSnd=cbind(ClassIntercept, ClassIsTeacher), CovRec=ClassIsTeacher), ordinal=FALSE, hessian=TRUE)
summary(classfit6)
classfit5$AICC-classfit6$AICC  #Reduced model is indeed preferred


## ----classfit6-timing, tidy=TRUE, eval=TRUE-----------------------------------
exp(classfit6$coef["PSAB-BA"]) #Response events have apx 100 times the hazard of other events


## ----class-timing-scenarios, tidy=TRUE, eval=TRUE-----------------------------
#Mean inter-event time if nothing else going on....
1/(20*19*exp(classfit6$coef["CovSnd.1"]))

#Mean teacher-student time (again, if nothing else happened)
1/(2*18*exp(sum(classfit6$coef[c("CovSnd.1","CovSnd.2")])))

#Sequential address by teacher w/out prior interaction, given a prior teacher-student
#interaction, and assuming nothing else happened
1/(17*exp(sum(classfit6$coef[c("CovSnd.1","CovSnd.2","PSAB-AY")])))

#Teacher responding to a specific student, given an immediate event
1/(exp(sum(classfit6$coef[c("CovSnd.1","CovSnd.2","PSAB-BA","RRecSnd")])))

#Student responding to a specific teacher, given an immediate event
1/(exp(sum(classfit6$coef[c("CovSnd.1","CovRec.1","PSAB-BA","RRecSnd")])))


## ----class-surprise, tidy=TRUE, eval=TRUE-------------------------------------
#Where is the model "surprised"? Can't use null residual trick, but can see
#what the distribution looks like
hist(classfit6$residuals)    #Deviance residuals - lumpier by far, most smallish


## ----class-predmatch, tidy=TRUE, eval=TRUE, max.height="200px"----------------
mean(apply(classfit6$predicted.match,1,all))    #Exactly right about 33%
mean(apply(classfit6$predicted.match,1,any))    #Get one party exactly right 52%
colMeans(classfit6$predicted.match)             #Better at sender than receiver!
classfit6$observed.rank
cbind(Class,c(classfit6$observed.rank,NA))


## ----class-surprise-net, tidy=TRUE, eval=TRUE---------------------------------
#Get the surprising events, and display as a network
surprising<-as.sociomatrix.eventlist(Class[classfit6$observed.rank>19,],20)
gplot(surprising, vertex.col=4-2*ClassIsFemale, vertex.sides=3+ClassIsTeacher, vertex.cex=2)

#Show how the "surprising" events fit into the broader communication structure
edgecol<-matrix(rgb(surprising/(ClassNet+0.01),0,0),20,20) #Color me surprised
gplot(ClassNet, edge.col=edgecol, edge.lwd=ClassNet^0.75, vertex.col=4-2*ClassIsFemale, vertex.sides=3+ClassIsTeacher, vertex.cex=2)


## ----class-sim, tidy=TRUE, eval=TRUE, max.height="200px"----------------------
set.seed(1331)
ClassSim<-simulate(classfit6, covar=list(CovSnd=cbind(ClassIntercept,ClassIsTeacher), CovRec=ClassIsTeacher))

ClassSim  #Examine the resulting trajectory


## ----anarchy-sim, tidy=TRUE, eval=TRUE, max.height="200px"--------------------
set.seed(1331)
AnarchSim<-simulate(classfit6, covar=list(CovSnd=cbind(ClassIntercept,rep(0,20)), CovRec=rep(0,20)))

AnarchSim  #Examine the trajectory

#Plot the network structure of the simulations, and the observed data
par(mfrow=c(2,2), mar=c(2,2,2,2))
gplot(ClassNet, vertex.col=4-2*ClassIsFemale, vertex.sides=3+ClassIsTeacher, vertex.cex=2, edge.lwd=ClassNet^0.75, main="Observed Network", edge.col=rgb(0,0,0,(1-1/(1+ClassNet))^3))
SimNet<-as.sociomatrix.eventlist(ClassSim,20)  #Create a network from the fitted sim
gplot(SimNet, vertex.col=4-2*ClassIsFemale, vertex.sides=3+ClassIsTeacher, vertex.cex=2, edge.lwd=SimNet^0.75, main="Simulated Network", edge.col=rgb(0,0,0,(1-1/(1+SimNet))^3))
AnarchNet<-as.sociomatrix.eventlist(AnarchSim,20)  #Create a network from the anarchy sim
gplot(AnarchNet, vertex.col=4-2*ClassIsFemale, vertex.sides=3+ClassIsTeacher, vertex.cex=2,edge.lwd=AnarchNet^0.75, main="Anarchic Network", edge.col=rgb(0,0,0,(1-1/(1+AnarchNet))^3))

#Plot the valued degree distributions
plot(density(degree(ClassNet),bw="SJ"),lwd=3,main="Degree Distribution")
lines(density(degree(SimNet),bw="SJ"),lwd=3,col=2)
lines(density(degree(AnarchNet),bw="SJ"),lwd=3,col=4)
legend("topright",legend=c("Obs","Sim","Anarch"),lwd=3,col=c(1,2,4))


## ----modskel, tidy=TRUE, eval=TRUE--------------------------------------------
ModInt<-rep(1,25)
modskel <- rem.dyad(NULL, n=25, effects=c("CovSnd", "PSAB-BA", "RSndSnd"), covar=list(CovSnd=ModInt))
modskel


## ----modskel-sim, tidy=TRUE, eval=TRUE----------------------------------------
set.seed(1331)
modsim<-simulate(modskel, nsim=100, coef=c(0.25,-1,4), covar=list(CovSnd=ModInt))
head(modsim)                                  #See the trajectory
grecip(as.sociomatrix.eventlist(modsim,25), measure="edgewise")    #Relatively reciprocal


## ----modyncov, tidy=TRUE, eval=TRUE-------------------------------------------
set.seed(1331)
#Set up the model
tcovar<-array(sweep(sapply(1:10,rep,100),1,1/1.05^(0:99),"*"),dim=c(100,10,1))
SndInt<-rep(1,10)
#Note that, in making the skeleton, we need to pass the covariates as if
#they are static - that's because the model doesn't contain time points yet.
modskel2<-rem.dyad(NULL, n=10, effects=c("CovSnd", "CovInt"), coef.seed=c(-1,1), covar=list(CovSnd=SndInt, CovInt=tcovar[1,,1]))

#Simulate draws
modsim2<-simulate(modskel2, nsim=100, covar=list(CovSnd=SndInt, CovInt=tcovar))

#Note that dynamics slow down, and participation evens out
plot(diff(modsim2[,1]), col=hsv(modsim2[,2]/10*0.6), pch=19, ylab="Inter-event Time")
lines(supsmu(x=2:100, y=diff(modsim2[,1])))


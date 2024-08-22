

### Analysis of decathlon data using thresholds models

library(FactoMineR)
library(dplyr)
library(spatstat) 

## needed programs for fitting:
source("Responsefunctions.R")
source("Difficultyfunctions.R")
source("ProgramsBsplinesPureResp.R")
source("Posteriorgen.R")


data(decathlon)
decathlon



#deca<-decathlon[14:41,]  # only olympic
deca<-decathlon
deca<-data.frame(deca)
deca
cor(deca[,1:10])

### some plots of data


d <- density(deca$X100m) # returns the density data
plot(d)

d <- density(1/deca$X100m) # returns the density data
plot(d)

d <- density(deca$X110m.hurdle) # returns the density data
plot(d)

d <- density(1/deca$X400m) # returns the density data
plot(d)

d <- density(deca$Long.jump) # returns the density data
plot(d)

d <- density(1/deca$Long.jump) # returns the density data
plot(d)


### select tasks

###########################
## specification of data sets

##run
decarun<-select(deca,X100m,X400m,X110m.hurdle,X1500m)
indrun<-c(1,5,6,10)
#decarunneg<--decarun

#jump
decajump<-select(deca,Long.jump,High.jump,Pole.vault)
indjump<-c(2,4,8)

#throw
decathrow<-select(deca,Shot.put,Javeline,Discus)
indthrow<-c(3,9,7)

# all
decaall<-deca[,1:10]
decaall[,indrun]<--decaall[,indrun]
indall<-1:10

## almost all 8without pole valult, 1500 m
decaalmost<-decaall
decaalmost<-decaalmost[,-c(3,10)]
indalmost<-1:10
indalmost<-indalmost[-c(3,10)]

###########################

#selection of data sets: select only one  

#jump
decaselect<-decajump
indsel<-indjump

#run
decaselect<-decarun
#decaselect<-decarunneg
indsel<-indrun

#throw
decaselect<-decathrow
indsel<-indthrow

## all
decaselect<-decaall
indsel<-indall

## almost all 8without pole valult, 1500 m
decaselect<-decaalmost
indsel<-indalmost

## corr
round(cor(deca[,indjump]), digits=2)  
round(cor(deca[,indrun]), digits=2) 
round(cor(deca[,indthrow]), digits=2) 


### specify difficulty and response function (default: lin <- "lin", respf<- "NV" )

lin <- "lin"  
#lin <- "log"
#lin <- "log1"
#lin <- "logit"

respf<- "NV"
#respf<- "logistic"
#respf<- "Gumbel"
#respf<- "Gompertz"

indicator<- "C"  #### responses are continuous
#indicator<- "D"


### always run (1) and (2) after selection of lin and respfct   

####(1) define function for response function

## first run "Responsefunctions"

if(respf=="NV"){respfctd <- respfctdNV
respfctc<-respfctcNV
respfctder<-respfctderNV}
if(respf=="logistic"){respfctd <- respfctdlogit
respfctc<-respfctclogit
respfctder<-respfctderlogit}
if(respf=="Gumbel"){respfctd <- respfctdGumbel
respfctc<-respfctcGumbel
respfctder<-respfctderGumbel}
if(respf=="Gompertz"){respfctd <- respfctdGompertz
respfctc<-respfctcGompertz
respfctder<-respfctderGompertz}

####(2) define function for difficulty function

## first run "Difficultyfunctions"

if(lin =="lin") {diffunct<-diffunctlin
derdiffunct<-derdiffunctlin}
if(lin =="log") {diffunct<-diffunctlog
derdiffunct<-derdiffunctlog}
if(lin =="log1") {diffunct<-diffunctlog1
derdiffunct<-derdiffunctlog1}






#### fitting od data set decaselect

### needed:

decaselecttr<-t(decaselect)  # transposed
I<- dim(decaselecttr)[1]
indicatorsingle  <- "C"  #### discrete or continuous
indicatorvec  <- indicatorsingle
for (l in 2:I )indicatorvec<- c(indicatorvec,indicatorsingle)
indicator<-indicatorvec



cov0<-matrix(0,1,dim(decaselecttr)[2])

# first run ProgramsBsplinesPureResp

## use if response function is Nv
ThrNV<-ThreshbasisRespCov(decaselecttr,basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start, 
                          startind = "no",der="der",cov0,hessiander="FALSE")


## use if response function is Gompertz
ThrGompertz <-ThreshbasisRespCov(decaselecttr,basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start, 
                            startind = "no",der="der",cov0,hessiander="FALSE")


## use if response function is Gumbel
ThrGumb<-ThreshbasisRespCov(decaselecttr,basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start, 
                            startind = "no",der="der",cov0,hessiander="FALSE")



### with start values from NV fit (if start values are bad)

ThrGumb<-ThreshbasisRespCov(decaselecttr,basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start=ThrNV$par, 
                            startind = "yes",der="der",cov0,hessiander="FALSE")

ThrGompertz<-ThreshbasisRespCov(decaselecttr,basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start=ThrNV$par, 
                            startind = "yes",der="der",cov0,hessiander="FALSE")


round(cor(deca[,1:10]), digits=2)  
round(ThrGompertz$itemmatrixtval, digits=3)


## Posterior

grid<- seq(-6,6,.05)

# select
parmatrest<-ThrNV$parmatrix[, 1:2]
stdest<-ThrNV$stdmixt

parmatrest<-ThrGumb$parmatrix[, 1:2]
stdest<-ThrGumb$stdmixt

parmatrest<-ThrGompertz$parmatrix[, 1:2]
stdest<-ThrGompertz$stdmixt

## run Posteriorgen

dat<-decaselecttr
tt<-PosteriorEstimatesGen(grid,dat,I, indicatorvec,lin =lin,basis="notspl",ord, numknotsstart,parmatrest[, 1:2],stdest)


 
##  memory of fits:

#jumpGumb<-tt$thetacen
#runGomp<-tt$thetacen
#ThrowNV<-tt$thetacen
#allNV<-tt$thetacen
#almostallNV<-tt$thetacen

plot(jumpGumb,-runGomp)
cor(jumpGumb,-runGomp)

plot(jumpGumb,ThrowNV)
cor(jumpGumb,ThrowNV)

plot(ThrowNV,-runGomp)
cor(ThrowNV,-runGomp)

par(cex.lab=1,cex.axis=1.8,cex.main=2.20,pch=16,cex=1.0)
par(mar=c(8,7,6,1)+.2)

plot(jumpGumb,-runGomp,cex.lab=2.0, main ="",xlab ="Ability jumping", ylab ="Ability running")
cor(jumpGumb,-runGomp)

plot(jumpGumb,deca$Points)
cor(jumpGumb,deca$Points)
cor(runGomp,deca$Points)

##### correlation subscores
dimensions <-cbind(jumpGumb,-runGomp,ThrowNV)

cor(dimensions )
dimensionsfr<-data.frame(dimensions)

names(dimensionsfr)[1] <- "Jumping"
names(dimensionsfr)[2] <- "Running"
names(dimensionsfr)[3] <- "Throwing"
plot(dimensionsfr, pch=19 )

sumscore<-jumpGumb-runGomp+ThrowNV

par(mar=c(8,7,6,1)+.2)
plot(sumscore,decathlon$Points, pch=19)
cor(sumscore,decathlon$Points)


par(cex.lab=1,cex.axis=1.8,cex.main=2.20,pch=16,cex=1.0)
par(mar=c(7,6,6,1)+.82)

plot(allNV,decathlon$Points, pch=19,xlab ="Estimated latent trait",ylab="Points",lwd=3, cex=2,cex.lab=2.0)
cor(allNV,decathlon$Points)

plot(allNV,decathlon$Points, pch=1,xlab ="Estimated latent trait",ylab="Points",lwd=3, cex=2,cex.lab=2.0)
cor(allNV,decathlon$Points)


### combined theta observations
jumpcomb<-cbind(jumpGumb,deca[,indjump])
round(cor(jumpcomb), digits=2) 

runcomb<-cbind(runGomp,deca[,indrun])
round(cor(runcomb), digits=2)

allcomb<-cbind(allNV,deca[,indjump],-deca[,indrun],deca[,indthrow])
round(cor(allcomb), digits=2)


almostcomb<-cbind(almostallNV,deca[,indjump],-deca[,indrun],deca[,indthrow])
round(cor(almostcomb), digits=2)


#### Plots  

## select

#parmatrest <- ThrGumb$parmatrix ### now 2
#parmatrest <- ThrNV$parmatrix
parmatrest <- ThrGompertz$parmatrix[,1:2]
#parmatrest <- Thralpha1$parmatrix[,1:2]

dat<-decaselecttr 


pcdum <-c ('1','2','3','4','5','6','7','8','9')

theta <-0 # specify theta

plotitems<-1:dim(dat)[1]  ## all items

par(cex.lab=1,cex.axis=1.8,cex.main=2.20,pch=16,cex=1.0)
par(mar=c(7,6,6,1)+.2)

for (i in plotitems){
  width<-max(dat[i,])-min(dat[i,])
  min <- min(dat[i,])-.2*width
  max <-  max(dat[i,])+.2*width
  
  y <- seq(min,max,(max-min)/45)
  prob<-0*y
  
    for (l in 1:length(y))prob[l]<- {respfctd(theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])
  } 
  sum(prob)*(y[2]-y[1])
  #lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b", pch =pcdum[i])
  plot(y,prob,cex.lab=2.0, main =names(deca)[indsel[i]],xlab ="y", type="b",
       lwd=3,  ylab="Density", pch =1)#pcdum[1])
  }


### plot reverse
for (i in plotitems){
  width<-max(dat[i,])-min(dat[i,])
  min <- min(dat[i,])-.2*width
  max <-  max(dat[i,])+.2*width
  
  y <- seq(min,max,(max-min)/45)
  
  
  for (l in 1:length(y))prob[l]<- {respfctd(theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])
  } 
  sum(prob)*(y[2]-y[1])
  #lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b", pch =pcdum[i])
  rev<-seq(length(y),1,-1)
  prob[rev]
  plot(-y,prob,cex.lab=2.0, main =names(deca)[indsel[i]],xlab ="y", type="b",
       lwd=3,  ylab="Density", pch =1)#pcdum[1])
}


## more lines

theta1 <-0 # specify theta
theta2 <-1 # specify theta

plotitems<-1:dim(dat)[1]  ## all items

par(cex.lab=1,cex.axis=1.8,cex.main=2.20,pch=16,cex=1.0)
par(mar=c(7,6.5,6.5,1)+.2)

for (i in plotitems){
  width<-max(dat[i,])-min(dat[i,])
  min <- min(dat[i,])-.3*width
  max <-  max(dat[i,])+.2*width
  
  y <- seq(min,max,(max-min)/45)
  prob<-0*y
  
  for (l in 1:length(y))prob[l]<- {respfctd(theta1 - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])
  } 
  sum(prob)*(y[2]-y[1])
  #lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b", pch =pcdum[i])
  plot(y,prob,cex.lab=2.0, main =names(deca)[indsel[i]],xlab ="y", type="b",
       lwd=3,  ylab="Density", pch =1)#pcdum[1])

  for (l in 1:length(y))prob[l]<- {respfctd(theta2 - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])
  } 
  sum(prob)*(y[2]-y[1])
  #lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b", pch =pcdum[i])
  lines(y,prob,cex.lab=2.0, main =names(deca)[indsel[i]], type="b",
       lwd=3,   pch =5)#pcdum[1])
  }





### delta function plot

#parmatrest <- ThrGumb$parmatrix ### now 2
#parmatrest <- ThrNV$parmatrix
parmatrest <- ThrGompertz$parmatrix[,1:2]


yb <- seq(1,8,8/45)
delta<-0*yb

namescomp <-c ('L','H','P','4','5','6','7','8','9')



for (l in 1:length(yb)){delta[l]<-diffunct(parmatrest[1,1],parmatrest[1,2],y[l])}  
plot(yb,delta,cex.axis=1.5,cex.lab=1.5,cex.main=2, main ="Delta function",xlab ="y", type="b",lwd=1.2,
     ylab="", pch =namescomp[1], ylim=c(-25,65))


for (i in 2:I){
  
  for (l in 1:length(yb))delta[l]<-diffunct(parmatrest[i,1],parmatrest[i,2],y[l])
  
  lines(yb,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =namescomp[i],cex = 1)
}

length(yb)
length(delta)


####PT function plot

theta<-0
plotitems<-1:dim(dat)[1]  ## all items

par(cex.lab=1,cex.axis=1.8,cex.main=2.20,pch=16,cex=1.0)
par(mar=c(7,6.5,6.5,1)+.8)

for (i in plotitems){
  width<-max(dat[i,])-min(dat[i,])
  min <- min(dat[i,])-.1*width
  max <-  max(dat[i,])+.1*width
  
  y <- seq(min,max,(max-min)/45)
  problargy<-0*y
  for (l in 1:length(yb)){delta[l]<-diffunct(parmatrest[i,1],parmatrest[i,2],y[l])}
     
  for (l in 1:length(yb)){
    problargy[l] <- respfctc(theta - delta[l])
  } 
  plot(y,problargy, type="b",cex.lab=2.0, xlab =names(deca)[indsel[i]],main ="P(Y>y)",ylim=c(0,1),cex.main=2.5,
       lwd=3,  ylab=names(deca)[indsel[j]], pch =1)
}




### Isodifficulties

#parmatrest <- ThrGumb$parmatrix 

dat<-decaselecttr 

par(cex.lab=1,cex.axis=1.8,cex.main=2.20,pch=16,cex=1.0)
par(mar=c(7,6,6,1)+.8)

for(i in 1:(I-1)){
  for (j in (i+1):I){
    widthj<-max(dat[j,])-min(dat[j,])
    minj <- min(dat[j,])-.01*widthj
    maxj <-  max(dat[j,])+.01*widthj
    
    yj <- seq(minj,maxj,(maxj-minj)/6)
    #yi<-0*yj
    yi<- (parmatrest[j,1]+parmatrest[j,2]*yj-parmatrest[i,1])/parmatrest[i,2]
    plot(yj,yi, type="l",cex.lab=2.0, xlab =names(deca)[indsel[j]],
         lwd=3,  ylab=names(deca)[indsel[i]], pch =1)
    slope<-parmatrest[j,2]/parmatrest[i,2]
    
    val<-5
    if(i %in% c(1))val<-4
    
    
    lines(c(yj[val],yj[val]),c(0,yi[val]),type = "l", lty = "dashed",lwd=2)
    lines(c(0,yj[val]),c(yi[val],yi[val]),type = "l", lty = "dashed",lwd=2)
    
    plot(yi,yj, type="l",cex.lab=2.0, xlab =names(deca)[indsel[i]],
         lwd=3,  ylab=names(deca)[indsel[j]], pch =1)
    
    lines(c(yi[val],yi[val]),c(0,yj[val]),type = "l", lty = "dashed",lwd=2)
    lines(c(0,yi[val]),c(yj[val],yj[val]),type = "l", lty = "dashed",lwd=2)
    
    }}
##################

## other order
widthi<-max(dat[i,])-min(dat[i,])
mini <- min(dat[i,])-.01*widthi
maxi <-  max(dat[i,])+.01*widthi

yi <- seq(mini,maxi,(maxi-mini)/4)
#yi<-0*yj
yj<- (parmatrest[i,1]+parmatrest[i,2]*yj-parmatrest[j,1])/parmatrest[j,2]
plot(yi,yj, type="l",cex.lab=2.0, xlab =names(deca)[indsel[i]],
     lwd=3,  ylab=names(deca)[indsel[j]], pch =1)

dev.off()





############################################################
## leaving one out

#decaall<-deca[,1:10]
#decaall[,indrun]<--decaall[,indrun]
i#ndall<-1:10


## new order of variables
decanew<-cbind(deca[,indjump],-deca[,indrun],deca[,indthrow])

### loop one task omitted
corlo<-matrix(0,10,2)
corlowith<-matrix(0,10,3)

for (l in 1:10){

decaleav<-decanew[-l]
indleav<-indall[-l]

decaselect<-decaleav
indsel<-indleav

decaselecttr<-t(decaselect)  # transposed


I<- dim(decaselecttr)[1]
indicatorsingle  <- "C"  #### discrete or continuous
indicatorvec  <- indicatorsingle
for (ll in 2:I )indicatorvec<- c(indicatorvec,indicatorsingle)
indicator<-indicatorvec

cov0<-matrix(0,1,dim(decaselecttr)[2])

Thrleav<-ThreshbasisRespCov(decaselecttr,basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start, 
                          startind = "no",der="der",cov0,hessiander="FALSE")


grid<- seq(-6,6,.05)
parmatrest<-Thrleav$parmatrix[, 1:2]
stdest<-Thrleav$stdmixt

dat<-decaselecttr
tt<-PosteriorEstimatesGen(grid,dat,I, indicatorvec,lin =lin,basis="notspl",ord, numknotsstart,parmatrest[, 1:2],stdest)
tt$thetacen

corobs<-0
maxcor<-0
clat<-abs(cor(tt$thetacen,deca[,l]))
 for (i in indsel){corobs<-corobs+abs(cor(deca[,i],deca[,l]))/9
   print(cor(deca[,i],deca[,l]))
   if (abs(cor(deca[,i],deca[,l]))>maxcor )maxcor<-abs(cor(deca[,i],deca[,l]))
    corlo[l,]<-c(clat,corobs)
    corlowith[l,]<-c(clat,corobs,maxcor)
    }

}  ### end loop


corlo
corlowith
round(corlo, digits=2)

c(names(deca[,indjump]),  names(deca[,indrun]),names(deca[,indthrow]))


####### almost all

decaalmost<-cbind(deca[,indjump],-deca[,indrun],deca[,indthrow])





#########################################################
### Pairs



decaselecttr<-t(decaselect)  # transposed

I<-dim(decaselecttr)[1]
pairsval<-matrix(0,(I-1)*I/2,dim(decaselecttr)[2])
pairs<-matrix(0,(I-1)*I/2,2)


startused<-1  ### if 1 startvalues form NV fit used, generating start values below 

count<-0
for(i in 1:(I-1)){
    for(j in (i+1):I){
      count<-count+1
     
      
    if(startused!=1)ThrM<-ThreshbasisRespCov(decaselecttr[c(i,j),],basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start, 
                                startind = "no",der="der",cov0,hessiander="FALSE")
    
    ### with start values
    if(startused==1)ThrM<-ThreshbasisRespCov(decaselecttr[c(i,j),],basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start=pairsstart[count,],startind = "yes",der="der",cov0,hessiander="FALSE")
    
    grid<- seq(-15,15,.01)
    grid<- seq(-20,20,.01)
    parmatrest<-ThrM$parmatrix[, 1:2]
    stdest<-ThrM$stdmixt
    if(stdest< .1)stdest<-.1  ## correction std dev
    tt<-PosteriorEstimatesGen(grid,decaselecttr[c(i,j),],I=2, indicatorvec,lin =lin,basis="notspl",ord, numknotsstart,parmatrest[, 1:2],stdest)
    pairs[count,]<-c(i,j)
    pairsval[count,]<-tt$thetacen
    
    
     }}
 cor(t(pairsval))
 round(cor(t(pairsval)), digits=2)

 
 # pairs are compared

lengthcomp<-(I-1)*I/2
#lengthcomp<-8
corrmatrix<-matrix(1,lengthcomp,lengthcomp)
comp<--matrix(0,(lengthcomp-1)*lengthcomp/2,5)
dimcomp<-(lengthcomp-1)*lengthcomp/2

count<-0
for(c1 in 1:(lengthcomp-1)){
  for(c2 in (c1+1):lengthcomp){
    count<-count+1
    #plot(pairsval[c1,],pairsval[c2,])
    corr<-cor(pairsval[c1,],pairsval[c2,])
    print(corr)
    corrmatrix[c1,c2]<-corr
    corrmatrix[c2,c1]<-corr
    corrmatrix[c1,c1]<-1
    comp[count,]<-c(pairs[c1,],pairs[c2,],corr)
    }}

round(corrmatrix, digits=2) 
round(comp, digits=2) 

# write 
for (count in 1:dimcomp){
cat("&( ",comp[count,1],",",comp[count,2],") ","&( ",comp[count,3],",",comp[count,4],") ", "& ",round(comp[count,5], digits=2),"\\")

  }
#memall<-comp



##   If start values needed
### generate start values


pairsstart<-matrix(0,(I-1)*I/2,7)
pairs<-matrix(0,(I-1)*I/2,2)


count<-0
for(i in 1:(I-1)){
  for(j in (i+1):I){
    count<-count+1
    
    
    ThrNV<-ThreshbasisRespCov(decaselecttr[c(i,j),],basis="lin",indicatorvec,ord=4, numknotsstart=6,lambdpenin=0,lambdc=0,start, 
                             startind = "no",der="der",cov0,hessiander="FALSE")
    
    pairs[count,]<-c(i,j)
    pairsstart[count,]<-ThrNV$par
  }}



###### varying alpha


## with and without  penalty on alphas 


Thralpha<- ThreshModFixedAlphaResp(decaselecttr,commonslope="var" ,indicator="C", der="der",lin =lin, start= par, 
                                   startind="no",penalpha=0.01,respf) 
Thralpha 

## correct negative discrim par but diverges without penalty
par<-Thralpha$par
par[8:9]<-c(1,1)



### with no penalty does not converge

Thralpha0<- ThreshModFixedAlphaResp(decaselecttr,commonslope="var" ,indicator="C", der="der",lin =lin, start= par, 
                                   startind="yes",penalpha=0,respf) 
##repeat
Thralpha0<- ThreshModFixedAlphaResp(decaselecttr,commonslope="var" ,indicator="C", der="no",lin =lin, start= Thralpha0$par, 
                                    startind="yes",penalpha=0,respf)
Thralpha0$Loglik
Thralpha0$alpha


Thralpha<- ThreshModFixedAlphaResp(decaselecttr,commonslope="var" ,indicator="C", der="der",lin =lin, start= par, 
                                   startind="yes",penalpha=.001,respf) 

Thralphat<- ThreshModFixedAlphaResp(decaselecttr,commonslope="var" ,indicator="C", der="der",lin =lin, start= par, 
                                   startind="yes",penalpha=100,respf) 

alph<-c(0,.01,.1,1,20)
val<-0*alph
for(r in 1:length(alph)){
  Thralpha1<- ThreshModFixedAlphaResp(decaselecttr,commonslope="var" ,indicator="C", der="der",lin =lin, start= par, 
                                                                startind="yes",penalpha=alph[r] ,respf) 

  val[r]<-Thralpha1$Loglik
  }

plot(alph,val, type="b")


Thralpha1$Loglik

Thrcom<- ThreshModFixedAlphaResp(decaselecttr,commonslope="com" ,indicator="C", der="der",lin =lin, start= par, 
                                   startind="no",penalpha=0.1,respf) 
Thralpha$parmatrix


### ok with basis, no varying alpha
Thrcom<- ThreshModFixedResp(decaselecttr,commonslope="var" ,indicator="C", der="der",lin =lin, start= par, 
                            startind="no",respf) 
Thrcom



#### PCA


data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
res.pca3 <- PCA(decathlon, ncp=3,quanti.sup = 11:12, quali.sup=13)
res.pca3 <- PCA(decathlon, ncp=3,quanti.sup = 11:12, quali.sup=13,axes = c(1,2))
res.pca3 <- PCA(decathlon, ncp=3,quanti.sup = 11:12, quali.sup=13,axes = c(2,3))
res.pca3 <- PCA(decathlon, ncp=3,quanti.sup = 11:12, quali.sup=13,axes = c(1,3))

res.pca3$eig



# Principal Component Analysis:

# Clustering, auto nb of clusters:
hc <- HCPC(res.pca3, nb.clust=-1)
### Construct a hierarchical tree from a partition (with 10 clusters)
### (useful when the number of individuals is very important)
hc2 <- HCPC(decathlon, kk=10, nb.clust=-1)
## Graphical interface
require(Factoshiny)
res <- Factoshiny(iris[,1:4])





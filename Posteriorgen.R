

### trial posterior

grid<- seq(-5,5,.1)
parmatrest<-parmatrest[, 1:2]
stdest<-ThrNV$stdmixt

tt<-PosteriorEstimatesGen(grid,dat,I, indicatorvec,lin =lin,basis="notspl",ord, numknotsstart,parmatrest[, 1:2],stdest)

plot(deca$Points,tt)


PosteriorEstimatesGen <-function(grid,dat,I, indicatorvec,lin =lin,basis,ord, numknotsstart,parmatrest,stdest){ 
  
  ####computes centered posterior person parameters
  ######   parmatrest: estimated parameters
  
  
  numgrid <- length(grid)
  dens <- matrix(0,numgrid,1)
  P<-dim(dat)[2]
  esttheta <- matrix(0,P,1)
  datb<- dat
  
  
  ### P-splines
  if(basis =="splines"){
  
  I <- dim(datb)[1]  
  
  minresp <- min(datb)
  maxresp <- max(datb)
  
  
  dif<- maxresp-minresp
  knots <- seq(minresp-(ord-1)*dif/(numknotsstart-1),maxresp+(ord-1)*dif/(numknotsstart-1),dif/(numknotsstart-1))
  
  ### determine number of knots and basis
  sdim <-splineDesign(knots,datb[1,1], ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)
  numknots <- dim(sdim)[2]
  
  bas <- array(0, dim=c(I,P,numknots))
  basderiv <- array(0, dim=c(I,P,numknots))
  basdummy <- array(0, dim=c(I,P,numknots))  #for discrete only
  
  for (i in 1:I){for (p in 1:P){
    bas[i,p,]<- splineDesign(knots,datb[i,p], ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)}}
  
  for (i in 1:I){for (p in 1:P){
    basderiv[i,p,]<- splineDesign(knots,datb[i,p], ord = ord, derivs=1, outer.ok = TRUE, sparse = FALSE)}}
  
  for (i in 1:I){for (p in 1:P){
    datdum <- datb[i,p]-1
    basdummy[i,p,]<- splineDesign(knots,datdum, ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)}}
  }
  # end splines
  
  if(basis !="splines"){
    
    #numcov<-dim(cov)[1]
    #for (i in 1:numcov)indicator<-c(indicator,"C")
    
    #numknots <- 2+numcov
    numknots <- 2
    bas <- array(0, dim=c(I,P,numknots))
    basderiv <- array(0, dim=c(I,P,numknots))
    basdummy <- array(0, dim=c(I,P,numknots)) 
    
    for (i in 1:I){for (p in 1:P)   bas[i,p,]<- c(1,datb[i,p])}
    for (i in 1:I){for (p in 1:P)   basderiv[i,p,]<- c(0,1)}
    #for (i in 1:I){for (p in 1:P)   bas[i,p,]<- c(1,diffunct(0,1,datb[i,p]),-cov[,p])}
    #for (i in 1:I){for (p in 1:P)   basderiv[i,p,]<- c(0,derdiffunct(0,1,datb[i,p]),0*cov[,p])}
    
    for (i in 1:I){for (p in 1:P){
      datdum <- datb[i,p]-1
      #basdummy[i,p,]<- c(1,datdum)
      #basdummy[i,p,]<- c(1,diffunct(0,1,datdum),-cov[,p])  ### here covariates
      basdummy[i,p,]<- c(1,diffunct(0,1,datdum))
      }}
    
    
  } # end not splines
  
   
   
  
  ###
  
  for (p  in 1:P){obs<- dat[,p]
  for (l in 1:numgrid){
    theta <- grid[l]
    #dens[l,1]<-prodfct(theta,dat=obs,I=I,parmatr=parmatrest,slope=1,indicator=indicator, lin =lin)*dnorm(theta,0,stdest)
    dens[l,1]<-prodfctIntExtBsplINVecTrial(theta,dat=obs,I=I,parmatrest=parmatrest,slope=1,
                                      indicator=indicatorvec,bas,basderiv,basdummy,pers=p,numknots=numknots)*dnorm(theta,0,stdest)
  }
  num<-  which.max(dens)
  esttheta[p]<- grid[num]
  }
  
  #### centering
  estthetac<-esttheta-mean(esttheta)
  
  newList <- list("theta" = esttheta,"thetacen"=estthetac)
  return(newList)
  
}
###### end fct  




plot(grid,dens)

prodfctIntExtBsplINVecTrial  <-function(theta,datitem=datitem,I=I,parmatrest=parmatrest,slope=slope,
                                   indicator=indicator, bas,basderiv,basdummy,pers=pers,numknots=numknots){
  #### with std mixt only indicator C modified
  ### datitem vector length I for fixed person
  #####slope on y is 1
  prod <-1    
  
  #dimBspl<-length(par)-1
  #parBspl <- par[1:dimBspl]
  #itemmatrix<-  t(matrix(parBspl,numknots,I)) ##I x numknots matrix
  
  for (i in 1:I){
    #iteml <- itemmatrix[i,]
    iteml <- parmatrest[i,]
    item=i
    if(indicator[i]=="C"){
      prod <- prod*deltalinNVdensBsplI(iteml,slope , theta, bas,basderiv,basdummy=basdummy,
                                       item=item,pers=pers)} 
    if(indicator[i]!="C"){
      prod <- prod*deltalinNVdensDiscrBsplI(datitem[i],iteml,slope , theta, bas,basderiv,basdummy=basdummy,
                                            item=item,pers=pers,indicator[i])}
    
    #if(indicator[i]=="D0"){
    #  prod <- prod*deltalinNVdensDiscrBsplI(datitem[i],iteml,slope , theta, bas,basderiv,basdummy=basdummy,
    #                                        item=item,pers=pers,indicator[i])}
    #if(indicator[i]=="D1"){
    #  prod <- prod*deltalinNVdensDiscrBsplI(datitem[i],iteml,slope , theta, bas,basderiv,basdummy=basdummy,
    #                                        item=item,pers=pers,indicator[i])} ##idetisch mit D0
  }
  
  return(prod)}  





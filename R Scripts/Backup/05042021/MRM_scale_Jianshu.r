install.packages("ieggr",repos = c("ftp://gr:gr!123@129.15.40.254","http://cran.us.r-project.org"))

library(ecodist)
library(vegan)
library(ieggr)
library(parallel)

MRMonce<-function(m, mrank = FALSE,standard.cc=FALSE,geoscale=c(0,Inf),include.geo=TRUE){
  
  m<-m[m[,2]>=geoscale[1] & m[,2]< geoscale[2],]
  
  if(mrank) {
    m <- apply(m, 2, rank)
  }
  
  
  if(standard.cc){
    m<-scale(m)
  }
  
  
  X <- m[ ,2:ncol(m), drop=FALSE]
  X <- cbind(rep(1, nrow(X)), X)
  Y <- m[ ,1, drop=FALSE]
  
  if(include.geo){
    X<-X
  }else {X<-X[,-2]}
  
  nd <- nrow(X)
  
  # only need to calculate (X'X)^-1 once
  XX <- crossprod(X)
  XX <- solve(XX)
  
  # will need to calculate Xy for each permutation
  XY <- crossprod(X, Y)
  YY <- crossprod(Y)
  
  # regression coefficients
  b <- XX %*% XY
  rownames(b) <- c("Int", colnames(X)[2:ncol(X)])
  
  bXY <- crossprod(b, XY)
  SSE <- YY - bXY
  
  SSTO <- YY - sum(Y)^2/nd
  SSR = SSTO - SSE
  
  # R2 = 1 - SSE/SSTO
  R2 <- 1 - SSE/SSTO
  R2 <- as.vector(R2)
  
  # F* = MSR / MSE
  # MSR = SSR / (p - 1) 
  # MSE = SSE / (n - p)
  #b / sqrt(1 - R2)#
  p <- ncol(X) # number of parameters estimated
  F.value <- (SSR / (p - 1)) / (SSE / (nd - p))
  b.t<-b/sqrt(1-R2)
  result<-list(b.t=b.t,b=b,R2=R2,F.value=F.value)
  result
}

####
MRMnew<-function(formula = formula(data), data = sys.parent(), mrank = FALSE,standard.cc=FALSE,geoscale=c(0,Inf),nperm=999,no_cores=4,include.geo=TRUE){
  
  m <- match.call(expand.dots = FALSE)
  m2 <- match(c("formula", "data"), names(m), nomatch=0)
  m <- m[c(1, m2)]
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  m <- as.matrix(m)
  observed<-MRMonce(m=m,mrank =mrank, standard.cc=standard.cc,geoscale=geoscale,include.geo=include.geo)
  c1<-makeCluster(no_cores)
  permresult<-parSapply(c1,1:nperm,function(i){
    m2=m
    newY <- ecodist::full(m2[,1])
    newSample <- sample(nrow(newY))
    newY <- newY[newSample, newSample]
    m2[,1] <- ecodist::lower(newY)
    
    m2<-m2[m2[,2]>=geoscale[1] & m2[,2]< geoscale[2],]
    
    if(mrank) {
      m2 <- apply(m2, 2, rank)
    }
    
    
    if(standard.cc){
      m2<-scale(m2)
    }
    
    
    X <- m2[ ,2:ncol(m2), drop=FALSE]
    X <- cbind(rep(1, nrow(X)), X)
    Y <- m2[ ,1, drop=FALSE]
    
    if(include.geo){
      X<-X
    }else {X<-X[,-2]}
    
    
    
    nd <- nrow(X)
    
    # only need to calculate (X'X)^-1 once
    XX <- crossprod(X)
    XX <- solve(XX)
    
    # will need to calculate Xy for each permutation
    XY <- crossprod(X, Y)
    YY <- crossprod(Y)
    
    # regression coefficients
    b <- XX %*% XY
    rownames(b) <- c("Int", colnames(X)[2:ncol(X)])
    
    bXY <- crossprod(b, XY)
    SSE <- YY - bXY
    
    SSTO <- YY - sum(Y)^2/nd
    SSR = SSTO - SSE
    
    # R2 = 1 - SSE/SSTO
    R2 <- 1 - SSE/SSTO
    R2 <- as.vector(R2)
    
    # F* = MSR / MSE
    # MSR = SSR / (p - 1) 
    # MSE = SSE / (n - p)
    #b / sqrt(1 - R2)#
    p <- ncol(X) # number of parameters estimated
    F.value <- (SSR / (p - 1)) / (SSE / (nd - p))
    b.t<-b/sqrt(1-R2)
    result<-list(b.t=b.t,b=b,R2=R2,F.value=F.value)
    result
  },simplify = F)
  
  stopCluster(c1)
  ##combine permutations, generate a list of 4 elements##
  permresult2<-do.call(mapply, c(cbind, permresult))
  
  R2.all<-c(observed$R2,as.vector(t(permresult2$R2)))
  R2.pval <- length(R2.all[R2.all >= R2.all[1]])/(1+nperm)
  
  F.all <- c(observed$F.value,as.vector(t(permresult2$F.value)))
  F.pval <- length(F.all[F.all >= F.all[1]])/(1+nperm)
  
  # b.all contains pseudo-t of Legendre et al. 1994
  b.all<-t(cbind(observed$b.t,permresult2$b.t))
  b.pval <- apply(b.all, 2, function(x)length(x[abs(x) >= abs(x[1])])/(1+nperm))
  
  results <- list(coef=cbind(b=observed$b, pval=b.pval), r.squared=c(R2=observed$R2, pval = R2.pval),F.test=c(F.value=observed$F.value, F.pval = F.pval))
  results
  
}


#### data input
setwd("E:/Xiangyun")
com="file:///E:/Xiangyun/16s 75-100Weighted UniFrac Qun.csv"
env="file:///E:/Xiangyun/16s env no na.csv"
geo="file:///E:/Xiangyun/GW.geodist.csv"
comm=lazyopen(com)
env=lazyopen(env)
geo=lazyopen(geo)
sam.name=match.name(rn.list = list(env=env), both.list = list(comm=comm, geo=geo))
comm=sam.name$comm
env=sam.name$env
geo=sam.name$geo
min=min(geo)
Y=as.dist(log(comm))
x1=as.dist(log(geo+1e-7))

x2=vegdist(scale(env$DOC),"euclidean")

x3=vegdist(scale(env$DTN),"euclidean")

x4=vegdist(scale(env$NH4_N),"euclidean")

x5=vegdist(scale(env$NO3_N),"euclidean")

x6=vegdist(scale(env$VWC),"euclidean")

x7=vegdist(scale(env$soil.T),"euclidean")


### MRM at different scales
##overall scale
MRM.overall=MRMnew(Y ~ x1+x2+x3+x4+x5+x6+x7,standard.cc = T,no_cores=5)
MRM.overall

##micro scale
scale=c(0,log(403))
MRM.micro=MRMnew(Y ~ x1+x2+x3+x4+x5+x6+x7,standard.cc = T,no_cores=5, geoscale =scale)
MRM.micro

##meso scale
scale=c(6,11)
MRM.meso=MRMnew(Y ~x1+x2+x3+x4+x5+x6+x7,standard.cc = T,no_cores=5, geoscale =scale)
MRM.meso

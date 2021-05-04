# MMRR performs Multiple Matrix Regression with Randomization analysis
# Y is a dependent distance matrix
# X is a list of independent distance matrices (with optional names)

Y=dist.amf
X= Xmats
nperm=999


MMRR<-function(Y,X,nperm=999){
  #compute regression coefficients and test statistics
  nrowsY<-nrow(Y)
  y<-unfold(Y)
  if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
  Xmats<-sapply(X,unfold)
  fit<-lm(y~Xmats)
  coeffs<-fit$coefficients
  summ<-summary(fit)
  r.squared<-summ$r.squared
  tstat<-summ$coefficients[,"t value"]
  Fstat<-summ$fstatistic[1]
  tprob<-rep(1,length(tstat))
  Fprob<-1
  
  # relative importance
  relimp <- calc.relimp(fit, type = c("lmg"), rela = TRUE) 
  
  
  #perform permutations
  for(i in 1:nperm){
    rand<-sample(1:nrowsY)
    Yperm<-Y[rand,rand]
    yperm<-unfold(Yperm)
    fit<-lm(yperm~Xmats)
    summ<-summary(fit)
    Fprob<-Fprob+as.numeric(summ$fstatistic[1]>=Fstat)
    tprob<-tprob+as.numeric(abs(summ$coefficients[,"t value"])>=abs(tstat))
  }
  
  #return values
  tp<-tprob/(nperm+1)
  Fp<-Fprob/(nperm+1)
  names(r.squared)<-"r.squared"
  names(coeffs)<-c("Intercept",names(X))
  names(tstat)<-paste(c("Intercept",names(X)),"(t)",sep="")
  names(tp)<-paste(c("Intercept",names(X)),"(p)",sep="")
  names(Fstat)<-"F-statistic"
  names(Fp)<-"F p-value"
  return(list(r.squared=r.squared,
              coefficients=coeffs,
              tstatistic=tstat,
              tpvalue=tp,
              Fstatistic=Fstat,
              Fpvalue=Fp,
              relimp = relimp))
}

# unfold converts the lower diagonal elements of a matrix into a vector
# unfold is called by MMRR

unfold<-function(X){
  x<-vector()
  for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
  x<-scale(x, center=TRUE, scale=TRUE)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
  return(x)
}


# Tutorial for data files gendist.txt, geodist.txt, and ecodist.txt

# Read the matrices from files.
# The read.matrix function requires {tseries} package to be installed and loaded.
# If the files have a row as a header (e.g. column names), then specify 'header=TRUE', default is 'header=FALSE'.
library(tseries)

df <- mono_a$df
df <- mono_w$df
df <- all_wa$df

df <- df %>% mutate(Key = as.integer(Key)) 
row.names(df) <- df$Key
amf_otu <- df%>% dplyr::select(contains("OTU"))
dist.amf <- as.matrix(vegdist(amf_otu, "bray"))

geo <- df %>% dplyr::select("Long_point", "Lat_point")
dist.geo <- as.matrix(as.dist(distm(geo, fun = distHaversine) ))
dist.pH <- as.matrix(dist(df %>% dplyr::select("pH")))
dist.P <- as.matrix(dist(df %>% dplyr::select("P")))
dist.N <- as.matrix(dist(df %>% dplyr::select("N")))
dist.TOC <- as.matrix(dist(df %>% dplyr::select("TOC")))
dist.FarmMgmt <- as.matrix(dist(df %>% dplyr::select("FarmBi")))

# genMat <- read.matrix("MMRRtutorial/gendist.txt")
# geoMat <- read.matrix("MMRRtutorial/geodist.txt")
# ecoMat <- read.matrix("MMRRtutorial/ecodist.txt")

# Make a list of the explanatory (X) matrices.
# Names are optional.  Order doesn't matter.
# Can include more than two matrices, if desired.
Xmats <- list(geography=dist.geo,pH=dist.pH, P=dist.P, N=dist.N, TOC=dist.TOC, FarmMgmt=dist.FarmMgmt)
# Xmats <- list(geography=dist.geo,pH=dist.pH, P=dist.P, N=dist.N, TOC=dist.TOC)

# Run MMRR function using genMat as the response variable and Xmats as the explanatory variables.
# nperm does not need to be specified, default is nperm=999)
MMRRoutput <- MMRR(dist.amf,Xmats,nperm=999)

MMRRtable <- cbind(coefficent=MMRRoutput$coefficients, t=MMRRoutput$tstatistic, p=MMRRoutput$tpvalue)
round(MMRRtable,3)

# These data should generate results of approximately:
# Coefficient of geography = 0.778 (p = 0.001)
# Coefficient of ecology = 0.167 (p = 0.063)
# Model r.squared = 0.727 (p = 0.001)
# Note that significance values may change slightly due to the permutation procedure.

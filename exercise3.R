#Xuyi Guan
#Exercise3
#MA415
#########################
#1
#a
tmpFn1<-function(xVec)
{
  xVec^(1:length(xVec))
}
tmpFn2<-function(xVec)
{
  n<-length(xVec)
  (xVec^(1:n))/(1:n)
}
#b
tmpFn3<-function(x,n)
{
  1+sum((x^(1:n))/(1:n))
}
###########################
#2
tmpFn<-function(xVec)
{
  n<-length(xVec)
  (x[1:(n-2)]+x[2:(n-1)]+x[3:n])/3
}
tmpFn(c(1:5,6:1))
##########################
#3
tmpFn<-function(x)
{
  ifelse(x>=2, x^2 + 4*x - 7 , ifelse(x>=0, x+3, x^2 + 2*x + 3))
}
tmp <- seq(-3, 3, length=100)
tmpFn(tmp)
tmp
plot(tmp,tmpFn(tmp))
###########################
#4
tmpFn4<-function(mat)
{
  mat[mat%%2==1]<- 2*mat[mat%%2==1]
  mat
}  
A<-matrix(c(1,1,3,5,2,6,-2,-1,-3),nr=3,byrow=TRUE)
tmpFn4(A)
###########################
#5
#n=5, k=2:
tmp <- diag(2,nr=5)
tmp[abs(row(tmp)-col(tmp))==1] <-1
tmp
#general:
tmpFn5<-function(n,k)
{
  tmp<-diag(k,nr=n)
  tmp[abs(row(tmp)-col(tmp))==1]<-1
  tmp
}
##########################
#6
quadrant<-function(alpha)
{
  1=(alpha%%360)%%90
}
##########################
#7
weekday <- function(day,month,year)
{
  month <- month-2
  if(month <= 0) {
    month <- month+12
    year <- year-1
   }
  cc <- year%/%100
  year <- year%%100
  tmp <- floor(2.6*month-0.2)+day+year+year%/%4+cc%/%4-2*cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}  
c( weekday(27,2,1997), weekday(18,2,1940), weekday(21,1,1963) )
###########################
#8
#a
testloop<-function(n)
{
  xVec<-rep(NA,n-1)
  xVec[1]<-1
  xVec[2]<-2
  for(j in 3:(n-1))
      xVec[j]<-xVec[j-1]+2/xVec[j-1]
      xVec
}  
#b
testloopb<-function(yVec)
{
  n<-length(yVec)
  sum(exp(seq(along=yVec)))
}  
###########################
#9
#a
quadmap<- function(start,rho,niter)
{
  xVec<-rep(NA,niter)
  xVec[1]<-start
  for(i in 1:(niter-1)){
    xVec[i+1]<-rho*xVec[i]*(1-xVec[i])
  }
  x
}
#b
quadb<- function(start,rho,eps=0.02)
{
  x1<-start
  x2<-rho*x1*(1-x1)
  niter<-1
  while(abs(x1-x2)>=eps){
    x1<-x2
    x2<-rho*x1*(1-x1)
    niter<-niter+1
  }
  niter
}
#############################
#10
#a
tmpFn10<-function(xVec)
{
  x10<-xVec-mean(xVec)
  x11<-sum(x10^2)
  n<-length(x)
  r1<-sum(x10[2:n]*x10[1:(n-1)])/x11
  r2<-sum(x10[3:n]*x10[1:(n-2)])/x11
  list(r1=r1,r2=r2)
}  
#b
tmpFn10b<-function(x,k)
{
  x10b<-x-mean(x)
  x11b<-sum(x10b^2)
  n<- length(x)
  tmpFn<- function(j){sum(x10b[(j+1):n]*xc[1:(n-j)])/x11b}
  c(1,sapply(1:k,tmpFn))
}  
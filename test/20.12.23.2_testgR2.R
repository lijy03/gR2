#First, clean and rebuild

set.seed(1)
#Set parameters
if(TRUE){
  n<-200 #200 people total.
  p<-runif(1,min=0.3,max=0.4) #30% to 40% of all people are in Group 1.
  n1<-round(p*n) #Number of people in Group 1 (x=0)
  n2<-n-n1 #Number of people in Group 2 (x=1)

  #Probability vector for s=0, 1, or 2
  prob<-c(0.7,0.2,0.1)
  #intercept1, intercept2, slope1, slope2
  intercept1<-1
  intercept2<-3
  slope1<-1
  slope2<--0.8
  #Sd of epsilon
  sd<-0.5
}

#Generate data
if(TRUE){
  x1<-rep(0,n1)
  x2<-rep(1,n2)
  x<-c(x1,x2)
  s1<-sample(0:2,size=n1,replace=TRUE,prob=prob)
  s2<-sample(0:2,size=n2,replace=TRUE,prob=prob)
  s<-c(s1,s2)
  g1<-intercept1+slope1*s1+rnorm(n1,mean=0,sd=sd)
  g2<-intercept2+slope2*s2+rnorm(n2,mean=0,sd=sd)
  g<-c(g1,g2)
  data<-as.data.frame(cbind(x,s,g))
}

set.seed(1)
gR2Result<-gR2::gR2(x=s,y=g,K=2,regressionMethod="LM",
                    inference=TRUE,gR2.pop=0.1,
                    genotypeVector=s)
cor(gR2Result$membership,x) #-0.7695047
data$gR2Membership<-gR2Result$membership #gR2Membership tends to agree with x, the true membership.

gR2Result<-gR2::gR2(x=s,y=g,cand.Ks=1:2,regressionMethod="LM",
                    inference=TRUE,gR2.pop=0.1,
                    genotypeVector=s)









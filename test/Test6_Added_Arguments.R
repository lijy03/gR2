#First, clean and rebuild

library(gR2)
source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions1_simulateData.R")

data<-simulateData(seed=7)
x<-data[,1]
y<-data[,2]
z<-data[,3]

#Scenario 4: unspecified K chosen, no inference, MA
#Output: a list of three item: estimate, K, membership
result4<-gR2(x,y,K=2,details=TRUE)
result4

#Scenario 5: unspecified K chosen, inference (general), MA
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result5<-gR2(x,y,K=2,inference=TRUE,details=TRUE)
result5

#Scenario 6: unspecified K chosen, inference (bivariate normal), MA
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result6<-gR2(x,y,K=2,inference=TRUE,method="binorm",details=TRUE)
result6

#Scenario 7: unspecified K not chosen, no inference, MA
#Output: a list of three item: estimate, K, membership
result7<-gR2(x,y,details=TRUE)
result7

#Scenario 8: unspecified K not chosen, inference (general), MA
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result8<-gR2(x,y,inference=TRUE,details=TRUE)
result8

#Scenario 9: unspecified K not chosen, inference (bivariate normal), MA
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result9<-gR2(x,y,inference=TRUE,method="binorm",details=TRUE)
result9

#Scenario 10: unspecified K chosen, no inference, LM
#Output: a list of three item: estimate, K, membership
result10<-gR2(x,y,K=2,regressionMethod="LM",details=TRUE)
result10

#Scenario 11: unspecified K chosen, inference (general), LM
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result11<-gR2(x,y,K=2,regressionMethod="LM",inference=TRUE,details=TRUE)
result11

#Scenario 12: unspecified K chosen, inference (bivariate normal), LM
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result12<-gR2(x,y,K=2,regressionMethod="LM",inference=TRUE,method="binorm",details=TRUE)
result12

#Scenario 13: unspecified K not chosen, no inference, LM
#Output: a list of three item: estimate, K, membership
result13<-gR2(x,y,regressionMethod="LM",details=TRUE)
result13

#Scenario 14: unspecified K not chosen, inference (general), LM
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result14<-gR2(x,y,regressionMethod="LM",inference=TRUE,details=TRUE)
result14

#Scenario 15: unspecified K not chosen, inference (bivariate normal), LM
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result15<-gR2(x,y,regressionMethod="LM",inference=TRUE,method="binorm",details=TRUE)
result15

#Check that the result is reasonable when cand.Ks is customized
result16<-gR2(x,y,cand.Ks=1:5,details=TRUE)
result16

#Check that the result is reasonable when cand.Ks is customized
result17<-gR2(x,y,cand.Ks=2:5,details=TRUE)
result17


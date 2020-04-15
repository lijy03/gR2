#First, clean and rebuild package.

#This file shows that gR2 produces reasonable results for various scenarios.

library(gR2)
source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions1_simulateData.R")

#Basic categories: specified, unspecified K chosen, unspecified K not chosen
#Inference: no inference, inference (general), inference (bivariate normal)
#If unspecified: MA, LM
#Total number of scenarios: 3(specified)+3(unspecified K chosen)+3(unspecified K not chosen)+6(LM)=15
#If specified + no inference, then output a list of one item: estimate.
#If specified + inference, then output a list of four items: estimate, conf.level, conf.int, and p.val.
#If unspecified + no inference, then output a list of three item: estimate, K, membership.
#If unspecified + inference, then output a list of six items: estimate, conf.level, conf.int, p.val, K, membership.
data<-simulateData(seed=7)
x<-data[,1]
y<-data[,2]
z<-data[,3]

#Scenario 1: specified, no inference
#Output: a list of one item: estimate
result1<-gR2(x,y,z)
result1

#Scenario 2: specified, inference (general)
#Output: a list of four items: estimate, conf.level, conf.int, and p.val
result2<-gR2(x,y,z,inference=TRUE)
result2

#Scenario 3: specified, inference (bivariate normal)
#Output: a list of four items: estimate, conf.level, conf.int, and p.val
result3<-gR2(x,y,z,inference=TRUE,method="binorm")
result3

#Scenario 4: unspecified K chosen, no inference, MA
#Output: a list of three item: estimate, K, membership
result4<-gR2(x,y,K=2)
result4

#Scenario 5: unspecified K chosen, inference (general), MA
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result5<-gR2(x,y,K=2,inference=TRUE)
result5

#Scenario 6: unspecified K chosen, inference (bivariate normal), MA
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result6<-gR2(x,y,K=2,inference=TRUE,method="binorm")
result6

#Scenario 7: unspecified K not chosen, no inference, MA
#Output: a list of three item: estimate, K, membership
result7<-gR2(x,y)
result7

#Scenario 8: unspecified K not chosen, inference (general), MA
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result8<-gR2(x,y,inference=TRUE)
result8

#Scenario 9: unspecified K not chosen, inference (bivariate normal), MA
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result9<-gR2(x,y,inference=TRUE,method="binorm")
result9

#Scenario 10: unspecified K chosen, no inference, LM
#Output: a list of three item: estimate, K, membership
result10<-gR2(x,y,K=2,regressionMethod="LM")
result10

#Scenario 11: unspecified K chosen, inference (general), LM
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result11<-gR2(x,y,K=2,regressionMethod="LM",inference=TRUE)
result11

#Scenario 12: unspecified K chosen, inference (bivariate normal), LM
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result12<-gR2(x,y,K=2,regressionMethod="LM",inference=TRUE,method="binorm")
result12

#Scenario 13: unspecified K not chosen, no inference, LM
#Output: a list of three item: estimate, K, membership
result13<-gR2(x,y,regressionMethod="LM")
result13

#Scenario 14: unspecified K not chosen, inference (general), LM
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result14<-gR2(x,y,regressionMethod="LM",inference=TRUE)
result14

#Scenario 15: unspecified K not chosen, inference (bivariate normal), LM
#Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
result15<-gR2(x,y,regressionMethod="LM",inference=TRUE,method="binorm")
result15

#Check that the result is reasonable when cand.Ks is customized
result16<-gR2(x,y,cand.Ks=1:5)
result16

#Check that the result is reasonable when cand.Ks is customized
result17<-gR2(x,y,cand.Ks=2:5)
result17


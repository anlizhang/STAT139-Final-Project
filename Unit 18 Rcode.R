
#read in the reference data set
mlbdata=read.csv("mlb_batting_data.csv")
dim(mlbdata)
head(mlbdata)
names = as.character(unique(mlbdata$Name))
length(names)


install.packages('lme4')
library(lme4)

lmm1 = lmer(BA~Age+Age2 + (1|Name), data=mlbdata)
coef(lmm1)$Name[sum(names < "Ozzie Smith")+1,]
coef(lmm1)$Name[sum(names < "Barry Bonds")+1,]
coef(lmm1)$Name[sum(names < "Lou Merloni")+1,]


lmm2 = lmer(BA~Age+Age2-1 + (Age|Name), data=mlbdata)
coef(lmm2)$Name[sum(names < "Ozzie Smith")+1,]
coef(lmm2)$Name[sum(names < "Barry Bonds")+1,]
coef(lmm2)$Name[sum(names < "Lou Merloni")+1,]


lmm3 = lmer(BA~Age+Age2-1 + (Age+Age2|Name), data=mlbdata, weights = AB)
coef(lmm3)$Name[sum(names < "Ozzie Smith")+1,]
coef(lmm3)$Name[sum(names < "Barry Bonds")+1,]
coef(lmm3)$Name[sum(names < "Lou Merloni")+1,]

x=seq(18,45,.1)
X=cbind(1,x,x^2)

barry = X%*%t(coef(lmm3)$Name[sum(names < "Barry Bonds")+1,])
lou = X%*%t(coef(lmm3)$Name[sum(names < "Lou Merloni")+1,])
ozzie = X%*%t(coef(lmm3)$Name[sum(names < "Ozzie Smith")+1,])

plot(BA~jitter(Age),data = mlbdata,cex=0.3,col="gray",mar=2,ylim=c(0,0.5),xlim=c(18,45))
lines(ozzie~x,col="orange",lwd=3)
lines(barry~x,col="blue",lwd=3)
lines(lou~x,col="limegreen",lwd=3)

lmm3_w = lmer(BA~Age+Age2-1 + (Age+Age2|Name), data=mlbdata, weights = AB)
coef(lmm3_w)$Name[sum(names < "Ozzie Smith")+1,]
coef(lmm3_w)$Name[sum(names < "Barry Bonds")+1,]
coef(lmm3_w)$Name[sum(names < "Lou Merloni")+1,]

barry_w = X%*%t(coef(lmm3_w)$Name[sum(names < "Barry Bonds")+1,])
lou_w = X%*%t(coef(lmm3_w)$Name[sum(names < "Lou Merloni")+1,])
ozzie_w = X%*%t(coef(lmm3_w)$Name[sum(names < "Ozzie Smith")+1,])
spike_w = X%*%t(coef(lmm3_w)$Name[sum(names < "Spike Owen")+1,])

lines(ozzie_w~x,col="orange",lwd=3,lty=2)
lines(barry_w~x,col="blue",lwd=3,lty=2)
lines(lou_w~x,col="limegreen",lwd=3,lty=2)
lines(spike_w~x,col="skyblue",lwd=3,lty=2)

barrybonds=levels(mlbdata$Name)[206]
points(BA~Age,data=mlbdata,subset=which(mlbdata$Name==barrybonds),col="blue",pch=16)


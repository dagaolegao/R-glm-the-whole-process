#1
date<- read.csv("SpeedDating.csv")
table(date$DecisionM,date$DecisionF)/nrow(date)
nrow(date)

#2
second.date<- rep(0,nrow(date))
second.date[which((date$DecisionM==1)&(date$DecisionF==1)==TRUE)]<- 1
second.date
date.2<- cbind(date,second.date)

index.2<- seq(1,21,2)
par(mfrow=c(3,4))
colnames(date)[complete.cases(t(date))]#find which col contanins NA.

par(mfrow=c(3,3))
plot(jitter(date.2$LikeM), jitter(date$LikeF),las=TRUE, col=as.factor(date.2$second.date), main="Like")
plot(jitter(date.2$PartnerYesM), jitter(date$PartnerYesF),las=TRUE,col=as.factor(date.2$second.date), main="Partner")
plot(jitter(date.2$AttractiveM), jitter(date$AttractiveF),las=TRUE, col=as.factor(date.2$second.date), main="Attractive")
plot(jitter(date.2$SincereM), jitter(date$SincereF),las=TRUE, col=as.factor(date.2$second.date), main="Sincere")
plot(jitter(date.2$IntelligentM), jitter(date$IntelligentF),las=TRUE, col=as.factor(date.2$second.date), main="Intelligent")
plot(jitter(date.2$FunM), jitter(date$FunF),las=TRUE,  col=as.factor(date.2$second.date), main="Fun")
plot(jitter(date.2$AmbitiousM), jitter(date$AmbitiousF),las=TRUE, col=as.factor(date.2$second.date), main="Ambitious")
plot(jitter(date.2$SharedInterestsM), jitter(date$SharedInterestsF),las=TRUE, col=as.factor(date.2$second.date), main="SharedInterests")


#3
date.4<- date.2[complete.cases(date.2),]
date.numeric<- date.4[,-c(1,2,7,8,9,10,23)]
i=1
date.4.old<- date.2[,-c(1,2,7,8,9,10,23)]#find NA

for (i in 1:8){
  index.na<-is.na(date.4.old[i])
  print(length(date.4.old[i][index.na]))
}#print the NA in the numerical variables.

complete.cases(t(date.4.old))
colnames(date.4.old)
for (i in 1:ncol(date.numeric)){
  print(i)
  index.find0<- print(which(date.numeric[,i]<1))
  date.numeric[index.find0,i]<- 1
  index.find11<- print(which(date.numeric[,i]>10))
  date.numeric[index.find11,i]<- 10
}
date.4[,-c(1,2,7,8,9,10,23)]<- date.numeric
date.4
#4
#dev.off()
table.raceM<- table(date$RaceM)
table.raceF<- table(date$RaceF)
race<- c(as.character(date$RaceM),as.character(date$RaceF))
gender<- c(rep("male",nrow(date)), rep("female",nrow(date)))
race.table<- table(race,gender)
mosaicplot(table(date.4$RaceM,date.4$RaceF),xlab = "male",ylab = "female",main = "race classification")

nrow(date.4)
#5
glm.5<- glm(data = date.4,second.date~
    LikeM+LikeF+PartnerYesM+PartnerYesF+AgeM
    +AgeF+RaceM+RaceF+AttractiveM+AttractiveF 
    +SincereM+SincereF+IntelligentM+IntelligentF+FunM
    +FunF+AmbitiousM+AmbitiousF+SharedInterestsM+SharedInterestsF,family=binomial(link="logit"))
summary(glm.5)


glm.test<- glm(data= date.4, second.date~FunF+LikeM+PartnerYesF+PartnerYesM,family=binomial(link="logit"))
summary(glm.test)
step(glm.test)
plot(glm.test)

library(usdm)
vif(date.2[c(3,6,7,18)])
#6
table(date.4$DecisionM,date.4$DecisionF)/200
#7
#8
predict<- glm.test$fitted.values
length(predict)

accuracy<- function(td){   #calculate the accuracy
  predict[glm.test$fitted.values>td]<- "predict date"
  predict[glm.test$fitted.values<td]<- "predict no date"
  table.pre.real<-table(predict,date.4$second.date)
  acc<- (table.pre.real[1,2]+table.pre.real[2,1])/(table.pre.real[1,1]+table.pre.real[2,2]+table.pre.real[1,2]+table.pre.real[2,1])
  return(acc)
}
accuracy.array<- array()
td<- seq(from=0.2,to=0.7,by=0.01)
for (i in td){
  print(i)
  accuracy.array<- c(accuracy.array,accuracy(i))
}
accuracy.array
accuracy.array<- accuracy.array[-1]
#dev.off()
plot(td,accuracy.array)
title("accuracy with threshold")
best.td<- optimise(f = accuracy,lower = -0.2,upper = 1, maximum = TRUE)
#9
index.matrix<- function(td=best.td){   #calculate the accuracy
  predict[glm.test$fitted.values>td]<- "predict date"
  predict[glm.test$fitted.values<td]<- "predict no date"
  table.pre.real<-table(predict,date.4$second.date)
  acc<- (table.pre.real[1,2]+table.pre.real[2,1])/(table.pre.real[1,1]+table.pre.real[2,2]+table.pre.real[1,2]+table.pre.real[2,1])
  sensi<- (table.pre.real[1,2])/(table.pre.real[1,1]+table.pre.real[1,2])
  speci<- (table.pre.real[2,1])/(table.pre.real[2,1]+table.pre.real[2,2])
  return(c(acc,sensi,speci))
}
index.matrix()

require(pROC)#draw ROC
y.hat<- glm.test$fitted.values
y.hat[y.hat<best.td]<- 0
y.hat[y.hat>best.td]<- 1
#dev.off()
roc(response=date.4$second.date, predictor=y.hat, plot=TRUE, las=TRUE, legacy.axes=TRUE, lwd=5,
    main="ROC for second date", cex.main=1.6, cex.axis=1.3, cex.lab=1.3)
  
Press<- function(i){ #jackknife
  glm.test<- glm(data = date.4[-i,],second.date~FunF+LikeM+PartnerYesF+PartnerYesM)
  y.i<- predict(glm.test,date.4[i,])
  if(y.i<best.td$maximum){
    y.i<- 0
  }else{
    y.i<- 1
  }
  return((date.4$second.date[i]-y.i)^2)
}
press.a<- array()
for (i in 1:200){
  press.a<- c(press.a,Press(i))
}
Press.a<-press.a[-1]
sum(Press.a)#nominator
sum((date.4$second.date-mean(date.4$second.date))^2)#denominator
R.jacknife<- sum(Press.a^2)/sum((date.4$second.date-mean(date.4$second.date))^2)
R.MSE.jacknife<- (sum(Press.a^2)/(200-(4+1)))^0.5



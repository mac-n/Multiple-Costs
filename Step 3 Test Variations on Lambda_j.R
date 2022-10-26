require(caret)
require(FSelector)
require(pROC)

#split data for training and testing
trainIndex <- createDataPartition(joinednewcut$CDRSB, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
trainData<-joinednewcut[trainIndex,]
testData<-joinednewcut[-trainIndex,]

lamdalist=0:15
lamdalist<-lamdalist/1000
lamda2list<-0:20
lamda2list<-lamda2list/50000
resultslookup<-matrix(nrow=(length(lamdalist)*length(lamda2list)),ncol=2)
rowname=0
resultslist<-list()
#
#first loop: vary lambda1
for (i in 1:length(lamdalist)){
#second loop: vary lamda2
  for (j in 1:length(lamda2list)){
    rowname=rowname+1
    temp<-cost_cfs_2(CDRSB ~.,lamda=lamdalist[i],costs=newcostscut[,2],trainData,budget=10000,costs2=newcostscut[,3],lamda2=lamda2list[j],budget2=1000)
  resultslist[[rowname]]<-temp
  resultslookup[rowname,]<-c(i,j)
  print(c(i,j))
 }
}
#most results will be duplicates, find the unique ones
thewhich<-!duplicated(resultslist)
thesets<-resultslist[!duplicated(resultslist)]
#which lamda values correspond with the first instances of the unique sets
thevalues<-resultslookup[thewhich,]
#calculate costs associated with selected feature sets
sums1=rep(0,length(thesets))
sums2=rep(0,length(thesets))
for (i in 1:length(thesets)){
  sums1[i]<-sum(newcostscut[which(newcostscut[,1]%in%thesets[[i]]),2])
  sums2[i]<-sum(newcostscut[which(newcostscut[,1]%in%thesets[[i]]),3])
}

#calculate AUCs associated with selected feature sets.
fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 10)

#modellist=list()
for (i in 1:length(thesets)){
  print(i)
  f<-as.simple.formula(thesets[[i]],"CDRSB")
  modellist[[i]]<-train(f,data=trainData,trControl=fitControl,method="rf")
}
testresults=list()
aucs=rep(0,length(thesets))
for (i in 1:length(thesets)){
  
 testresults[[i]]<-predict(modellist[[i]],testData,type="prob")
 aucs[i]<-multiclass.roc(response=as.ordered(testData$CDRSB),predictor=testresults[[i]])$auc
}


lamda1<-resultslookup[thewhich,1]
lamda2<-resultslookup[thewhich,2]

#we now have all the information needed to generate Figure 1.


#this is entirely the same as step 5 but with a different value of lamda

budget1<-1:20
budget1<-budget1*100

budget2<-c(0,250,500,750,1000,2000,2250,2500,2750,3000)
budget2resultslookup<-matrix(nrow=(length(budget1)*length(budget2)),ncol=2)
rowname=0
budget2resultslist=list()
for (i in 1:length(budget1)){
  for (j in 1:length(budget2)){
    rowname=rowname+1
    temp<-cost_cfs_2(CDRSB ~.,lamda=0.004,costs=newcostscut[,2],trainData,budget=budget1[i],costs2=newcostscut[,3],lamda2=0,budget2=budget2[j])
    budget2resultslist[[rowname]]<-temp
    budget2resultslookup[rowname,]<-c(i,j)
    print(c(i,j))
  }
}    
budget2thewhich<-!duplicated(budget2resultslist)
budget2thesets<-budget2resultslist[!duplicated(budget2resultslist)]
budget2thevalues<-budget2resultslookup[budget2thewhich,]
budget2sums1=rep(0,length(budget2thesets))
budget2sums2=rep(0,length(budget2thesets))
for (i in 1:length(budget2thesets)){
  budget2sums1[i]<-sum(newcostscut[which(newcostscut[,1]%in%budget2thesets[[i]]),2])
  budget2sums2[i]<-sum(newcostscut[which(newcostscut[,1]%in%budget2thesets[[i]]),3])
}

thresholds22<-budget2resultslookup[budget2thewhich,2]
thresholds12<-budget2resultslookup[budget2thewhich,1]
b2ls1<-data.frame(thresholds12,budget2sums1)
b2ls2<-data.frame(thresholds22,budget2sums2)

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 3)

budgetmodellist=list()
for (i in 1:length(budgetthesets)){
  print(i)
  f<-as.simple.formula(budgetthesets[[i]],"CDRSB")
  budgetmodellist[[i]]<-train(f,data=trainData,trControl=fitControl,method="rf")
}
budgettestresults=list()
budgetaucs=rep(0,length(budgetthesets))
for (i in 1:length(budgetthesets)){
  
  budgettestresults[[i]]<-predict(budgetmodellist[[i]],testData,type="prob")
  budgetaucs[i]<-multiclass.roc(response=as.ordered(testData$CDRSB),predictor=budgettestresults[[i]])$auc
}

budget2modellist=list()
for (i in 1:length(budget2thesets)){
  print(i)
  f<-as.simple.formula(budget2thesets[[i]],"CDRSB")
  budget2modellist[[i]]<-train(f,data=trainData,trControl=fitControl,method="rf")
}
budget2testresults=list()
budget2aucs=rep(0,length(budget2thesets))
for (i in 1:length(budget2thesets)){
  
  budget2testresults[[i]]<-predict(budget2modellist[[i]],testData,type="prob")
  budget2aucs[i]<-multiclass.roc(response=as.ordered(testData$CDRSB),predictor=budget2testresults[[i]])$auc
}
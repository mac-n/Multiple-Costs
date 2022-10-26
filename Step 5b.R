#this is the same as Steps 5 and 5a with a different value of lambda
budget1<-1:20
budget1<-budget1*100

budget2<-c(0,250,500,750,1000,2000,2250,2500,2750,3000)
budget3resultslookup<-matrix(nrow=(length(budget1)*length(budget2)),ncol=2)
rowname=0
budget3resultslist=list()
for (i in 1:length(budget1)){
  for (j in 1:length(budget2)){
    rowname=rowname+1
    temp<-cost_cfs_2(CDRSB ~.,lamda=0.009,costs=newcostscut[,2],trainData,budget=budget1[i],costs2=newcostscut[,3],lamda2=0,budget2=budget2[j])
    budget3resultslist[[rowname]]<-temp
    budget3resultslookup[rowname,]<-c(i,j)
    print(c(i,j))
  }
}    
budget3thewhich<-!duplicated(budget3resultslist)
budget3thesets<-budget3resultslist[!duplicated(budget3resultslist)]
budget3thevalues<-budget3resultslookup[budget3thewhich,]
budget3sums1=rep(0,length(budget3thesets))
budget3sums2=rep(0,length(budget3thesets))
for (i in 1:length(budget3thesets)){
  budget3sums1[i]<-sum(newcostscut[which(newcostscut[,1]%in%budget3thesets[[i]]),2])
  budget3sums2[i]<-sum(newcostscut[which(newcostscut[,1]%in%budget3thesets[[i]]),3])
}

thresholds22<-budget3resultslookup[budget3thewhich,2]
thresholds12<-budget3resultslookup[budget3thewhich,1]
b2ls1<-data.frame(thresholds12,budget3sums1)
b2ls2<-data.frame(thresholds22,budget3sums2)

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 3)


budget3modellist=list()
for (i in 1:length(budget3thesets)){
  print(i)
  f<-as.simple.formula(budget3thesets[[i]],"CDRSB")
  budget3modellist[[i]]<-train(f,data=trainData,trControl=fitControl,method="rf")
}
budget3testresults=list()
budget3aucs=rep(0,length(budget3thesets))
for (i in 1:length(budget3thesets)){
  
  budget3testresults[[i]]<-predict(budget3modellist[[i]],testData,type="prob")
  budget3aucs[i]<-multiclass.roc(response=as.ordered(testData$CDRSB),predictor=budget3testresults[[i]])$auc
}
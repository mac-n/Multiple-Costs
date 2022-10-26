budget1<-1:20
budget1<-budget1*100

budget2<-c(0,250,500,750,1000,2000,2250,2500,2750,3000)
#set ranges to step through.

budgetresultslookup<-matrix(nrow=(length(budget1)*length(budget2)),ncol=2)
rowname=0
budgetresultslist=list()

for (i in 1:length(budget1)){
  #first loop: B1
  for (j in 1:length(budget2)){
    #second loop: B2
    rowname=rowname+1
    temp<-cost_cfs_2(CDRSB ~.,lamda=0,costs=newcostscut[,2],trainData,budget=budget1[i],costs2=newcostscut[,3],lamda2=0,budget2=budget2[j])
    budgetresultslist[[rowname]]<-temp
    budgetresultslookup[rowname,]<-c(i,j)
    print(c(i,j))
  }
}
#find unique sets
budgetthewhich<-!duplicated(budgetresultslist)
budgetthesets<-budgetresultslist[!duplicated(budgetresultslist)]
budgetthevalues<-budgetresultslookup[budgetthewhich,]
#calculate costs associated with unique sets
budgetsums1=rep(0,length(budgetthesets))
budgetsums2=rep(0,length(budgetthesets))
for (i in 1:length(budgetthesets)){
  budgetsums1[i]<-sum(newcostscut[which(newcostscut[,1]%in%budgetthesets[[i]]),2])
  budgetsums2[i]<-sum(newcostscut[which(newcostscut[,1]%in%budgetthesets[[i]]),3])
}
#calculate AUCs associated with unique sets
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
    
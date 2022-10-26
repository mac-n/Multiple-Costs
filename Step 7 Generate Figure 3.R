
require(viridisLite)
require(ggplot2)



#data from Step 5, 5a, 5b
z1<-data.frame(budgetaucs,budgetsums1,budgetsums2)
z2<-data.frame(budget2aucs,budget2sums1,budget2sums2)
z3<-data.frame(budget3aucs,budget3sums1,budget3sums2)
#organise into long format for ggplot
z1$group<-0
z2$group<-0.004
z3$group<-0.007
colnames(z2)<-colnames(z1)
colnames(z3)<-colnames(z1)
groupz<-rbind(z1,z2,z3)
groupz$budgetsums2<-as.ordered(groupz$budgetsums2)
groupz$group<-as.factor(groupz$group)
#set up colours
colourvector=plasma(12)[10:1]



z4<-z2
#orderbudget<-as.ordered(budget2sums2)
#levels(orderbudget)<-c(0,250,500,750,1000,2000,2250,2500,2750,3000)
#z4$budget2sums2<-orderbudget
#the above does not work! had to add values with NAs to force the legend to come out right
z4<-rbind(z4,c(NA,NA,2750))
z4<-rbind(z4,c(NA,NA,2250))

ggplot(data=groupz, aes(x=budgetsums1, y=budgetaucs,colour=budgetsums2,shape=group))+geom_point(size=3)+theme_bw()+
  labs(y="Model AUC",x="Assessment time (seconds)",colour="Financial Cost (£)",shape=(expression(lambda[1]~ value))) +
  scale_color_manual(values=colourvector)+
  guides(colour=guide_legend(override.aes=list(size=4,shape=15)))











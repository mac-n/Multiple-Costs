

lamda2<-resultslookup[thewhich,2]#get the lambda values which returned unique sets
lamda1<-resultslookup[thewhich,1]
thresholds21<-budgetresultslookup[budgetthewhich,2]#the budget values which returned unique sets
thresholds11<-budgetresultslookup[budgetthewhich,1]
#make data frames for graphs
ls1<-data.frame(lamda1,sums1) 
ls2<-data.frame(lamda2,sums2)

bls1<-data.frame(thresholds11,budgetsums1)
bls2<-data.frame(thresholds21,budgetsums2)
#generate and save graphs
l1<-ggplot(ls1,aes(x=lamda1/1000,y=sums1))+geom_point()+geom_line()+
  xlab(expression(lambda[1])) +ylab("Total Assessment Time (sec)")+theme_bw()+theme(axis.title=element_text(size=10))
ggsave(filename="./lamda11.pdf", plot=l1, width=3.5, height=2.2, units="in")





l2<-ggplot(ls2,aes(x=lamda2/50000,y=sums2))+geom_point()+geom_line()+
  xlab(expression(lambda[2])) +ylab("Total Financial Cost (£)")+theme_bw()+theme(axis.title=element_text(size=10))
ggsave(filename="./lamda12.pdf", plot=l2, width=3.5, height=2.2, units="in")




l3<-ggplot(bls1,aes(x=budget1[thresholds11],y=budgetsums1))+geom_point()+geom_line()+
  xlab(expression(B[1])) +ylab("Total Assessment Time (sec)")+theme_bw()+theme(axis.title=element_text(size=10))
ggsave(filename="./budget11.pdf", plot=l3, width=3.5, height=2.2, units="in")

l4<-ggplot(bls2,aes(x=budget2[thresholds21],y=budgetsums2))+geom_point()+geom_line()+
  xlab(expression(B[2])) +ylab("Total Financial Cost (£)")+theme_bw()+theme(axis.title=element_text(size=10))
ggsave(filename="./budget12.pdf", plot=l4, width=3.5, height=2.2, units="in")
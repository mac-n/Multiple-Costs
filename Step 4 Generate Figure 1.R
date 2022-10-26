require(viridisLite)
colourvector=plasma(12)[10:1]
#the first colours in the spectrum are too light, omit them from the graph.

#ggplot(data=z4, aes(x=budget2sums1, y=budget2aucs,colour=as.ordered(budget2sums2)))+geom_point(size=3)+theme_bw()+

zz<-data.frame(aucs,sums1,sums2)
#the data generated in Step 3
zz<-rbind(zz,c(NA,NA,2750))
zz<-rbind(zz,c(NA,NA,2250))
zz<-rbind(zz,c(NA,NA,2500))
zz<-rbind(zz,c(NA,NA,3000))
#I'm adding these NA values to force the colour spectrum to match up with the one in Fig 3, 
#as the full range of possible values for sums2 isn't covered here.
#It's not pretty but it works. 

ggplot(data=zz, aes(x=log10(sums1), y=aucs,color=as.ordered(sums2)),shape=15,size=4)+geom_point(size=3,shape=15)+theme_bw()+
  scale_color_manual(values=colourvector)+guides(colour=guide_legend(override.aes=list(size=4,shape=15)))+
  labs(y="Model AUC",x="Assessment time (seconds)",colour="Financial Cost (£)")




#ggplot(data=z, aes(x=sums1, y=aucs,color=as.ordered(sums2)))+geom_point()+theme_bw()

#this code requires the ADNIMERGE R package (version 0.0.1)
#and the following code, used under Academic Free License ("AFL") v. 3.0 to construct the dataset
#https://github.com/mac-n/Rshiny-app/blob/master/step%201%20recode%20age%20and%20CDR-SB.R
#https://github.com/mac-n/Rshiny-app/blob/master/step%202%20select%20from%20tables.R

#after installing the ADNIMERGE package and running the above, run this

adnicols<- adnimerge %>% dplyr::select(RID,VISCODE,TAU.bl,ABETA.bl,FDG.bl,Entorhinal.bl)
joinednew<-inner_join(all_table,adnicols)
adnicols$VISCODE<-as.character(adnicols$VISCODE)
adnicols$RID<-as.numeric(adnicols$RID)
joinednew<-inner_join(all_table,adnicols)
joinednew<-na.omit(joinednew)

#The RData item newcostscut is required to run this and has been included with the codes. It is a list of feature names and associated costs
joinednewcut<-joinednew[,which(colnames(joinednew) %in% newcostscut[,1])]




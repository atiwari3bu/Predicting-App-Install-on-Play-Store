#Created the dataframes by using first row from final
paid<-final[1,]
free<-final[1,]
names(paid)<-names(final)
names(free)<-names(final)

#Looping through every row of final dataframe
for(k in 9000:length(final$App)){
  if(final[k,]["Type"]=="Paid"){
    paid<-rbind(paid,final[k,])
  }
  else{
    free<-rbind(free,final[k,])
  }
}

#Removing first rows of paid and free dataframes
free<-free[-1,]
paid<-paid[-1,]
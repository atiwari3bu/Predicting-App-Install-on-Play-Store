#install.packages("tidyverse")
library(tidyverse)
user_review<-read_csv("user_reviews.csv")
user_review<-na.omit(user_review)  # Removing nan comments
googleplaystore<-read_csv("googleplaystore.csv")
googleplaystore<-unique(googleplaystore) # Removing copies of row
# Removing rows with same app name
googleplaystore<-googleplaystore[!duplicated(googleplaystore[c('App')]),]
googleplaystore$X14<-NULL # Removing the column X14 from df
googleplaystore %>% group_by(Category)%>% summarise(count=n())%>% top_n(10) %>%
ggplot(aes(reorder(Category,-count),count))+geom_bar(aes(fill=Category),stat="identity")+
theme(axis.text.x = element_text(angle = 15,hjust = 0.5, size =7))+
theme(legend.position="none")+xlab("")+
scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
# Also we need to plot rating vs category
aggregate_rating<-aggregate(user_review$Sentiment_Polarity, by = list(App=user_review$App), FUN=sum)
colnames(aggregate_rating)[2] <- "Sum"
freq<-user_review %>% count(App)
aggregate<-merge(aggregate_rating,freq,by="App")
aggregate$average<-aggregate$Sum/aggregate$n
aggregate$n<-NULL
aggregate$Sum<-NULL
rm(list=c('freq','aggregate_rating'))
final<-merge(googleplaystore,aggregate,by="App",all.x=TRUE)
rm(aggregate)
colnames(final)[which(names(final) == "average")] <- "Average Rev"
final<- final[!(final$Category=="1.9"),] # More of data cleaning

# splitting the dataset into evenly training and testing
# Creating the dataset with frequency of each variable based on category
final<- final[!(final$Category=="1.9"),]
freq<- data.frame(table(final$Category))
colnames(freq)[which(names(freq) == "Var1")] <- "Category"
freq$train<-floor(freq$Freq * 0.8)
freq$test<-round(freq$Freq * 0.2)

#Shuffling the dataframe row wise
final <- final[sample(nrow(final)),]
rownames(final)<-NULL

#Creating training, testing datasets
# Creating training and testing dataframes with first entry
training<-data.frame(final[1,])
names(training)<-names(final)
identical(names(training), names(final))

testing<-data.frame(final[1,])
names(testing)<-names(final)
identical(names(testing), names(final))

training_completed=0
testing_completed=0

#looping through every category
for(i in 32:length(freq$Category)){
  cat=freq[i,]["Category"]
  print(cat)
  print("Splitting it into training and testing")
  j=0
  #Looping thorugh every row in dataframe
  for(k in 1:length(final$App)){
    # Entering the type of dataframe for testing
    if(final[k,]["Category"]==cat && training_completed==0){
      training<-rbind(training,final[k,])
      j=j+1
      if(j==freq[i,]["train"]){
        j=0
        training_completed=1
      }
      next
    }
    
    if(final[k,]["Category"]==cat && testing_completed==0){
      testing<-rbind(testing,final[k,])
      j=j+1
      if(j==freq[i,]["test"]){
        j=0
        testing_completed=1
      }
      next
    }
  }
  training_completed=0
  testing_completed=0
}
#Removing the first entry of training,testing and validation
testing<-testing[-1,]
training<-training[-1,]
# Create another columns 80 and 20.
# Create 2 columns where paid testing and paid training
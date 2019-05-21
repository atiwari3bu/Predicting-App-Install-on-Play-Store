# Creating training, testing and validation with first entry
training<-data.frame(final[1,])
names(training)<-names(final)
identical(names(training), names(final))

testing<-data.frame(final[1,])
names(testing)<-names(final)
identical(names(testing), names(final))

validating<-data.frame(final[1,])
names(validating)<-names(final)
identical(names(validating), names(final))

training_completed=0
testing_completed=0
validation_completed=0

for(i in 33:length(freq$Category)){
  cat=freq[i,]["Category"]
  print(cat)
  j=0
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
    
    if(final[k,]["Category"]==cat && validation_completed==0){
      validating<-rbind(validating,final[k,])
      j=j+1
      if(j==freq[i,]["val"]){
        j=0
        validation_completed=1
      }
      next
    }
  }
#  testing<-testing[-1,]
}
# Create another columns 80 and 20.
# Create 2 columns where paid testing and paid training
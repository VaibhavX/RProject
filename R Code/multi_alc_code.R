#Change the path based on file location
alc2 <- read.csv("/Users/vaibhavx/Desktop/R_Data/EMA_mods/Alcohol/test_lml1001/lml1001.csv", header=T, stringsAsFactors = FALSE)
View(alc2)

#Converting the time column to show only day of the week
for ( i in 1:nrow(alc2))
{
  alc2$PromptTime[i] <- substr(alc2$PromptTime[i], 1, 3)
}
View(alc2)


#Creating a new data frame to save summed value of alcohol
alc1 <- data.frame(matrix(ncol = 4, nrow = 7))
x <- c("Subject_ID", "Prompt_Date", "Prompt_Day", "Q12_alcohol_sum")
colnames(alc1) <- x
View(alc1) #View the column details

#Add the Subject_ID for the first entry
alc1$Subject_ID[1] <- alc2$Subject_ID[1]
b <- 1

#Adding rest of the data
a <-1 
alc1$Prompt_Date[a] <- alc2$PromptDate[1]
alc1$Prompt_Day[a] <- alc2$PromptTime[1]
sum <- 0
for ( i in 1:nrow(alc2))
{
  if(alc2$Subject_ID[i]==alc1$Subject_ID[b]) #for each participant
  {
    
    
    
    if(alc2$PromptDate[i]==alc1$Prompt_Date[a])
    {
      if(alc2$Q12_alcohol[i] != 999)
        sum <- sum + alc2$Q12_alcohol[i]
    }
    else
    {
      alc1$Q12_alcohol_sum[a] <- sum
      a = a + 1
      b <- b+1
      alc1$Subject_ID[b] <- alc2$Subject_ID[i]
      alc1$Prompt_Date[a] <- alc2$PromptDate[i]
      alc1$Prompt_Day[a] <- alc2$PromptTime[i]
      sum <- alc2$Q12_alcohol[i]
      if(sum == 999)
      {
        sum<-0
      }
      
    }
  }
  else
  {
    alc1$Q12_alcohol_sum[a] <- sum
    b=b+1
    alc1$Subject_ID[b] <- alc2$Subject_ID[i]
    a=a+1
    sum <- alc2$Q12_alcohol[i]
    if(sum==999)
     { sum <- 0 }
    alc1$Prompt_Date[b] <- alc2$PromptDate[i]
    alc1$Prompt_Day[b] <- alc2$PromptTime[i]
  }
  
}
alc1$Q12_alcohol_sum[a] <- sum #Adding the last entry

#Writing the data frame into csv file; Change the path of the file
write.csv(alc1,"/Users/vaibhavx/Desktop/R_Data/EMA_mods/Alcohol/test_lml1001/lml1001_final.csv", row.names = FALSE)

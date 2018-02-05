df_marwho <- read.csv("/Users/vaibhavx/Desktop/R_Data/DailyLog_Social/dailylog_marwho.csv", header=T, stringsAsFactors = FALSE)
View(df_marwho)
for(i in 1:nrow(df_marwho))
{
  x <- strsplit(df_marwho$Q4_3_b_marijuana_who[i], ",|", fixed=TRUE) #Split the string of Social
  a <- length(x[[1]]) #Length of splits
  n1<-0
  n2<-0
  n3<-0
  n4<-0
  for(j in 1:a) #Running loop till the length
  {
    
    if(x[[1]][j]=="0")
    {
      df_marwho$Q4_3_b_marijuana_who_0[i]<-1
    }
    else if(x[[1]][j] =="999" || x[[1]][j]=="900")
    {
      next
    }
    else if(x[[1]][j] != "999" || x[[1]][j]!= "900")
    {
      
      if(n1==0)
      {
        df_marwho$Q4_3_b_marijuana_who_1[i]<- x[[1]][j]
        n1 <- 1
      }
      else if(n2==0)
      {
        df_marwho$Q4_3_b_marijuana_who_2[i]<- x[[1]][j]
        n2 <- 1
      }
      else if(n3==0)
      {
        df_marwho$Q4_3_b_marijuana_who_3[i]<-x[[1]][j]
        n3 <-1
      }
      else if(n4==0)
      {
        df_marwho$Q4_3_b_marijuana_who_4[i]<- x[[1]][j]
        n4 <- 1
      }
      else
      {
        df_marwho$Q4_3_b_marijuana_who_5[i]<-x[[1]][j]
      }
      
    }
    else
    {
      next
    }
  }
}
write.csv(df_marwho,"/Users/vaibhavx/Desktop/R_Data/DailyLog_Social/dailylog_marwho_final.csv", row.names = FALSE)
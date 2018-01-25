df_social <- read.csv("/Users/vaibhavx/Desktop/R_Data/DailyLog_Social/dailylog_social.csv", header=T, stringsAsFactors = FALSE)
View(df_social)
for(i in 320:nrow(df_social))
{
  x <- strsplit(df_social$Q4_0_soccore[i], ",|", fixed=TRUE) #Split the string of Social
  a <- length(x[[1]]) #Length of splits
  n1<-0
  n2<-0
  n3<-0
  n4<-0
  for(j in 1:a) #Running loop till the length
  {
    
    if(x[[1]][j]=="0")
    {
      df_social$Q4_0_soccore_0[i]<-1
    }
    else if(x[[1]][j] =="999" || x[[1]][j]=="900")
    {
      next
    }
    else if(x[[1]][j] != "999" || x[[1]][j]!= "900")
    {
      
        if(n1==0)
        {
          df_social$Q4_0_soccore_1[i]<- x[[1]][j]
          n1 <- 1
        }
        else if(n2==0)
        {
          df_social$Q4_0_soccore_2[i]<- x[[1]][j]
          n2 <- 1
        }
        else if(n3==0)
        {
          df_social$Q4_0_soccore_3[i]<-x[[1]][j]
          n3 <-1
        }
        else if(n4==0)
        {
          df_social$Q4_0_soccore_4[i]<- x[[1]][j]
        }
        else
        {
          df_social$Q4_0_soccore_5[i]<-x[[1]][j]
        }
      
    }
    else
    {
      next
    }
  }
}

write.csv(df_social,"/Users/vaibhavx/Desktop/R_Data/DailyLog_Social/dailylog_social_final.csv", row.names = FALSE)
df_subchk <- read.csv("/Users/vaibhavx/Desktop/R_Data/DailyLog_Social/dailylog_subchk1.csv", header=T, stringsAsFactors = FALSE)
View(df_subchk)
for( i in 1:nrow(df_subchk))
{
  x <- strsplit(df_subchk$Q4a_substances[i], ",", fixed=TRUE) #Split the string of Social
  a <- length(x[[1]]) #Length of splits
  for(j in 1:a) #Running loop till the length
  {
    
    if(x[[1]][j]=="0")
    {
      df_subchk$Q4a_substances_0[i]<-1
    }
    else if(x[[1]][j] =="999" || x[[1]][j]=="900")
    {
      next
    }
    else if(x[[1]][j] != "999" || x[[1]][j]!= "900")
    {
      
      if(x[[1]][j]=="1")
      {
        df_subchk$Q4a_substances_1[i]<- 1
      }
      else if(x[[1]][j]=="2")
      {
        df_subchk$Q4a_substances_2[i]<- 1
      }
      else if(x[[1]][j]=="3")
      {
        df_subchk$Q4a_substances_3[i]<- 1
      }
      else if(x[[1]][j]=="4")
      {
        df_subchk$Q4a_substances_4[i]<- 1
      }
      else if(x[[1]][j]=="5")
      {
        df_subchk$Q4a_substances_5[i]<- 1
      }
      else
      {
        df_subchk$Q4a_substances_6[i]<- 1
      }
      
    }
    else
    {
      next
    }
  }
}

write.csv(df_subchk,"/Users/vaibhavx/Desktop/R_Data/DailyLog_Social/dailylog_subchk1_final.csv", row.names = FALSE)
#New looop for assembling files in a single data frame
for (i in 1:length(pid))
{
  build_path <- paste("S:/LML_Share/USC LOG MY LIFE STUDY/Data/Quant/Raw Data-Cleaning/Raw EMA Data/Wockets Downloads/Decrypted Wockets Downloads/lml",pid[i],"@lml_com", sep="")
  build_filenames <- list.files(build_path, pattern= 'PromptResponses_Dailylog.csv$', recursive = T)
  
  filepath <- paste(build_path,build_filenames[1], sep="/")
  if(ctr == 0)
  {
    data <- read.csv(filepath, header = T)
    ctr <- 1
  }
  else
  {
    data1 <- read.csv(filepath, header=T)
    data <- rbind(data, data1)
  }
  
  
  if(length(build_filenames) > 1)
  {
    for (j in 2:length(build_filenames))
    {
      filepath <- paste(build_path,build_filenames[j], sep="/")
      data1 <- read.csv(filepath, header=T)
      data <- rbind(data, data1)
    }
  }
  
}

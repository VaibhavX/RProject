filenames <- list.files("S:/LML_Share/USC LOG MY LIFE STUDY/Data/Quant/Raw Data-Cleaning/Raw EMA Data/Wockets Downloads/Decrypted Wockets Downloads", pattern= 'Prompts.csv$', recursive = T)
filepath <- paste("S:/LML_Share/USC LOG MY LIFE STUDY/Data/Quant/Raw Data-Cleaning/Raw EMA Data/Wockets Downloads/Decrypted Wockets Downloads/lml1001@lml_com", filenames[1], sep="/")


for( i in 2:length(filenames)) {
  filepath <- paste("S:/LML_Share/USC LOG MY LIFE STUDY/Data/Quant/Raw Data-Cleaning/Raw EMA Data/Wockets Downloads/Decrypted Wockets Downloads/lml1001@lml_com", filenames[i], sep="/");
  data1 <- read.csv(filepath, header=T);
  data <- rbind(data, data1); 
}

#Storing Participant ID in an array which will help build the path on its own (Future automation to take input from user)
pid <- c("1047","1049","1053")

first_path <- paste("S:/LML_Share/USC LOG MY LIFE STUDY/Data/Quant/Raw Data-Cleaning/Raw EMA Data/Wockets Downloads/Decrypted Wockets Downloads/lml",pid[1],"@lml_com", sep="")

files_names <- list.files(first_path, pattern= 'Prompts.csv$', recursive = T)


ctr <- 0

#New looop for assembling files in a single data frame
for (i in 1:length(pid))
{
  build_path <- paste("S:/LML_Share/USC LOG MY LIFE STUDY/Data/Quant/Raw Data-Cleaning/Raw EMA Data/Wockets Downloads/Decrypted Wockets Downloads/lml",pid[i],"@lml_com", sep="")
  build_filenames <- list.files(build_path, pattern= 'PromptResponses_EMA.csv$', recursive = T)
  
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
      data1 <- read.csv(filepath, header=T, stringsAsFactors = )
      data <- rbind(data, data1)
    }
  }
  
}




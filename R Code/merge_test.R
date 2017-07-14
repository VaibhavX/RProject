#Collecting all files with PromptResponses
filenames <- list.files("/Users/vaibhavx/Desktop/R_Data/Input_Files", pattern = 'PromptResponses_EMA.csv$', recursive = T)

#Building path for reading data from the 1st csv
filepath <- paste("/Users/vaibhavx/Desktop/R_Data/Input_Files", filenames[1], sep = "/")

#Reading the csv files 
data <- read.csv(filepath, header =T)

#looping through different files and then binding them row wise
for( i in 2:length(filenames)) {
  
  filepath <- paste("/Users/vaibhavx/Desktop/R_Data/Input_Files", filenamesp[i], sep="/")
  data2 <- read.csv(filepath, header = T)
  data <- rbind(data, data1)
  
}

#Writing the files in a single CSV File
write.csv(data,"/Users/vaibhavx/Desktop/R_Data/merge_data.csv", row.names = FALSE)

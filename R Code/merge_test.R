#Reading the csv files 

data1 <- read.csv("/Users/vaibhavx/Desktop/R_Data/Input_Files/PromptResponses_EMA.csv", header = T)
data2 <- read.csv("/Users/vaibhavx/Desktop/R_Data/Input_Files/PromptResponses_EMA_copy.csv", header = T)
data3 <- read.csv("/Users/vaibhavx/Desktop/R_Data/Input_File/PromptResponses_EMA copt 2.csv", header = T)

#Binding all the files row-wise
final2 <- rbind(data1, data2, data3)

#Writing the files in a single CSV File
write.csv("/Users/vaibhavx/Desktop/R_Data/merge_test_2", row.names = FALSE)

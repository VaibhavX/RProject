            # This script builds a sceleton table that will be populated with the data#
          ##############################################################################


      ###The next two functions are for manipulating the dates###
          
# add day after the enter date
#This function takes a vector of dates and adds a number (N) to a day
#So this will format whatever string of dates we have in x in the format that we want plus adds one more day
ema.dayAfter=function(x, N) {
  day=as.Date(x, format = "%Y-%m-%d", tz = "America/Los_Angeles")
  return(as.character(day+N))
}

ema.dayAfter('16-04-12',1)

data <- read.csv('C:/Users/afilus/Dropbox/USC COMP/HOMELESS YOUTH GRANT/JINGS R FILES/Datax.csv', header=TRUE)
attach(data)

# convert manual input date 
ema.dateConv=function(x) {
  if (is.na(x)) {
    return(NA)
  } else {
    if (grepl("/", x)) {
      seg=strsplit(x, "/")[[1]]
      year=ifelse(nchar(seg[3])<4, paste0("20", seg[3]), seg[3])
      mon=ifelse(nchar(seg[1])<2, paste0("0", seg[1]), seg[1])
      day=ifelse(nchar(seg[2])<2, paste0("0", seg[2]), seg[2])
    } else {
      seg=strsplit(x, "-")[[1]]
      year=ifelse(nchar(seg[1])<4, paste0("20", seg[1]), seg[1])
      mon=ifelse(nchar(seg[2])<2, paste0("0", seg[2]), seg[2])
      day=ifelse(nchar(seg[3])<2, paste0("0", seg[3]), seg[3])
    }
    return(paste(year, mon, day, sep="-"))
  }
}

ema.dateConv('02/12/24')


                        ###This function creates a table of promots for an individual###


# indiv prompt list
ema.indivPromptList = function (idX, enterDateX) {
  win=rbind(
    names=c("7-8am", "9-10am", "11am-12pm", "1-2pm", "3-4pm", "5-6pm", "7-8pm", "9-9:30pm"),
    winSeq=c(1:8),
    start=c("07:00:00", "09:00:00", "11:00:00", "13:00:00", "15:00:00", "17:00:00","19:00:00", "21:00:00"),
    end=c("08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "21:30:00")
  )
 
  promptList=c()
  for (i in 0:7) {
    currentDate=ema.dayAfter(enterDateX, i)
    weekday=weekdays(strptime(currentDate, format = "%Y-%m-%d", tz = "America/Los_Angeles"), abbreviate = TRUE)
    weekend=ifelse(weekday %in% c("Sat", "Sun"), 1, 0)
      promptListX=data.frame(
      subjectID=idX,
      date=currentDate,
      dayInStudy=i+1,
      weekday=weekday,
      weekend=weekend,
      comply=NA_integer_,
      complete=NA_integer_,
      promptStart=NA,
      promptEnd=NA,
      reprompt=NA_integer_,
      bedtime=NA_character_,
      tmrwaketime=NA_character_,
      file=NA_character_,
      stringsAsFactors = FALSE)
    promptList=rbind(promptList, promptListX)
  }
    return(promptList)
}


ema.indivPromptList(DID, W1pickup)
head(promptList)
str(promptList)
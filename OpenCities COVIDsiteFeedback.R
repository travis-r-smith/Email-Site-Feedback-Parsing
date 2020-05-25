#CoSA COVID Website Feedback
#READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE
#Install Tidyverse to ease removing duplicates and other features
install.packages("tidyverse")
#setwd("~/R/InnovatoR")
setwd("C:/Users/travi/OneDrive/Documents/Data Science/My R Workspace/R Projects/CoSA COVID")
###READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE###

#Add Library that enables removing duplicates
library(tidyverse)
library(stringr)
library(dplyr)
#Set the working Directory to RVeraAuto folder on G:Drive
getwd()

#Store current datetime as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")
#print(now)
#
## Tell R to read the main email export file as well as the historical file pre-May13
# from the directory.
original_main_df <- read.csv("opencitiesCOVIDmail.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE )
# Can do code for historical file as time allows

#### Re-Run Code from HERE ####
main_df <- original_main_df
dim(main_df)
#Remove duplicates rows
main_df <- unique(main_df[,])

#main_df <- main_df[!duplicated(main_df$Body), ]
dim(main_df)

# THEN REMOVE ADDITIONAL columns from the file i.e. the subject, from, to, and cc columns come with the file
# but do not contain valuable information.


head(main_df$ï..Date)
names(main_df)
main_df <- subset(main_df, select = -c(Subject, From., To., CC.))
names(main_df)

head(main_df$ï..Date)
class(main_df$ï..Date)






#Convert the datetime field from character to a datetime
main_df$datetime <- strptime(main_df$ï..Date, format = "%m/%d/%Y %H:%M")
main_df$datetime <- as.POSIXct(main_df$datetime)
head(main_df$datetime)
class(main_df$datetime)

#Remove the poorly computer-titled character field that contained datetime info
main_df <- subset(main_df, select = -c(ï..Date))

#Use the NEW datetime field to create a date field
main_df$Date <- as.Date(main_df$datetime, format = "%m/%d/%Y" )
class(main_df$Date)

head(main_df$Date)

#Use the NEW datetime field to create a military time field
main_df$military_time <- main_df$datetime
main_df$military_time <- format(main_df$military_time, "%H:%M")

#Use the NEW datetime field to create an AM PM time field
main_df$time <- main_df$datetime
main_df$time <- format(main_df$time, "%I:%M %p")

class(main_df$time)
head(main_df$time)

#Use datetime to create a field that just says if the time was AM or PM (for possible filtering)
main_df$ampm <- main_df$datetime
main_df$ampm <- format(main_df$ampm, "%p")


names(main_df)



#### Remove Repetitive Text ####
main_df$Body <- gsub("[*]", "", main_df$Body)
main_df$Body <- gsub("&amp;", "", main_df$Body)
main_df$Body <- gsub("\n", "", main_df$Body)
main_df$Body <- gsub("[\"]", "", main_df$Body)
main_df$Body <- gsub("[']", "", main_df$Body)
main_df$Body <- gsub("â???T", "", main_df$Body)
main_df$Body <- gsub("&quot;", "", main_df$Body)
main_df$Body <- gsub("\"", "", main_df$Body)
main_df$Body <- gsub("Hi,You have received feedback for ", "", main_df$Body )
main_df$Body <- gsub("Did we help you today?", "", main_df$Body )
main_df$Body <- gsub("Regards.*","", main_df$Body )
head(main_df$Body)

#â???T"
#"â???T"


#Pull everything before "Was this page helpful"  from text string to create a feedback page column
main_df$page <- sub("[?] Was this page helpful.*", "\\1" ,main_df$Body)


main_df$page <- gsub(" <https://covid19.sanantonio.gov/About-COVID-19/Case-Numbers-Table-Data> ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/Assistance/Travelers/Healthy-Traveler-Tips>  ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/About-COVID-19/Dashboards-Data/Online-Self-Screening-Outcomes> ", "", main_df$page)
 
main_df$page <- gsub(" <https://covid19.sanantonio.gov/About-COVID-19/Dashboards-Data/Progress-Warning-Indicators> ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/About-COVID-19/Dashboards-Data/Surveillance> ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/About-COVID-19/Dashboards-Data> ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/Services/Testing-for-COVID-19> ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/Assistance/Travelers/Healthy-Traveler-Tips> ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/Assistance/Travelers> ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/Assistance/Residents/Unemployment-Job-Training> ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/What-YOU-Can-Do/Symptoms>  ", "", main_df$page)

main_df$page <- gsub(" <https://covid19.sanantonio.gov/Assistance/Seniors>  ", "", main_df$page)


summary(as.factor(main_df$page))


#Did we help you, Was this page helpful column
main_df$helpful <- as.factor(sub(".*Was this page helpful[?]: *(.*?) *Suggested Improvements.*", "\\1", main_df$Body))
head(main_df$helpful)

#Suggested improvements column
main_df$suggested_improvements <- sub(".*Suggested Improvements: *(.*?) *Email Address.*", "\\1", main_df$Body)
head(main_df$suggested_improvements)





#Pull everything after "email address: "  from text string to create a column for email addresses
main_df$email <- sub(".*Email Address: ", "\\1" ,main_df$Body)
main_df$email <- tolower(main_df$email)
head(main_df$email)

# If email address is extremely long make it null.

class(main_df$email)
summary(main_df$email)

main_df <- main_df %>%
  
  mutate(email = as.character(email)) %>%
  
  mutate(
    email = ifelse(
      nchar(email) > 38, 
      NA, 
      email)
  )

dim(main_df)
#main_df <- main_df[!duplicated(main_df$Body), ]
main_df <- main_df[!duplicated(main_df[c('suggested_improvements','email', 'page')]),]
dim(main_df)
#Index it you now have dates but you never know.
main_df$index <- nrow(main_df):1
head(main_df$index)

### Try to get a mini dataset of just the people who responded more than once but not working ###
#Special subset of people who have responded more than once. 
#**This isn't working as expected**
main_df$mult_resp <- as.logical(duplicated(main_df$email)|duplicated(main_df$email, fromLast = TRUE))
#dim(mult_responseSub)
summary(main_df$mult_resp)
summary(as.factor(main_df$email))



i1 <- grepl('find', main_df$suggested_improvements) | grepl('looking for', main_df$suggested_improvements) | grepl('searching', main_df$suggested_improvements)

main_df$finding <- NA
main_df$finding[i1] <- 'Having trouble locating information'
main_df$finding <- as.factor(main_df$finding)
summary(as.factor(main_df$finding))

main_df$response_len <- nchar(main_df$suggested_improvements)
head(main_df)

i2 <-
  grepl('help', main_df$suggested_improvements) |
  grepl('assist', main_df$suggested_improvements) |
  grepl('emerge', main_df$suggested_improvements) |
  grepl('to know', main_df$suggested_improvements) |
  grepl('guidance', main_df$suggested_improvements) |
  grepl('how to', main_df$suggested_improvements)
main_df$help_asst <- NA
main_df$help_asst[i2] <- 'Needs Help or Assistance'
main_df$help_asst <- as.factor(main_df$help_asst)
summary(main_df$help_asst)

#main_df[main_df$email == 'dmdelire1732@gmail.com',]
#which(main_df$email == 'dmdelire1732@gmail.com',)

i3 <- grepl('dash', main_df$suggested_improvements) | grepl('chart', main_df$suggested_improvements) | grepl('graph', main_df$suggested_improvements) | grepl(' line', main_df$suggested_improvements) | grepl('visual', main_df$suggested_improvements)
main_df$dashboard <- NA
main_df$dashboard[i3] <- 'Feedback on Visualizations'
main_df$dashboard <- as.factor(main_df$dashboard)
summary(main_df$dashboard)



summary(main_df$helpful)

NotHelpfulSub <- main_df[main_df$helpful == "No",]
YesHelpfulSub <- main_df[main_df$helpful == "Yes",]
MaybeHelpfulSub <- main_df[main_df$helpful == "Maybe",]

names(MaybeHelpfulSub)
#View(MaybeHelpfulSub[c('email', 'suggested_improvements', 'index'),])
#View(MaybeHelpfulSub)
#MaybeHelpfulSub[1 ,c(4, 6, 5)]

#View(NotHelpfulSub)
#View(YesHelpfulSub)

#### Reorder the fields ####

main_df <- main_df[ , c( "Date", "page",  "helpful", "suggested_improvements", "response_len", "email", "mult_resp", 
                         "help_asst", "finding", "dashboard", "datetime", "military_time", "time", "ampm",  "Body", "index")]

main_df <- main_df[order(main_df$datetime, decreasing = TRUE),]

names(main_df)
#FINISHING THE DATA AND SPITTING OUT A CSV WITH A datetimeED TITLE
#View(main_df2)
#Store current datetime as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")
file_name <- paste0("Open_Cities_Feedback_Munged", ".csv" )
#file_name <- paste0("Open_Cities_Feedback_Munged", now, ".csv" )


#### Export Data as a csv file ####
write.table(main_df, sep=",", file_name, row.names = FALSE, )
dim(main_df)
--------------------------------------------------------------------------

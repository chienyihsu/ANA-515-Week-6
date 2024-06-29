getwd()

#Importing the data from CSV
library(tidyverse)
data <- read.csv("StormEvents_details-ftp_v1.0_d1994_c20220425.csv")
head(data)

#Selecting the variables to use
vars <- c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE","STATE_FIPS","CZ_NAME","CZ_TYPE","CZ_FIPS","EVENT_TYPE")
newdata <- data[vars]
head(newdata)

#Arranging the data
library(dplyr)
arrangeddata <- arrange(newdata, STATE)
head(arrangeddata)

#Changing state and county names
library(stringr)
str_to_title(string = arrangeddata$STATE)
str_to_title(string = arrangeddata$CZ_NAME)

#Limiting to the events and removing a column
limitdata <- filter(arrangeddata, CZ_TYPE=='C')
limitdata <- select(arrangeddata, - c(CZ_TYPE))
head(limitdata)

#Padding and uniting states and counties
newerdata <- str_pad(newdata$STATE_FIPS, width=3, side= "left", pad= "0")
view(newerdata)

str_pad(newdata$CZ_FIPS, width=3, side= "left", pad= "0")
view(newerdata)

library(tidyr)
class(newerdata)
newerdata <- data.frame(STATE_FIPS = c("01", "02"), CZ_FIPS = c("001", "003"))

unite(newerdata, flips, STATE_FIPS, CZ_FIPS, sep ="00")


#Changing all the column names to lower case
rename_all(newerdata, tolower)

#Creating new dataframe
data("state")
stateinfo <- data.frame(state=state.name, region=state.region, area=state.area)
newset <- data.frame(table(newdata$STATE))
head(newset)

#Merge State
newerset<-rename(newset, c("state"="Var1"))
mergeddata <- merge(x=newerset, y=stateinfo, by.x = "state", by.y = "state")
head(mergeddata)

newstateinfo  <- mutate_all(stateinfo, toupper)
mergeddata <- merge(x=newerset, y=newstateinfo, by.x = "state", by.y = "state")
head(mergeddata)

#Creating scatter plot
library(ggplot2)
eventplot <- ggplot(mergeddata, aes(x = area, y = Freq)) +
  geom_point(aes(color = region)) +
  labs(x = "Land area(square miles)" , y = "# of storm events in 2017") +
  theme(axis.text.x = element_text(angle = 90, size = 8))
eventplot

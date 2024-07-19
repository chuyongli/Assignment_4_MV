#' Mausam Vadakkayil
#' BTC1855: Assignment 4

#Read the data into a dataframe
ufo <- as.data.frame(read.csv("/Users/mausamvk/Desktop/BTC1855/ufo_subset.csv"))

#Check and correct structural issues
str(ufo) #here we can see some issues, "datetime" includes two data the time and date, "datetime" is a character, date_posted" is character

####1. split "datetime" info into 2 seperate coloumns, date of sighting and time of sighting

ufo$date_sighting <- substr(ufo$datetime, start = 1, stop = 10) # first split date info into a new date_sighting coloumn, by using substring to split the first 10 characters of the date
names(ufo) #here we can now see a new coloumn called date_sighting

ufo$time_sighting <- substr(ufo$datetime, start = 11, stop = 16) #split time info into a new time_sighting coloumn, by using substring to split the 11th character to the 16th character (which are all time)
names(ufo) #here we can now see a new coloumn called time_sighting

ufo <- subset(ufo, select=c(13,12,2,3,4,5,6,7,8,9,10,11)) #remove the datetime coloumn as its info has been transferred to last 2 coloumns, and reorder the last two coloumns to first and second position
str(ufo) #here we see first two coloumns are now time_sighting and date_sighting, and orginal datetime coloumn has been removed

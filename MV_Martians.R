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


####2. Convert date_sighting to numeric for downstream analysis
# upon looking through a few values of the dates it seems the setup is YY - MM- DD, so we can use ymd()


#Your downstream analysis will require the variables 'country', 'shape' and 'duration seconds'. Analyze these columns, identify any problems (e.g. missingness, outliers, inconsistency etc) and implement your solution (e.g. remove, impute etc).

####3. Country analysis 
ufo$country
ufo$country <- as.factor(ufo$country) #convert countries to level
levels(ufo$country) #here we can see 6 levels, one of which is mssing values
ufo <- droplevels(ufo[!ufo$country == "",]) #drop level in country coloumn with "" (denotes missing values)
levels(ufo$country) #now we see it has 5 levels: au, ca, de, gb and us
plot(ufo$country) #genertes plot of country counts, and here we see no missing values 

####4. shape analysis 
ufo$shape #we again can see here "" values and "unknown" values
ufo$shape <- as.factor(ufo$shape) #convert to factor so we can see the groups
levels(ufo$shape) # here we see "" and "unkown", 22 levels
plot(ufo$shape) #to view the groupings
ufo <- droplevels(ufo[!ufo$shape == "",])
ufo <- droplevels(ufo[!ufo$shape == "unknown",])
levels(ufo$shape) #20 levels now
plot(ufo$shape) #plot again and see the missing/unkwown are gone 

####5. durations seconds analysis 
ufo$duration.seconds
summary(ufo$duration.seconds)
str(ufo$duration.seconds)
boxplot(ufo$duration.seconds)
#find IQR for duration.seconds and remove outliers 
Q1 <- quantile(ufo$duration.seconds, .25)

Q3 <- quantile(ufo$duration.seconds, .75)

IQR <- IQR(ufo$duration.seconds)

ufo <- subset(ufo, ufo$duration.seconds > (Q1 - 1.5*IQR) & ufo$duration.seconds < (Q3 + 1.5*IQR))

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

#####6. NUFORC officials comment on sightings that may be hoax. Identify the best way to identify and remove these sightings from the dataset.
#identify and remove rows with "hoax" or "HOAX"
str_view(ufo$comments, pattern = "HOAX") #can see HOAX
str_view(ufo$comments, pattern = "HOAX") #can see hoax
ufo <- ufo[- grep("HOAX", ufo$comments, ignore.case = T),] #find and remove from ufo instances of HOAX from the ufo$comments coloumn, ignore.case will check for hoax we as well, 
str_view(ufo$comments, pattern = "HOAX") #nothing shows up and HOAX/hoax have been removed

####7. Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported.
ufo$date_posted #format is DD-MM-YY
ufo$date_sighting #format is YY-MM-DD

library(lubridate)
ufo$date_posted <- dmy(ufo$date_posted) #convert dates from D/M/Y to Y/M/D

ufo$date_posted <- as.Date(ufo$date_posted) #ensure these are stored as dates
ufo$date_sighting <- as.Date(ufo$date_sighting)

ufo$report_delay <- ufo$date_posted - ufo$date_sighting #subtract date_posted and date_sighting, save results in report_delay
ufo$report_delay #can now see reporting delay in days

#Remove the rows where the sighting was reported before it happened. i.e. any negative values 
grep("-[1:9]", ufo$report_delay) #here we can see index 5990 and 17244 has negative days
ufo <- ufo[- grep("-[1:9]", ufo$report_delay),] #remove these
grep("-[1:9]", ufo$report_delay) #now this will return nothing as they have been removed

#### 8. Create a table with the average report_delay per country.

library(dplyr)

country_delay <- ufo %>% 
  group_by(country) %>% 
  summarise(avg_reported_delay = mean(report_delay, na.rm = TRUE))
country_delay

####9. Create a histogram using the 'duration seconds' column.
ufo$duration.seconds
hist(ufo$duration.seconds, main = "Duration of UFO sightings in seconds", xlab = "Seconds")

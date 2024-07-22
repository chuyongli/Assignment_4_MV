#' Mausam Vadakkayil
#' BTC1855: Assignment 4

#Read the data into a dataframe
ufo <- as.data.frame(read.csv("/Users/mausamvk/Desktop/BTC1855/ufo_subset.csv"))

#Check and correct structural issues
str(ufo) #here we can see some issues, "datetime" includes two data the time and date, "datetime" is a character, date_posted" is character

####1. split "datetime" info into 2 seperate coloumns, date of sighting and time of sighting
# (TP) I like that you are separating this so that you have one type of data in 
# (TP) one column. The approach below that you used assumes that the data inserted
# (TP) is always in the expected format. It may be a good idea to use a different 
# (TP) approach so that you can handle any possible variations or missing values
# (TP) in the datetime format. For instance, using lubridate's functions may 
# (TP) allow for a more robust approach that ensures your date and time 
# (TP) extraction works even if the input format varies.

ufo$date_sighting <- substr(ufo$datetime, start = 1, stop = 10) # first split date info into a new date_sighting coloumn, by using substring to split the first 10 characters of the date
names(ufo) #here we can now see a new coloumn called date_sighting

ufo$time_sighting <- substr(ufo$datetime, start = 11, stop = 16) #split time info into a new time_sighting coloumn, by using substring to split the 11th character to the 16th character (which are all time)
names(ufo) #here we can now see a new coloumn called time_sighting

# (TP) I like that you removed the datetime column so that it makes the
# (TP) datafrmae easier to read and ensures the relevatn time and date 
# (TP) information is readily accessible. However, manually specifying the 
# (TP) column indices can be error-prone, especially if the datetime structure 
# (TP) changes in the future. Instead, try specifying the columns by name to 
# (TP) make the code easier to maintain and avoids the manual count.
ufo <- subset(ufo, select=c(13,12,2,3,4,5,6,7,8,9,10,11)) #remove the datetime coloumn as its info has been transferred to last 2 coloumns, and reorder the last two coloumns to first and second position
# (TP) Good practice to verify the structure of the dataframe after changes!
str(ufo) #here we see first two coloumns are now time_sighting and date_sighting, and orginal datetime coloumn has been removed


####2. Convert date_sighting to numeric for downstream analysis
# upon looking through a few values of the dates it seems the setup is YY - MM- DD, so we can use ymd()
# (TP) I'm not sure if I missed a block of code involving ymd()?

#Your downstream analysis will require the variables 'country', 'shape' and 'duration seconds'. Analyze these columns, identify any problems (e.g. missingness, outliers, inconsistency etc) and implement your solution (e.g. remove, impute etc).

####3. Country analysis 
# (TP) Your approach of converting the country column to a factor to check all
# (TP) possible values is great! This lets you see all possible values - not just
# (TP) missing ones but also other potentially nonsense values. Another way to 
# (TP) remove the missing values is using the dplyr package and using the 
# (TP) filter + pipe + droplevels function to make the code more readable. 
# (TP) However, this is more of a suggestion. Your approach works well!

ufo$country
ufo$country <- as.factor(ufo$country) #convert countries to level
levels(ufo$country) #here we can see 6 levels, one of which is mssing values
ufo <- droplevels(ufo[!ufo$country == "",]) #drop level in country coloumn with "" (denotes missing values)
levels(ufo$country) #now we see it has 5 levels: au, ca, de, gb and us
plot(ufo$country) #genertes plot of country counts, and here we see no missing values 

####4. shape analysis 
# (TP) Similar to the previous step, your approach works well! A different
# (TP) approach is to use dplyr, pipe, filter, and droplevels functions to
# (TP) remove the missing values and the unknowns. This can improve readability
# (TP) of the code!
ufo$shape #we again can see here "" values and "unknown" values
ufo$shape <- as.factor(ufo$shape) #convert to factor so we can see the groups
levels(ufo$shape) # here we see "" and "unkown", 22 levels
plot(ufo$shape) #to view the groupings
ufo <- droplevels(ufo[!ufo$shape == "",])
ufo <- droplevels(ufo[!ufo$shape == "unknown",])
levels(ufo$shape) #20 levels now
plot(ufo$shape) #plot again and see the missing/unkwown are gone 

####5. durations seconds analysis 
# (TP) Great use of summary and boxplot functions to determine if there are
# (TP) outliers! Good job figuring this out and removing outliers using IQR!
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
# (TP) The str_view function is not working for me - it will be a good idea to 
# (TP) include what library you need to use this function!
str_view(ufo$comments, pattern = "HOAX") #can see HOAX
# (TP) Duplicate code!
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
# (TP) Nice use of lubridate for date conversion, especially using the
# (TP) date_posted column to make sure they are the same format. I like that you
# (TP) double checked their object types. Make sure you add another function to
# (TP) check for missing values!

#### 8. Create a table with the average report_delay per country.
# (TP) This is great use of dplyr. Nice code that is clear and concise. You also
# (TP) checked to make sure there were no missing values. Great job!
library(dplyr)

country_delay <- ufo %>% 
  group_by(country) %>% 
  summarise(avg_reported_delay = mean(report_delay, na.rm = TRUE))
country_delay

####9. Create a histogram using the 'duration seconds' column.
# (TP) Straightforward, clear, and concise. I like that you made sure there is a
# (TP) main title and an appropriate x-axis label. It might also be a good idea
# (TP) to use log10, since the range of the seconds spans several orders of
# (TP) magnitidue. With log10, you can make the histogram more interpretable.
ufo$duration.seconds
hist(ufo$duration.seconds, main = "Duration of UFO sightings in seconds", xlab = "Seconds")

# (TP) Overall, great job Mausam! Your code was very clear and concise. Your
# (TP) code did the tasks required and I really liked the way you consistently
# (TP) checked the structure, missing information, and etc. of the data. Nice
# (TP) job!


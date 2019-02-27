# read student data
students <- read.csv("students_with_dates.csv")

# Preview students with str()
str(students)

# Coerce Grades to character
students$Grades <- as.character(students$Grades)

# Coerce Medu to factor
students$Medu <- as.factor(students$Medu)

# Coerce Fedu to factor
students$Fedu <- as.factor(students$Fedu)
    
# Look at students once more with str()
str(students)

###

# Load the lubridate package
library(lubridate)
library(stringr)

# Parse as date
dmy("17 Sep 2015")

# Parse as date and time (with no seconds!)
mdy_hms("July 15, 2012 12:56:32")

# Coerce dob to a date (with no time)
students$dob <- ymd(students$dob)

# Coerce nurse_visit to a date and time
students$nurse_visit <- ymd_hms(students$nurse_visit)
    
# Look at students once more with str()
str(students)

# Detect all dates of birth after 2000
t <- ymd("2000 1 1")
students$dob[students$dob > t]
filter(students, students$dob > t)

###

# Detect all dates of birth (dob) in 1997
str_detect(students$dob, "1997")

# In the sex column, replace "F" with "Female" ...
students$sex <- str_replace(students$sex, "F", "Female")

# ... and "M" with "Male"
students$sex <- str_replace(students$sex, "M", "Male")

# View the head of students
head(students)

###

library(dplyr)
library(tidyr)
library(lubridate)

data <- read.csv("data.csv")

dim(data)
str(data)
names(data)
summary(data)
head(data)

city <- select(data, wsid, wsnm, elvt, lat, lon, inme, city, prov)
city <- unique(city)
city

data2 <- select(data, wsid, mdct:gust)
str(data2)

data2$mdct <- as.character(data2$mdct)
str(data2)
data2 <- select(data2, mdct, prcp:gust)
str(data2)
data2$mdct <- ymd_hms(data2$mdct)
str(data2)

summary(data2)
data3 <- filter(data2, is.na(prcp) | prcp != 0)

filter(data2, prcp != 0 , stp != 0)
summary(data3)

filter(data3, stp == 0)
data3$stp[data3$stp == 0] <- NA
data3$smax[data3$smax == 0] <- NA
data3$smin[data3$smin == 0] <- NA
summary(data3)


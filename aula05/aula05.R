# aula05 - Bioestatística

library(dplyr)
library(tidyr)

# Exercício

# Trabalhe com o dados de iris e manipule os dados de forma que
# ele apresente as seguintes estruturas:

iris

# a)

#  Species  Part Measure Value
#1  setosa Sepal  Length   5.1
#2  setosa Sepal  Length   4.9
#3  setosa Sepal  Length   4.7
#4  setosa Sepal  Length   4.6
#5  setosa Sepal  Length   5.0
#6  setosa Sepal  Length   5.4


# b)

#  Species Flower  Part Length Width
#1  setosa      1 Petal    1.4   0.2
#2  setosa      1 Sepal    5.1   3.5
#3  setosa      2 Petal    1.4   0.2
#4  setosa      2 Sepal    4.9   3.0
#5  setosa      3 Petal    1.3   0.2
#6  setosa      3 Sepal    4.7   3.2


############################

# as.<class>()

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


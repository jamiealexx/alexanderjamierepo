# Jamie Alexander
# Codefile


`City_Service_Requests_in_2017.(1)` <- read.csv("C:/Users/JAMIE/Downloads/City_Service_Requests_in_2017 (1).csv")
library(tidyverse)
library(dplyr)
library(lubridate)
#install.packages("gmodels")
#install.packages("pastecs")
#install.packages("plotly")
library(plotly)

attach(`City_Service_Requests_in_2017.(1)`)
data <- `City_Service_Requests_in_2017.(1)`
data1 <- `City_Service_Requests_in_2017.(1)`[ which(WARD==1),]
data2 <- `City_Service_Requests_in_2017.(1)`[ which(WARD==2),]
data3 <- `City_Service_Requests_in_2017.(1)`[ which(WARD==3),]
data4 <- `City_Service_Requests_in_2017.(1)`[ which(WARD==4),]
data5 <- `City_Service_Requests_in_2017.(1)`[ which(WARD==5),]
data6 <- `City_Service_Requests_in_2017.(1)`[ which(WARD==6),]
data7 <- `City_Service_Requests_in_2017.(1)`[ which(WARD==7),]
data8 <- `City_Service_Requests_in_2017.(1)`[ which(WARD==8),]
detach(`City_Service_Requests_in_2017.(1)`)

summarise(data3)
table(data3$SERVICECODEDESCRIPTION)
table(data8$SERVICECODEDESCRIPTION)


# Make a list for ward 8 and the amount of services requested in descending order
# of how many times that service was requested

sort(table(data8$SERVICECODEDESCRIPTION),decreasing=T)

# Bulk Collection - 5472 
# Parking Enforcement - 2814 
# Abandoned Vehicle - On Public Property - 1129 
# Sanitation Enforcement - 973 
# Alley Cleaning - 818 

# Make a list for ward 3 and the amount of services requested in descending order
# of how many times that service was requested
sort(table(data3$SERVICECODEDESCRIPTION), decreasing=T)

# Parking Meter Repair - 4629 
# Bulk Collection - 4571 
# Roadway Signs - 2346 
# Parking Enforcement - 2329 
# Pothole - 1559 

# Make a list for ward 1 and the amount of services requested in descending order
# of how many times that service was requested
sort(table(data1$SERVICECODEDESCRIPTION),decreasing=T)
# Parking Enforcement - 5833 
# Bulk Collection - 4588 
# Emergency No-Parking Verification - 2671 
# Parking Meter Repair - 2566 
# Roadway Signs -  2319 

# Make a list for ward 2and the amount of services requested in descending order
# of how many times that service was requested

sort(table(data2$SERVICECODEDESCRIPTION),decreasing=T)
# Parking Meter Repair - 17966
# Parking Enforcement - 6874
# Emergency No Parking Verification 3808
# Roadway Signs 3388
# Bulk Collection 1989


# Make a list for ward 4and the amount of services requested in descending order
# of how many times that service was requested
sort(table(data4$SERVICECODEDESCRIPTION),decreasing=T)
# Bulk Collection 9479
#Parking Enforcement 3137
#Streetlight Repair Investigation -  2394 
#Roadway Signs- 2246 
# Trash Collection - Missed - 1635 



# Make a list for ward 5 and the amount of services requested in descending order
# of how many times that service was requested
sort(table(data5$SERVICECODEDESCRIPTION),decreasing=T)

# Bulk Collection - 8433 
# Parking Enforcement - 4834
# Sanitation Enforcement - 2080
# Residential Parking Permit Violation - 1644 
# Roadway Signs - 1643

# Make a list for ward 6 and the amount of services requested in descending order
# of how many times that service was requested
sort(table(data6$SERVICECODEDESCRIPTION),decreasing=T)
# Parking Enforcement- 8181
# Bulk Collection- 7384 
# Parking Meter Repair- 4539 
# Emergency No-Parking Verification - 3045
# Residential Parking Permit Violation - 2221 

# Make a list for ward 7 and the amount of services requested in descending order
# of how many times that service was requested
sort(table(data7$SERVICECODEDESCRIPTION),decreasing=T)
# Bulk Collection- 8710 
# Parking Enforcement- 3045
# Alley Cleaning _ 1606 
# Sanitation Enforcement _ 1512
# Streetlight Repair Investigation - 1386 



# Date and Time
# Create a column that shows how long it takes to resolve case

as.Date(data3$RESOLUTIONDATE)
difftime(as.Date(data3$RESOLUTIONDATE),as.Date(data3$SERVICEORDERDATE),units="days")
ResolveDays <- difftime(as.Date(data3$RESOLUTIONDATE),as.Date(data3$SERVICEORDERDATE),units="days")
data$ResolveDays <- difftime(as.Date(data$RESOLUTIONDATE),as.Date(data$SERVICEORDERDATE),units="days")


data3$ResolveDays <- difftime(as.Date(data3$RESOLUTIONDATE),as.Date(data3$SERVICEORDERDATE),units="days")

data8$ResolveDays <- difftime(as.Date(data8$RESOLUTIONDATE),as.Date(data8$SERVICEORDERDATE),units="days")

ResolveDays <- difftime(as.Date(data1$RESOLUTIONDATE),as.Date(data1$SERVICEORDERDATE),units="days")
data1$ResolveDays <- difftime(as.Date(data1$RESOLUTIONDATE),as.Date(data1$SERVICEORDERDATE),units="days")

data2$ResolveDays <- difftime(as.Date(data2$RESOLUTIONDATE),as.Date(data2$SERVICEORDERDATE),units="days")

data4$ResolveDays <- difftime(as.Date(data4$RESOLUTIONDATE),as.Date(data4$SERVICEORDERDATE),units="days")

data5$ResolveDays <- difftime(as.Date(data5$RESOLUTIONDATE),as.Date(data5$SERVICEORDERDATE),units="days")

data6$ResolveDays <- difftime(as.Date(data6$RESOLUTIONDATE),as.Date(data6$SERVICEORDERDATE),units="days")

data7$ResolveDays <- difftime(as.Date(data7$RESOLUTIONDATE),as.Date(data7$SERVICEORDERDATE),units="days")


# Summary Statistics for how long it takes to resolve a case in WARD 3

options(scipen=100)
options(digits=2)
stat.desc(data3$ResolveDays)

# In ward 3 it takes on average less than 15 days to resolve a case with a standard 
# deviation of 41.43. The maximum amount of days it takes to resolve a case 
# is 444 days, about a year and 3 months. 


# Summary statistics for how long it takes to resolve a case in WARD 8

stat.desc(data8$ResolveDays)

# In ward 8 it takes on average less than 13 days to resolve a case with a standard
# deviation of thirty days. The maximum amount of days it takes to resolve a case is 
# 443 days, about a year and 3 months.

stat.desc(data1$ResolveDays)
# 14 days 

stat.desc(data2$ResolveDays)
# In ward 2 it takes on average 14 days to resolve a case 

stat.desc(data4$ResolveDays)
# In ward 4 it takes on average 15days to resolve a case 

stat.desc(data5$ResolveDays)
#In ward 5 it takes on average 12 days to resolve a case 

stat.desc(data6$ResolveDays)
# In ward 6 it takes on average 11 days to resolve a case 

stat.desc(data7$ResolveDays)
# In ward 7 it takes on average 12 1/2 days to resolve a case 

avgdays <- matrix(c(14,1,14,2,15,3,15,4,12,5,11,6,12.5,7,13,8),ncol=2,byrow=TRUE)
colnames(avgdays) <- c("Days", "WARD")
avgdays <- as.table(avgdays)

avgdays
# Barplot of the top 3 requested services in WARD 3
temp3<- sort(table(data3$SERVICECODEDESCRIPTION),decreasing=T)
temp3 <- temp3[c((1:3))]
barplot(temp3, main="Most Requested Services: WARD 3", xlab= "Service Type")


# Barplot of the top three requested services in ward 8

temp8 <- sort(table(data8$SERVICECODEDESCRIPTION),decreasing=T)
temp8 <- temp8[c((1:3))]
barplot(temp8, main= "WARD 8", xlab="Service Type")


# Barplot of top three services requested in ward 2
temp2 <- sort(table(data2$SERVICECODEDESCRIPTION),decreasing=T)
temp2 <- temp2[c((1:3))]
barplot(temp2, main= "WARD 2", xlab="Service Type")

# Barplot of top three services requested in ward 1
temp1 <- sort(table(data1$SERVICECODEDESCRIPTION),decreasing=T)
temp1 <- temp1[c((1:3))]
barplot(temp1, main= "WARD 1", xlab="Service Type")

#Barplot of top three services requested in ward 4
temp4 <- sort(table(data4$SERVICECODEDESCRIPTION),decreasing=T)
temp4 <- temp4[c((1:3))]
barplot(temp4, main= "WARD 4", xlab="Service Type")

# Barplot of top three services requested in ward 5
temp5 <- sort(table(data5$SERVICECODEDESCRIPTION),decreasing=T)
temp5 <- temp5[c((1:3))]
barplot(temp5, main= "WARD 5", xlab="Service Type")

# Barplot of top three services requested in ward 6
temp6 <- sort(table(data6$SERVICECODEDESCRIPTION),decreasing=T)
temp6 <- temp6[c((1:3))]
barplot(temp6, main= "WARD 6", xlab="Service Type")

# Barplot of top three services requested in ward 7
temp7 <- sort(table(data7$SERVICECODEDESCRIPTION),decreasing=T)
temp7 <- temp7[c((1:3))]
barplot(temp7, main= "WARD 7", xlab="Service Type")


# Which Ward requests the most service?

crosstab(test, row.vars = "SERVICECODE", col.vars = "WARD", type = "f")

# In order of most requested to least requested services: 2, 6, 5, 4, 1, 7, 3, 8

# Slicing data:

# Creating the new variable called neworderdate and making it read as a date from
# the SERVICEORDERDATE variable

data3$neworderdate <- substr(as.character(data3$SERVICEORDERDATE), 1,10)
data3$neworderdate <- as.Date(data3$neworderdate)
class(data3$neworderdate)

# Creating the new variable called duedate and making it read as a date from the
# SERVICEDUEDATE variable 

data3$duedate <- substr(as.character(data3$SERVICEDUEDATE), 1,10)
data3$duedate <- as.Date(data3$duedate)
class(data3$duedate)

# Creating a new variable called ExpectedDueDate which takes the difference between
# the neworderdate and the due date
ExpectedResolveDays <- data3$duedate-data3$neworderdate
data3$ExpectedResolveDays <- data3$duedate-data3$neworderdate

# Finding out how much time is saved or lost 
data3$Diff_ExpectedActual <- data3$ExpectedResolveDays-data3$ResolveDays

nrow(data3[data3$Diff_ExpectedActual>=0, ])

# There are 26,464 values that are positive meaning that the services are being 
# resolved quicker than their due dates.

nrow(data3[data3$Diff_ExpectedActual<0, ])
# There are 6278 values that are negative, meaning that these services took longer 
# than there expected due date. 

View(table_overdue)

# Creating a bar graph of the services that took longer to resolve than the 
# due date to resolve


barplot(table_overdue, main="Overdue Services between all 8 Wards", ylab="Frequency", xlab="", las=2)


# Creating expected resolution days variable and finding out the amount of days between 
# actual resolve days and expected resolve days. Making a bar graph of overdue services


data8$neworderdate8 <- substr(as.character(data8$SERVICEORDERDATE), 1,10)
data8$neworderdate8 <- as.Date(data8$neworderdate8)
class(data8$neworderdate8)
data8$duedate8 <- substr(as.character(data8$SERVICEDUEDATE), 1,10)
data8$duedate8 <- as.Date(data8$duedate8)
class(data8$duedate8)

ExpectedResolveDays8 <- data8$duedate8-data8$neworderdate8
data8$ExpectedResolveDays8 <- data8$duedate8-data8$neworderdate8

data8$Diff_ExpectedActual8 <- data8$ExpectedDueDate8-data8$ResolveDays
nrow(data8[data8$Diff_ExpectedActual8>=0, ])

# There were 21,167 requests in ward 8 that were resolved faster than expected 

nrow(data8[data8$Diff_ExpectedActual8<0, ])
# There were 3,532 requests in ward 8 that were overdue. 

table_overdue3 <- table(Servicetype$SERVICETYPECODEDESCRIPTION)
table_overdue8 <- table(Servicetype$SERVICETYPECODEDESCRIPTION)
View(table_overdue8)

# Ward 1 creating new variables
data1$neworderdate1 <- substr(as.character(data1$SERVICEORDERDATE), 1,10)
data1$neworderdate1 <- as.Date(data1$neworderdate1)
class(data1$neworderdate1)
data1$duedate1 <- substr(as.character(data1$SERVICEDUEDATE), 1,10)
data1$duedate1 <- as.Date(data1$duedate1)
class(data1$duedate1)

ExpectedDueDate1 <- data1$duedate1-data1$neworderdate1
data1$ExpectedDueDate1 <- data1$duedate1-data1$neworderdate1

data1$Diff_ExpectedActual1 <- data1$ExpectedDueDate1-data1$ResolveDays
nrow(data1[data1$Diff_ExpectedActual1>=0, ])

# Original Data --- adding new variables
data$neworderdate <- substr(as.character(data$SERVICEORDERDATE), 1,10)
data$neworderdate <- as.Date(data$neworderdate)
class(data$neworderdate)
data$duedate <- substr(as.character(data$SERVICEDUEDATE), 1,10)
data$duedate <- as.Date(data$duedate)
class(data$duedate)

ExpectedDueDate <- data$duedate-data$neworderdate
data$ExpectedDueDate <- data$duedate-data$neworderdate

data$Diff_ExpectedActual <- data$ExpectedDueDate-data$ResolveDays
nrow(data[data$Diff_ExpectedActual>=0, ])

# table of overdue services in DC
table_overdue <- table(data$SERVICETYPECODEDESCRIPTION)

# barplot of overdue services in DC
View(barplot(table_overdue, main="Overdue Services in D.C.", ylab="Frequency", xlab="", las=2))


my_df <- as.data.frame(table_overdue)[order(as.data.frame(table_overdue)$Freq,decreasing = TRUE),]
old_par <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp <- barplot(my_df$Freq, main = "Administrations with Overdue Services", 
        space=1, axes = FALSE, axisnames = FALSE)
labels <- my_df$Var1
text(mp, par("usr")[3], labels = labels, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2)



       
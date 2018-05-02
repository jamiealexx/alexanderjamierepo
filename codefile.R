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

View(table_overdue)

# Creating a bar graph of the services that took longer to resolve than the 
# due date to resolve


barplot(table_overdue, main="Overdue Services between all 8 Wards", ylab="Frequency", xlab="", las=2)


# Creating expected resolution days variable and finding out the amount of days between 
# actual resolve days and expected resolve days. Making a bar graph of overdue services

table_overdue3 <- table(Servicetype$SERVICETYPECODEDESCRIPTION)
table_overdue8 <- table(Servicetype$SERVICETYPECODEDESCRIPTION)
View(table_overdue8)


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
# There were 33,025 requests in ward 1 that were resolved on or before their due date.

nrow(data1[data1$Diff_ExpectedActual1<0, ])
# There were 5864 requests in Ward 1 that were resolved after their due date. 


# Ward 2 --- adding new variables
data2$neworderdate2 <- substr(as.character(data2$SERVICEORDERDATE), 1,10)
data2$neworderdate2 <- as.Date(data2$neworderdate2)
class(data2$neworderdate2)
data2$duedate2 <- substr(as.character(data2$SERVICEDUEDATE), 1,10)
data2$duedate2 <- as.Date(data2$duedate2)
class(data2$duedate2)

ExpectedDueDate2 <- data2$duedate2-data2$neworderdate2
data2$ExpectedDueDate2 <- data2$duedate2-data2$neworderdate2

data2$Diff_ExpectedActual2 <- data2$ExpectedDueDate2-data2$ResolveDays
nrow(data2[data2$Diff_ExpectedActual2>=0, ])
# There were 43,952 requests in Ward 2 closed on or before their due date 

nrow(data2[data2$Diff_ExpectedActual2<0, ])
# There were 11,305 requests in Ward 2 that were overdue

# Ward 3 --- adding new variables

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

# Ward 4 --- adding new variables
data4$neworderdate4 <- substr(as.character(data4$SERVICEORDERDATE), 1,10)
data4$neworderdate4 <- as.Date(data4$neworderdate4)
class(data4$neworderdate4)
data4$duedate4 <- substr(as.character(data4$SERVICEDUEDATE), 1,10)
data4$duedate4 <- as.Date(data4$duedate4)
class(data4$duedate4)

ExpectedDueDate4 <- data4$duedate4-data4$neworderdate4
data4$ExpectedDueDate4 <- data4$duedate4-data4$neworderdate4

data4$Diff_ExpectedActual4 <- data4$ExpectedDueDate4-data4$ResolveDays
nrow(data4[data4$Diff_ExpectedActual4>=0, ])
# There were 38,833 requests in Ward 4 closed on or before their due date. 

nrow(data4[data4$Diff_ExpectedActual4<0, ])
# There were 6,707 requests in Ward 4 that were closed after their due date.

# Ward 5 --- adding new variables
data5$neworderdate5 <- substr(as.character(data5$SERVICEORDERDATE), 1,10)
data5$neworderdate5 <- as.Date(data5$neworderdate5)
class(data5$neworderdate5)
data5$duedate5 <- substr(as.character(data5$SERVICEDUEDATE), 1,10)
data5$duedate5 <- as.Date(data5$duedate5)
class(data5$duedate5)

ExpectedDueDate5 <- data5$duedate5-data5$neworderdate5
data5$ExpectedDueDate5 <- data5$duedate5-data5$neworderdate5

data5$Diff_ExpectedActual5 <- data5$ExpectedDueDate5-data5$ResolveDays
nrow(data5[data5$Diff_ExpectedActual5>=0, ])
# There were 39,123 requests in Ward 5 closed on or before their due date. 

nrow(data5[data5$Diff_ExpectedActual5<0, ])
# There were  6,415 requests in Ward 5 that were closed after their due date.

# Ward 6 -- adding new variables 
data6$neworderdate6 <- substr(as.character(data6$SERVICEORDERDATE), 1,10)
data6$neworderdate6 <- as.Date(data6$neworderdate6)
class(data6$neworderdate6)
data6$duedate6 <- substr(as.character(data6$SERVICEDUEDATE), 1,10)
data6$duedate6 <- as.Date(data6$duedate6)
class(data6$duedate6)

ExpectedDueDate6 <- data6$duedate6-data6$neworderdate6
data6$ExpectedDueDate6 <- data6$duedate6-data6$neworderdate6

data6$Diff_ExpectedActual6 <- data6$ExpectedDueDate6-data6$ResolveDays
nrow(data6[data6$Diff_ExpectedActual6>=0, ])
# There were 45,261 requests in Ward 6 closed on or before their due date. 

nrow(data6[data6$Diff_ExpectedActual6<0, ])
# There were  7,746 requests in Ward 6 that were closed after their due date.

# Ward 7 --- new variable 
data7$neworderdate7 <- substr(as.character(data7$SERVICEORDERDATE), 1,10)
data7$neworderdate7 <- as.Date(data7$neworderdate7)
class(data7$neworderdate7)
data7$duedate7 <- substr(as.character(data7$SERVICEDUEDATE), 1,10)
data7$duedate7 <- as.Date(data7$duedate7)
class(data7$duedate7)

ExpectedDueDate7 <- data7$duedate7-data7$neworderdate7
data7$ExpectedDueDate7 <- data7$duedate7-data7$neworderdate7

data7$Diff_ExpectedActual7 <- data7$ExpectedDueDate7-data7$ResolveDays
nrow(data7[data7$Diff_ExpectedActual7>=0, ])
# There were 30,705 requests in Ward 7 closed on or before their due date. 

nrow(data7[data7$Diff_ExpectedActual7<0, ])
# There were 4,966 requests in Ward 7 that were closed after their due date.

# Ward 8 -- creating new variables 
data8$neworderdate8 <- substr(as.character(data8$SERVICEORDERDATE), 1,10)
data8$neworderdate8 <- as.Date(data8$neworderdate8)
class(data8$neworderdate8)
data8$duedate8 <- substr(as.character(data8$SERVICEDUEDATE), 1,10)
data8$duedate8 <- as.Date(data8$duedate8)
class(data8$duedate8)

ExpectedDueDate8 <- data8$duedate8-data8$neworderdate8
data8$ExpectedDueDate8 <- data8$duedate8-data8$neworderdate8

data8$Diff_ExpectedActual8 <- data8$ExpectedDueDate8-data8$ResolveDays
nrow(data8[data8$Diff_ExpectedActual8>=0, ])
# There were 21,167 requests in ward 8 that were resolved faster than expected 

nrow(data8[data8$Diff_ExpectedActual8<0, ])
# There were 3,532 requests in ward 8 that were overdue. 

# table of overdue services in DC
table_overdue_byadmin <- table(data$SERVICETYPECODEDESCRIPTION)

# barplot of administrations that produce overdue services in DC 
View(barplot(table_overdue_byadmin, main="Overdue Services in D.C.", ylab="Frequency", xlab="", las=2))


my_df_admin <- as.data.frame(table_overdue_byadmin)[order(as.data.frame(table_overdue_byadmin)$Freq,decreasing = TRUE),]
old_par_admin <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp_admin <- barplot(my_df_admin$Freq, main = "Administrations with Overdue Services", 
        space=1, axes = FALSE, axisnames = FALSE)
labels_admin <- my_df_admin$Var1
text(mp, par("usr")[3], labels = labels_admin, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2)


# barplot of overdue services in DC
table_overdue_services <- table(data$SERVICECODEDESCRIPTION)
my_df_services <- as.data.frame(table_overdue_services)[order(as.data.frame(table_overdue_services)$Freq,decreasing = TRUE),]
old_par_services <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp_services <- barplot(my_df_services$Freq, main = "Overdue Services in D.C.", 
              space=1, axes = FALSE, axisnames = FALSE)
labels_services <- my_df_services$Var1
text(mp_services, par("usr")[3], labels = labels_services, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2)       


# Ward 1 -- overdue services barplot
table_overdue1 <- table(data1$SERVICECODEDESCRIPTION)
my_df1 <- as.data.frame(table_overdue1)[order(as.data.frame(table_overdue1)$Freq,decreasing = TRUE),]
old_par1 <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp1 <- barplot(my_df1$Freq, main = "Overdue Services in Ward 1", 
              space=1, axes = FALSE, axisnames = FALSE)
labels1 <- my_df1$Var1
text(mp, par("usr")[3], labels = labels1, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2) 

# Ward 2 -- overdue services barplot
table_overdue2 <- table(data2$SERVICECODEDESCRIPTION)
my_df2 <- as.data.frame(table_overdue2)[order(as.data.frame(table_overdue2)$Freq,decreasing = TRUE),]
old_par2 <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp2 <- barplot(my_df2$Freq, main = "Overdue Services in Ward 2", 
               space=1, axes = FALSE, axisnames = FALSE)
labels2 <- my_df2$Var1
text(mp, par("usr")[3], labels = labels1, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2) 

# Ward 3 -- overdue services barplot
table_overdue3 <- table(data3$SERVICECODEDESCRIPTION)
my_df3 <- as.data.frame(table_overdue3)[order(as.data.frame(table_overdue3)$Freq,decreasing = TRUE),]
old_par3 <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp3 <- barplot(my_df3$Freq, main = "Overdue Services in Ward 3", 
               space=1, axes = FALSE, axisnames = FALSE)
labels3 <- my_df3$Var1
text(mp, par("usr")[3], labels = labels3, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2) 

# Ward 4 --- overdue services barplot
table_overdue4 <- table(data4$SERVICECODEDESCRIPTION)
my_df4 <- as.data.frame(table_overdue4)[order(as.data.frame(table_overdue4)$Freq,decreasing = TRUE),]
old_par4 <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp4 <- barplot(my_df4$Freq, main = "Overdue Services in Ward 4", 
               space=1, axes = FALSE, axisnames = FALSE)
labels4 <- my_df4$Var1
text(mp, par("usr")[3], labels = labels4, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2) 

# Ward 5 --- overdue services barplot
table_overdue5 <- table(data5$SERVICECODEDESCRIPTION)
my_df5 <- as.data.frame(table_overdue5)[order(as.data.frame(table_overdue)$Freq,decreasing = TRUE),]
old_par5 <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp5 <- barplot(my_df5$Freq, main = "Overdue Services in Ward 5", 
               space=1, axes = FALSE, axisnames = FALSE)
labels5 <- my_df5$Var1
text(mp, par("usr")[3], labels = labels5, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2) 

# Ward 6 -- overdue services barplot
table_overdue6 <- table(data6$SERVICECODEDESCRIPTION)
my_df6 <- as.data.frame(table_overdue6)[order(as.data.frame(table_overdue)$Freq,decreasing = TRUE),]
old_par6 <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp6 <- barplot(my_df6$Freq, main = "Overdue Services in Ward 6", 
               space=1, axes = FALSE, axisnames = FALSE)
labels6 <- my_df6$Var1
text(mp, par("usr")[3], labels = labels6, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2) 

# Ward 7 --- overdue requests barplot
table_overdue7 <- table(data7$SERVICECODEDESCRIPTION)
my_df7 <- as.data.frame(table_overdue7)[order(as.data.frame(table_overdue)$Freq,decreasing = TRUE),]
old_par7 <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp7 <- barplot(my_df7$Freq, main = "Overdue Services in Ward 7", 
               space=1, axes = FALSE, axisnames = FALSE)
labels7 <- my_df7$Var1
text(mp, par("usr")[3], labels = labels7, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2) 

# Ward 8 --- overdue requests barplot
table_overdue8 <- table(data8$SERVICECODEDESCRIPTION)
my_df8 <- as.data.frame(table_overdue8)[order(as.data.frame(table_overdue)$Freq,decreasing = TRUE),]
old_par8 <- par()
par(mar=c(15.1,6.1,4.1,2.1))
mp8 <- barplot(my_df8$Freq, main = "Overdue Services in Ward 8", 
               space=1, axes = FALSE, axisnames = FALSE)
labels8 <- my_df8$Var1
text(mp, par("usr")[3], labels = labels8, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
axis(2) 
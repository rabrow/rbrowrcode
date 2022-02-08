#### PSCI 107
## Problem Set #2
## 21249856
## March 3, 2021

setwd("~/Dropbox/PSCI107")

require(tidyverse)

#loading the MLB data
load("RawData/PS2.2019MLBTeamsData.Rdata")

# Question 1

#a. Generating a variable, avg.attendance.home, for average attendance per 
#   home game using home.attendance and games.played

bb$avg.attendance.home <- bb$home.attendance/(.5*bb$games.played)
head(bb$avg.attendance.home)
summary(bb$avg.attendance.home)

#Finding median average attendance
median(bb$avg.attendance.home,na.rm = T)

#First subsetting the data to only team.name and avg.attendance.home
bb1 = bb[,c(1,42)]
#Then creating a function to find median
median.team = function(df){
        df = na.omit(df)
        df = df[order(df[,2]),]
        if (dim(df)[1] %% 2 == 0) {
                mid = dim(df)[1]/2
                midNext = mid + 1
                team1 = df[mid,1]
                team2 = df[midNext,1]
                sprintf("%s and %s", team1, team2)
        }
        else {
                medianV = median(df[,2])
                median.team = df[,1][which(df[,2] == medianV)]
                sprintf("%s", median.team)
        }
}

median.team(bb1)
# see Word doc for explanation of median

#Finding maximum average attendance
max(bb$avg.attendance.home,na.rm = T)
bb$team.name[which.max(bb$avg.attendance.home)]

#Finding minimum average attendance
min(bb$avg.attendance.home,na.rm = T)
bb$team.name[which.min(bb$avg.attendance.home)]

#b. Finding if home attendance differs between AL and NL using boxplot()

boxplot(bb$home.attendance ~ bb$league,
xlab="League",
ylab="Home Attendance",
main="Distribution of Home Attendance for 
American League and National League")

# see Word doc for analysis

#c. Generating a variable, team.batting.avg, using hits and at.bats

bb$team.batting.avg <- bb$hits/bb$at.bats
head(bb$team.batting.avg)
summary(bb$team.batting.avg)

#Which team had the median batting average?
median(bb$team.batting.avg,na.rm = T)

#Subsetting the data to only team.name and team.batting.avg
bb2 = bb[,c(1,43)]
#Using the function median.team to find median
median.team(bb2)
 
#Which team had the max batting average?
max(bb$team.batting.avg,na.rm = T)
bb$team.name[which.max(bb$team.batting.avg)]

#Which team had the min batting average?
min(bb$team.batting.avg,na.rm = T)
bb$team.name[which.min(bb$team.batting.avg)]


#d. Generating a scatter plot with x=team.batting.avg and y=avg.attendance.home

plot(bb$team.batting.avg, bb$avg.attendance.home,
     main = "Team Home Attendence by Batting Average ",
     xlab = "Team Batting Average",
     ylab = "Average Home Attendance",
     pch = 16,
     col = "darkgreen")

#Using cor() to help determine strength of relationship
cor(bb$team.batting.avg, bb$avg.attendance.home, use="pairwise.complete")

#e. Generating a new variable, team.win.percentage, using wins and games.played

bb$team.win.percentage <- (bb$wins/bb$games.played)*100
head(bb$team.win.percentage)

#Plotting x=team.batting.avg and y=team.win.percentage in a scatter plot

plot(bb$team.batting.avg, bb$team.win.percentage,
     main = "Team Winning Percentage by Batting Average ",
     xlab = "Team Batting Average",
     ylab = "Winning Percentage",
     pch = 16,
     col = "darkblue")

#Using cor() to help determine strength of relationship
cor(bb$team.batting.avg, bb$team.win.percentage, use="pairwise.complete")

##Bonus: Generating a variable that better predicts team winning percentage

#Creating a variable of average home runs per game, home.runs.avg, using
# home.runs and games.played
bb$home.runs.avg <- bb$home.runs/bb$games.played


plot(bb$home.runs.avg, bb$team.win.percentage,
      main = "Team Winning Percentage by Home Runs ",
      xlab = "Average Home Run Per Game",
      ylab = "Winning Percentage",
      pch = 16,
      col = "darkred")

#Using cor() to help determine strength of relationship
cor(bb$home.runs.avg, bb$team.win.percentage, use="pairwise.complete")


# Question 2

#loading the Exit Poll data
load("RawData/exit-poll-2016.RData")

#a. See Word document

#b. Reorganizing the data from long to wide so obs. only populate one row. 

#First creating an untouched dataset
exit.untouched <- exit

#Then using spread() to condense two id rows into one, using favorable.cand as
# new column names (key) and favorable.rating as values in these columns (value)
exit <- spread(exit,
                 key = favorable.cand,
                 value = favorable.rating)

head(exit)

#c. Ensuring that each id number only appears once

table(duplicated(exit$id))

#d. Creating a new variable, educ, out of educ.hs, educ.somecoll, educ.bach, and
#    educ.postgrad and dropping the 4 old columns

exit <- gather(exit,
                 key = "educ", 
                 value = "val", 
                 educ.hs:educ.postgrad)
nrow(exit)

#Getting rid of extra rows when val == 0
exit <- exit[exit$val != 0,]
nrow(exit)

#Setting missing values to NA
exit$educ[exit$val == 99] <- NA
#Getting rid of val
exit$val <- NULL
nrow(exit)

#Ensuring there are no more extra rows
exit <- exit[!duplicated(exit$id),]
head(exit)
nrow(exit)

#e. Splitting sex.age.race into 3 separate columns: sex, age, and race

exit <- separate(exit,
                   col = "sex.age.race",
                   into = c("sex","age","race"),
                   sep = " ")

#Checking for labels of missing/unknown values
table(exit$sex)
table(exit$age)
table(exit$race)

#Coding missing/unknown values as NA
#When sex == "unknown"
exit$sex[exit$sex == "unknown"] <- NA
#When age == -999
exit$age[exit$age == -999] <- NA
#When race == "NA"
exit$race[exit$race == "NA"] <- NA


#f. Creating a new variable third.party

#First figuring out the meanings of the values in PRSPA16
attributes(exit.untouched$PRSPA16)

#Then creating third.party using PRSPA16
exit$third.party <- NA
exit$third.party[exit$PRSPA16 == 1 | exit$PRSPA16 == 2] <- 0
exit$third.party[exit$PRSPA16 == 3 | exit$PRSPA16 == 4] <- 1
exit$third.party[exit$PRSPA16 == 0 | exit$PRSPA16 == 9 ] <- NA

#Checking the new variable
table(exit$third.party,useNA = "always")

#g. Converting married into a dummy variable, 1=married and 0=not married

#First figuring out the meanings of the values in married
attributes(exit.untouched$married)

#Then recoding married
table(exit$married)
exit$married[exit$married == 1] <- 1
exit$married[exit$married == 2] <- 0

#Checking the recoded variable
table(exit$married,useNA = "always")

#h. Recoding the PHIL3 and partyid variables with meaningful labels

#PHIL3:
#First figuring out the meanings of the values in PHIL3
attributes(exit.untouched$PHIL3)

#Then recoding PHIL3
table(exit$PHIL3)
exit$PHIL3[exit$PHIL3 == 1] <- "Liberal"
exit$PHIL3[exit$PHIL3 == 2] <- "Moderate"
exit$PHIL3[exit$PHIL3 == 3] <- "Conservative"

#Checking the recoded variable
table(exit$PHIL3,useNA = "always")

#partyid:
#First figuring out the meanings of the values in partyid
attributes(exit.untouched$partyid)

#Then recoding PHIL3
table(exit$partyid)
exit$partyid[exit$partyid == 1] <- "Democrat"
exit$partyid[exit$partyid == 2] <- "Republican"
exit$partyid[exit$partyid == 3] <- "Independent"
exit$partyid[exit$partyid == 4] <- "Other"

#Checking the recoded variable
table(exit$partyid,useNA = "always")

#i. Cleaning/formatting the names of the columns

#Checking column names
names(exit)

#Making all the names lowercase
names(exit) <- tolower(names(exit))
names(exit)

#Changing prspa16 and phil3 to more descriptive names

attributes(exit.untouched$PRSPA16)
attributes(exit.untouched$PHIL3)

#Changing prspa16 to vote2016 and phil3 to pol.ideology
exit <- rename(exit,
                 vote2016 = prspa16,
                 pol.ideology = phil3)
names(exit)

#j. Analysis of third.party and race

#Creating a table, my.table, with race and third.party
my.table <- table(exit$race,exit$third.party)

#Using prop.table() to find proportions of third party voters per race category
prop.table.1 <- prop.table(my.table,1)
prop.table.1 <- round(prop.table.1,3)

#Creating a matrix with my.table and prop.table.1
new.table <- cbind(my.table,prop.table.1)

#Reordering and renaming the columns
new.table <- new.table[,c(1,3,2,4)]
colnames(new.table) = c("not.third.freq", "not.third.prop",
                        "third.freq", "third.prop")
View(new.table)

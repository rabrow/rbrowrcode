#### PSCI 107
## Midterm
## 21249856
## March 12, 2021

setwd("~/Dropbox/PSCI107")
require(tidyverse)
require(lubridate)

# Question 1

#a. Loading/Familiarizing myself with the data

monmouth <- readRDS("RawData/monmouth-poll.RDS")

#How many observations in the data?
nrow(monmouth)

#b. Cleaning up the names of the columns

#First creating untouched dataset
monmouth.untouched <- monmouth

#Checking column names
names(monmouth)

#Making all the names lowercase
names(monmouth) <- tolower(names(monmouth))
names(monmouth)

#Changing q1,2,4,5,11 to more descriptive names

attributes(monmouth$q1)
attributes(monmouth$q2)
attributes(monmouth$q4)
attributes(monmouth$q5)
attributes(monmouth$q11)

#Using tidyverse rename() for q1,2,4,5,11

monmouth <- rename(monmouth,
               trump.app = q1,
               country.direction = q2,
               covid.concern = q4,
               limit.confidence = q5,
               stress.level = q11)
names(monmouth)

#Changing underscore in race_eth to period

names(monmouth) <- gsub("_",".",names(monmouth))
names(monmouth)

#c. Finding percentages of Democrats/Very or Somewhat liberal

#Finding the percentage of Democrats who said very liberal/somewhat liberal

#First recoding party to party names
attributes(monmouth$party)
table(monmouth$party)
monmouth$party[monmouth$party == 1] <- "Republican"
monmouth$party[monmouth$party == 2] <- "Democrat"
monmouth$party[monmouth$party == 3] <- "Independent"
monmouth$party[monmouth$party == 4] <- "Other"
monmouth$party[monmouth$party == 9] <- NA
table(monmouth$party,useNA = "always")

#Then recoding ideology to consolidate ideology names
attributes(monmouth$ideology)
table(monmouth$ideology)
monmouth$ideology[monmouth$ideology == 1 | monmouth$ideology == 2] <- "Liberal"
monmouth$ideology[monmouth$ideology == 3] <- "Moderate"
monmouth$ideology[monmouth$ideology == 4 | monmouth$ideology == 5] <- "Conservative"
monmouth$ideology[monmouth$ideology == 9] <- NA
table(monmouth$ideology,useNA = "always")

#Next subsetting number of Democrats 
dem.sub <- monmouth[monmouth$party == "Democrat",]
#Then finding percentage using prop.table
table.1 <- table(dem.sub$party,dem.sub$ideology)
prop.table.1 <- prop.table(table.1,1)
prop.table.1
#Converting proportion into percentage
perc.dem.liberal <- round(((prop.table.1[,2])*100),3)
perc.dem.liberal

#Vice versa: Finding percentage of very liberal/somewhat liberal who said Democrat

#Subsetting number of liberal/somewhat liberal
lib.sub <- monmouth[monmouth$ideology == "Liberal",]
#Finding percentage using prop.table
table.2 <- table(lib.sub$ideology,lib.sub$party)
prop.table.2 <- prop.table(table.2,1)
prop.table.2
#Converting proportion into percentage
perc.liberal.dem <- round(((prop.table.2[,1])*100),3)
perc.liberal.dem

#d. Finding and comparing percentages of stress levels for men/women

#First recoding stress.level to consolidate stress levels
attributes(monmouth$stress.level)
table(monmouth$stress.level)
monmouth$stress.level[monmouth$stress.level == 1 |
                        monmouth$stress.level == 2] <- "Gone up"
monmouth$stress.level[monmouth$stress.level == 3] <- "Stayed same"
monmouth$stress.level[monmouth$stress.level == 4 |
                        monmouth$stress.level == 5] <- "Gone down"
monmouth$stress.level[monmouth$stress.level == 9] <- NA
table(monmouth$stress.level,useNA = "always")

#Then recoding gender to men/women
attributes(monmouth$gender)
table(monmouth$gender,useNA = "always")
monmouth$gender[monmouth$gender == 1] <- "Man"
monmouth$gender[monmouth$gender == 2] <- "Woman"
table(monmouth$gender)

#Then subsetting to women
women.sub <- monmouth[monmouth$gender == "Woman",]
#Finding percentage using prop.table
table.3 <- table(women.sub$gender,women.sub$stress.level)
prop.table.3 <- prop.table(table.3,1)
prop.table.3
#Converting proportion into percentage
perc.women.stress <- round(((prop.table.3[,2])*100),3)
perc.women.stress

#Doing the same process for men

#Subsetting to men
men.sub <- monmouth[monmouth$gender == "Man",]
#Finding percentage using prop.table
table.4 <- table(men.sub$gender,men.sub$stress.level)
prop.table.4 <- prop.table(table.4,1)
prop.table.4
#Converting proportion to percentage
perc.men.stress <- round(((prop.table.4[,2])*100),3)
perc.men.stress

#e. Finding how many states had 10+ respondents
table(table(monmouth$state)>10)

#Subsetting to the states which had 10+ respondents
monmouth.sub <- monmouth[monmouth$state %in% names(table(monmouth$state))
                         [table(monmouth$state)>10],]
#Confirming number of states
length(unique(monmouth.sub$state))

#Recoding limit.confidence
attributes(monmouth$limit.confidence)
table(monmouth.sub$limit.confidence)
monmouth.sub$limit.confidence[monmouth.sub$limit.confidence == 1 |
                              monmouth.sub$limit.confidence == 2] <- "Confident"
monmouth.sub$limit.confidence[monmouth.sub$limit.confidence == 3] <- "Not too confident"
monmouth.sub$limit.confidence[monmouth.sub$limit.confidence == 4] <- "Not at all confident"
monmouth.sub$limit.confidence[monmouth.sub$limit.confidence == 9] <- NA
table(monmouth.sub$limit.confidence,useNA = "always")

#Using prop.table to find highest/lowest percentage
table.5 <- table(monmouth.sub$state,monmouth.sub$limit.confidence)
prop.table.5 <- prop.table(table.5,1)
prop.table.5 <- round((prop.table.5*100),3)
prop.table.5

#State with highest percentage
which.max(prop.table.5[,1])
attributes(monmouth$state)
#State with lowest percentage
which.min(prop.table.5[,1])
attributes(monmouth$state)

# Question 2

#a. Loading/familiarizing myself with the data

load("RawData/Question2JohnsHopkins.Rdata")

table(duplicated(jh$county.fips))

#b. Reshaping the data from wide to long

jh.reshaped <- pivot_longer(jh, 
                            cols = c(10:39), 
                            names_to = "date", 
                            values_to = "cases") 
dim(jh.reshaped)

#c. Cleaning the date variable in jh.reshaped

#Getting rid of "X" in front of date
head(jh.reshaped$date)
jh.reshaped$date <- gsub("X","",jh.reshaped$date)
head(jh.reshaped$date)
#Using mdy from lubridate
jh.reshaped$date <- mdy(jh.reshaped$date)
head(jh.reshaped$date)

#Finding case totals from April 15 through 18 in New York County, NY
ny.county.sub <- jh.reshaped[jh.reshaped$county.name == "New York County" &
                 (jh.reshaped$date >= "2020-04-15" & 
                   jh.reshaped$date <= "2020-04-18"),]
ny.county.sub$cases

#d.Finding mean/median of cases

mean(jh.reshaped$cases)
median(jh.reshaped$cases)

#Making a density plot of cases
plot(density(jh.reshaped$cases,na.rm=T),
     xlab="COVID-19 Cases",
     main="Density of COVID-19 Cases Per County",
     col="dodgerblue")

#e. Creating a new variable cases.per.1000 residents

#First creating residents.1000 and divide population by 1000
jh.reshaped$residents.1000 <- jh.reshaped$population/1000
#Then creating cases.per.1000.residents
jh.reshaped$cases.per.1000.residents <- 
  jh.reshaped$cases/jh.reshaped$residents.1000

mean(jh.reshaped$cases.per.1000.residents)
median(jh.reshaped$cases.per.1000.residents)  

plot(density(jh.reshaped$cases.per.1000.residents,na.rm=T),
     xlab="COVID-19 Cases Per 1000 Residents",
     main="Density of COVID-19 Cases Per County",
     col="darkgreen")

#f. Recording median number of cases.per.1000.residents out of all counties for
## each day

#First creating a dates vector of unique dates
dates <- unique(jh.reshaped$date)

#Then creating an empty vector
jh.reshaped$median.cases.per.1000 <- NA

#Trying it out for one day, April 1
jh.reshaped$median.cases.per.1000[jh.reshaped$date == "2020-04-01"] <- 
  median(jh.reshaped$cases.per.1000.residents[jh.reshaped$date == "2020-04-01"])

#Creating a for loop
for(i in 1: length(dates)){
  jh.reshaped$median.cases.per.1000[jh.reshaped$date == dates[i]] <- 
    median(jh.reshaped$cases.per.1000.residents[jh.reshaped$date == dates[i]])
}

jh.reshaped$median.cases.per.1000
unique(jh.reshaped$median.cases.per.1000)

#Creating a scatter plot with date on x-axis, median.cases.per.1000 on y-axis
plot(jh.reshaped$date,jh.reshaped$median.cases.per.1000,
     xlab = "Date",
     ylab = "Median Cases per 1000 Residents",
     main = "Median COVID-19 Cases per 1000 Residents in April",
     col= "darkred",
     pch = 16)


#g. Recording the median number of cases.per.1000.residents
## and median population density for each state on April 30

#First subsetting to only April 30
jh.april30 <- jh.reshaped[jh.reshaped$date == "2020-04-30",]

#Then creating a states vector of unique state abbreviations
states <- unique(jh.april30$state.abbr)

#Creating empty vector for median population density
jh.april30$median.pop.density <- NA

jh.april30$median.pop.density[jh.april30$state.abbr == "AL"] <-
  median(jh.april30$population.density[jh.april30$state.abbr == "AL"])

jh.april30$median.pop.density

#Creating another for loop to find median pop density for each state
for(i in 1:length(states)){
  jh.april30$median.pop.density[jh.april30$state.abbr == states[i]] <-
    median(jh.april30$population.density[jh.april30$state.abbr == states[i]])
}

jh.april30$median.pop.density
unique(jh.april30$median.pop.density)

#Then creating empty vector median.cases.per.1000
median.cases.per.1000 <- rep(NA, length(states))

#Next creating a for loop to find median number of cases.per.1000 residents
for(i in 1:length(states)){
  median.cases.per.1000[i] <- median(jh.april30$cases.per.1000.residents[jh.april30$state.abbr==states[i]])
}

cbind(states,median.cases.per.1000)

#Attaching loop to jh.april30 dataframe
for(i in 1: length(states)){
  jh.april30$median.cases.per.1000[jh.april30$state.abbr == states[i]] <- 
    median(jh.april30$cases.per.1000.residents[jh.april30$state.abbr == states[i]])
}
jh.april30$median.cases.per.1000


#Creating a scatterplot
plot(unique(jh.april30$median.pop.density),median.cases.per.1000,
     xlab = "Median Population Density",
     ylab = "Median Cases per 1000 Residents",
     main = "Median Population Density and 
     COVID-19 Cases per 1000 Residents on April 30",
     pch = 16,
     type = "n")
text(jh.april30$median.pop.density,jh.april30$median.cases.per.1000,
     labels = jh.april30$state.abbr)

#h.Removing the outlier of population density, DC

med.pop.dens.nodc <- jh.april30$median.pop.density[jh.april30$state.abbr != "DC"]
length(unique(med.pop.dens.nodc))

med.case.1000.nodc <- median.cases.per.1000[states != "DC"]
length(med.case.1000.nodc)

#Creating a new scatterplot sans DC
plot(unique(med.pop.dens.nodc),med.case.1000.nodc,
     xlab = "Median Population Density",
     ylab = "Median Cases per 1000 Residents",
     main = "Median Population Density and 
     COVID-19 Cases per 1000 Residents on April 30",
     pch = 16,
     type = "n")
text(jh.april30$median.pop.density,jh.april30$median.cases.per.1000,
     labels = jh.april30$state.abbr)

plot(unique(med.pop.dens.nodc),med.case.1000.nodc,
     xlab = "Median Population Density",
     ylab = "Median Cases per 1000 Residents",
     main = "Median Population Density and 
     COVID-19 Cases per 1000 Residents on April 30",
     pch = 16,
     type = "n",
     xlim = c(0,50),
     ylim = c(0,4))
text(jh.april30$median.pop.density,jh.april30$median.cases.per.1000,
     labels = jh.april30$state.abbr)


#i. Calculating correlation coefficients with a double loop

library(dplyr)

#Getting rid of DC
jh.sort <- jh.reshaped
jh.sort <- jh.sort[jh.sort$state.abbr != "DC",]

#Creating group index variable of states, stateid
jh.sort$stateid <- group_by(jh.sort,jh.sort$state.abbr)
jh.sort$stateid <- group_indices(jh.sort,jh.sort$state.abbr)

#Ordering stateid
jh.sort <- jh.sort[order(jh.sort$stateid),]
unique(jh.sort$stateid)
state.sort <- unique(jh.sort$state.abbr)

#Creating countyid
library(data.table)
jh.sort$countyid <- rowid(jh.sort$stateid)

#Creating a double loop
within.state.cor <- NA

for (j in 1:length(state.sort)) {
  pop.density <- NA
  med.cases <- NA
  for (i in 1:nrow(jh.sort[jh.sort$stateid == j,])){
    
    pop.density[i] <- jh.sort$population.density[which(jh.sort$stateid == j &
                                                         jh.sort$countyid == i)]
    med.cases[i] <- jh.sort$median.cases.per.1000[which(jh.sort$stateid == j &
                                                          jh.sort$countyid == i)]
  }
  within.state.cor[j] <- cor(pop.density,med.cases)
  print(j)
}

within.state.cor

state.cor <- as.data.frame(cbind(state.sort,within.state.cor))
state.cor

state.cor$within.state.cor <- as.numeric(state.cor$within.state.cor)

boxplot(state.cor$within.state.cor ~ state.cor$state.sort,
        xlab="State",
        ylab="Correlation Coefficient",
        main="Distribution of Correlation Coefficients
        for each State")

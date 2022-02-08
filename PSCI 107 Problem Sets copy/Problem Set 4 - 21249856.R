#### PSCI 107
## Problem Set #4
## 21249856
## March 24, 2021

setwd("~/Dropbox/PSCI107")

# Question 1

#a. Simulating 5 game cribbage championship once

sara.win.prob <- c(1,1,1,1,1,1,0,0,0,0)
set.seed(1234)
champ <- sample(sara.win.prob,5,replace = TRUE)
length(which(champ == 1))

#b. Simulating 5 game cribbage championship 10,000 times

set.seed(1234)
sara.win <- rep(NA, 10000)

for(i in 1:length(sara.win)){
  champ <- sample(sara.win.prob,5,replace = TRUE)
  sara.win[i] <- length(which(champ == 1))
}

#Creating a prop.table of simulated championships
sara.prop <- prop.table(table(sara.win))
sara.prop
sum(sara.prop[4:6])

#c. Simulating cribbage championship of length 5 to 151 (by 2) 10,000 times

#What will happen in the loop:
set.seed(1234)
sara.win <- rep(NA, 10000)
for(i in 1:length(sara.win)){
  champ <- sample(sara.win.prob,5,replace = TRUE)
  sara.win[i] <- length(which(champ == 1))
}
length(sara.win[sara.win >= 3])/length(sara.win)

#The double loop:
num.games <- seq(5,151,2)
overall.prop <- rep(NA, 74)

for(j in 1:length(num.games)){
  set.seed(1234)
  sara.win.1 <- rep(NA,10000)
  for(i in 1:length(sara.win.1)){
    champ <- sample(sara.win.prob,num.games[j],replace = TRUE)
    sara.win.1[i] <- length(which(champ == 1))
  }
  overall.prop[j] <- length(sara.win.1[sara.win.1 >= j+2])/length(sara.win.1)
}

overall.prop


#Plotting num.games and overall.prop
plot(num.games,overall.prop,
     xlab = "Number of Games in Championship",
     ylab = "Proportion of Times Sara Wins",
     main = "Proportion of Times Sara Wins the Series
by Championship Length",
     pch = 16,
     col = "darkblue")
abline(h = .95,
       col = "darkred",
       lty = 2)
axis(1, at = seq(0, 151, 10))

# Question 2

load("RawData/Philadelphia daily arrests 2021.Rdata")
dim(arrest21.dta)
#2175 observations of 29 variables

load("RawData/Philadelphia daily bail type 2021.Rdata")
dim(bail21.type.dta)
#2236 observations of 9 variables

#a. Identifying unit of observation in each dataframe

#For both datasets, the unit of observation is the unique combination of
#district and date.

#b. Checking format of each dataframe for compatibility

#First checking class of unique identifier, dc_district
class(arrest21.dta$dc_district)
class(bail21.type.dta$dc_district)
#Changing class for bail21.type.dta$dc_district from character to integer
bail21.type.dta$dc_district <- as.integer(bail21.type.dta$dc_district)
class(bail21.type.dta$dc_district)
#Making sure that both unique identifier columns are the same class  
class(arrest21.dta$dc_district) == class(bail21.type.dta$dc_district)

#Next checking class of the dates
class(arrest21.dta$date_value)
class(bail21.type.dta$date_value)
#Changing class to dates using lubridate
library(lubridate)
arrest21.dta$date_value <- ymd(arrest21.dta$date_value)
class(arrest21.dta$date_value)
bail21.type.dta$date_value <- ymd(bail21.type.dta$date_value)
class(bail21.type.dta$date_value)
#Making sure that both date columns are the same class
class(arrest21.dta$date_value) == class(bail21.type.dta$date_value)

#c. Identifying which observations the dataframes have in common,
##  and which only belong to one or the other

#Identifying which observations in common
length(arrest21.dta$dc_district[arrest21.dta$dc_district %in% bail21.type.dta$dc_district])

both.arrest.bail <- length(bail21.type.dta$dc_district[bail21.type.dta$dc_district
                                                       %in% arrest21.dta$dc_district])


#Identifying how many districts appear in one dataset and not the other
length(unique(arrest21.dta$dc_district))
length(unique(bail21.type.dta$dc_district))

arrest21.dta$dc_district[!arrest21.dta$dc_district %in% 
                                              bail21.type.dta$dc_district]
#all districts in arrest21.dta appear in bail21.type.dta

unique(bail21.type.dta$dc_district[!bail21.type.dta$dc_district %in% 
                                                        arrest21.dta$dc_district])
#district 44 appears in bail21.type.dta but not in arrest21.dta

bail.not.arrest <- length(bail21.type.dta$dc_district[!bail21.type.dta$dc_district %in% 
                              arrest21.dta$dc_district])

#Identifying how many dates appear in one dataset and not the other
length(unique(arrest21.dta$date_value))
length(unique(bail21.type.dta$date_value))

unique(arrest21.dta$date_value[!arrest21.dta$date_value %in% bail21.type.dta$date_value])
#2021-03-28 appears in arrest21.dta but not in bail21.type.dta

arrest.not.bail <- length(arrest21.dta$date_value[!arrest21.dta$date_value 
                                                  %in% bail21.type.dta$date_value])


bail21.type.dta$date_value[!bail21.type.dta$date_value %in% arrest21.dta$date_value]
#all dates in bail21.type.data appear in arrest21.dta

#d. Creating the final merged dataframe

full.merge <- merge(arrest21.dta, bail21.type.dta, 
                    by.x=c("dc_district","date_value"),by.y=c("dc_district","date_value"),
                    all = T)
head(full.merge)
dim(full.merge)
names(full.merge)

#Checking that district 44 and 2021-03-28 are included
unique(full.merge$dc_district)
unique(full.merge$date_value)

#Checking columns
ncol(arrest21.dta) + ncol(bail21.type.dta) - 2
ncol(arrest21.dta) + ncol(bail21.type.dta) - 2 == ncol(full.merge)

#Checking rows
arrest.not.bail + both.arrest.bail + bail.not.arrest
nrow(arrest21.dta) + nrow(bail21.type.dta) - both.arrest.bail == nrow(full.merge)

##Bonus: Do daily drug sales increase as the months get warmer?

#Adding month to full.merge
full.merge$month = NA
#Extracting month as number and saving as 'month.num' in data set
full.merge$month.num = format(full.merge$date_value,"%m")
full.merge$month[full.merge$month.num=="01"] = "Jan"
full.merge$month[full.merge$month.num=="02"] = "Feb"
full.merge$month[full.merge$month.num=="03"] = "Mar"
#Saving 'dc_district' as a factor
full.merge$dc_district = as.factor(full.merge$dc_district)
#Saving the order of month variables so appears in appropriate way in the plot
full.merge$month = factor(full.merge$month, levels = c("Jan", "Feb", "Mar"))

library(ggplot2)
#Turning off warnings (because many districts have sparse data)
options(warn=-1)
#Bar plot with fill = dc_district
bar <- ggplot(full.merge, aes(month, `Drug Sales`, fill = dc_district))
barStats <- bar+stat_summary(fun= mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", 
               position=position_dodge(width=0.90), width=0.2)
barStats + labs(x = "Month", y="Mean Daily Drug Sale Arrests", fill ="Philadelphia District", 
               title="Mean Daily Drug Sale Arrests by Month per District") + 
               theme(plot.title = element_text(hjust = 0.5))

#Bar plot with facet = dc_district for districts 24 and 25
bar <- ggplot(full.merge[full.merge$dc_district == "24" | full.merge$dc_district == "25",], 
              aes(month, `Drug Sales`,fill = dc_district))
barStats <- bar + stat_summary(fun= mean, geom = "bar", position="dodge")+
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", 
               position=position_dodge(width=0.90), width=0.2)+ facet_wrap(~dc_district)
barStats + labs(x = "Month", y="Mean Daily Drug Sale Arrests", fill ="Philadelphia District", 
               title="Mean Daily Drug Sale Arrests by Month per District") + 
               theme(plot.title = element_text(hjust = 0.5)) +
               scale_fill_manual(values = c("#AC88FF","#CF78FF"))
#Turning warnings back on
options(warn=0)

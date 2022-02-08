#### PSCI 107
## Problem Set #2
## 21249856
## March 24, 2021

setwd("~/Dropbox/PSCI107")

load("RawData/US County Temp Pop History - CORRECT.Rdata")

# Question 1

#a. Calculating average temperature out of all years from 1980 to 2019
##  and visualizing the result

#First subsetting data to only 1980 onwards
county.sub <- county.dta[county.dta$year >= 1980,]
head(county.sub)

#Then creating loop
#What will happen in the loop:
mean(county.sub$temp[county.sub$fips == 1001])
mean(county.sub$temp[county.sub$fips == 1003])
mean(county.sub$temp[county.sub$fips == 1005])
#Creating empty variable and vector of unique counties
county.sub$avg.temp <- NA
county <- unique(county.sub$fips)
#The loop:
for(i in 1: length(county)){
  county.sub$avg.temp[county.sub$fips==county[i]] <- 
    mean(county.sub$temp[county.sub$fips == county[i]])
  
}

#Visualizing the average temperatures per county by region
boxplot(county.sub$avg.temp~county.sub$region,
        xlab = "County Region",
        ylab = "Average Temperature (1980-2019) per County",
        main = "Average Temperature per County by Region",
        ylim = c(30,80))

#b. Recording lowest and highest average annual temperature for each state

#What will happen in the loop:
max(county.sub$avg.temp[county.sub$state.abb == "AL"])
min(county.sub$avg.temp[county.sub$state.abb == "AL"])
max(county.sub$avg.temp[county.sub$state.abb == "AK"])
min(county.sub$avg.temp[county.sub$state.abb == "AK"])

#Creating two empty variables: avg.temp.max and avg.temp.min
#and vector of unique states
county.sub$avg.temp.max <- NA
county.sub$avg.temp.min <- NA
states <- unique(county.sub$state.abb)

#Then creating loop:
for(i in 1: length(states)){
  county.sub$avg.temp.max[county.sub$state.abb == states[i]] <-
            max(county.sub$avg.temp[county.sub$state.abb == states[i]])
  county.sub$avg.temp.min[county.sub$state.abb == states[i]] <-
    min(county.sub$avg.temp[county.sub$state.abb == states[i]])
}

#Creating a new variable, avg.temp.range, of max - min
county.sub$avg.temp.range <- NA
county.sub$avg.temp.range <- county.sub$avg.temp.max-county.sub$avg.temp.min

#Finding the most/least consistent average temperatures
#First subsetting the dataset to only state.abb and avg.temp.range
df <- aggregate(avg.temp.range~state.abb, data = county.sub, FUN = mean)
row.names(df) <- df[,1]
#Ordering df from most consistent to least consistent (low to high range)
df <- df[order(df$avg.temp.range),]

#Finding the 5 states with most consistent average temperature
head(df,5)
#Finding the 5 states with least consistent average temperature
tail(df,5)

#Visualizing with a barplot
#Using a function to label each bar with state abbreviation
par(mar = c(7,4,2,2) + 0.2)
rotate_x <- function(data, column_to_plot, labels_vec, rot_angle) {
  plt <- barplot(data[[column_to_plot]], 
                 col='steelblue', 
                 xaxt="n",
                 ylim = c(0,30),
                 xlab = "State",
                 ylab = "Average Temperature Range",
                 main = "Average Temperature Range per State")
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, 
       adj = c(1.1,1.1), xpd = TRUE, cex=0.6) 
}

rotate_x(df,"avg.temp.range",row.names(df),60)

#c. Creating variable that indicates both region and subregion
county.sub$region.subregion <- NA
county.sub$region.subregion <- paste(county.sub$region,county.sub$subregion,
                                     sep = ".")
unique(county.sub$region.subregion)

#d. Investigating correlation between average temperature 
##  and population over time

#What will happen in the loop:
cor(county.sub$temp[county.sub$region.subregion == "South.ESC" & 
                      county.sub$year == 1980],
    county.sub$population[county.sub$region.subregion == "South.ESC" & 
                      county.sub$year == 1980],
    use = "pairwise.complete")
cor(county.sub$temp[county.sub$region.subregion == "South.ESC" & 
                      county.sub$year == 1981],
    county.sub$population[county.sub$region.subregion == "South.ESC" & 
                            county.sub$year == 1981],
    use = "pairwise.complete")
cor(county.sub$temp[county.sub$region.subregion == "West.M" & 
                      county.sub$year == 1980],
    county.sub$population[county.sub$region.subregion == "West.M" & 
                            county.sub$year == 1980],
    use = "pairwise.complete")
cor(county.sub$temp[county.sub$region.subregion == "West.M" & 
                      county.sub$year == 1981],
    county.sub$population[county.sub$region.subregion == "West.M" & 
                            county.sub$year == 1981],
    use = "pairwise.complete")

#Creating the loop
year <- unique(county.sub$year)
subregion <- unique(county.sub$region.subregion)

temp.pop.cor <- matrix(NA, nrow=length(year), ncol=length(subregion))

for(j in 1: length(subregion)){
  for(i in 1: length(year)){
    temp.pop.cor[i,j] <- cor(county.sub$temp[county.sub$region.subregion == subregion[j] & 
                                               county.sub$year == year[i]],
                             county.sub$population[county.sub$region.subregion == subregion[j] & 
                                                     county.sub$year == year[i]],
                             use = "pairwise.complete")
  }
}

#Setting the row names to the years and the column names to the subregion
row.names(temp.pop.cor) <- year
colnames(temp.pop.cor) <- subregion

##Bonus: Visualizing temp.pop.cor
#Saving temp.pop.cor as data frame with subregion names ordered
temp.pop.cor <- as.data.frame(temp.pop.cor)
temp.pop.cor <- temp.pop.cor[,order(names(temp.pop.cor))]

#Creating a line plot with matplot()
library(RColorBrewer) 
subregion.colors <- brewer.pal(9, "Paired")
par(mar=c(5.5, 4.1, 4.1, 8.9), xpd=TRUE)
matplot(year,temp.pop.cor,
        type = "l",
        lty = 1,
        col = subregion.colors,
        xlab = "Year",
        ylab = "Correlations of Temperature and Population",
        main = "Correlations of Temperature and Population
        from 1980 to 2019")
legend("topright", inset=c(-0.34,0), 
       legend=names(temp.pop.cor), 
       lty = 1,
       col = subregion.colors,
       title="Subregion")

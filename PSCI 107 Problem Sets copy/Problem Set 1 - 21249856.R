#### PSCI 107
## Problem Set #1
## 21249856
## February 17, 2021

setwd("~/Dropbox/PSCI107")

# Question 1

#a.Creating 3 vectors: state, pop, and cases
state <- c(1:5)
state

pop <- c(12.80,19.45,3.56,0.97,8.88)
pop

cases <- c(139623,439238,52095,17857,196337)
cases


#b. Combining 3 columns (state, pop, and cases) into a matrix, ne.covid
ne.covid <- cbind(state,pop,cases)
ne.covid

#c. Determining the mean, median, max, and min of pop in ne.covid
mean(ne.covid[,2])
median(ne.covid[,2])
max(ne.covid[,2])
min(ne.covid[,2])

#d. Determining mean, median, max, and min of cases in ne.covid
mean(ne.covid[,3])
median(ne.covid[,3])
max(ne.covid[,3])
min(ne.covid[,3])

#e. Creating a new vector, cases.per.1000, that is the number of cases
#     in each state per 1000 residents.

pop.1000 <- pop*1000
pop.1000

cases.per.1000 <- cases/pop.1000
cases.per.1000

#f. Creating a scatterplot with population on the x-axis,
#      and cases.per.1000 on the y-axis

plot(pop.1000, cases.per.1000,
     main = "COVID-19 Case Rates by State Population",
     xlab = "Population (in Thousands)",
     ylab = "Cases per Thousand",
     pch = 16,
     col = "darkblue")

##Bonus: using the text() function to replace the dots with
#        the state abbreviations
state.names <- c("PA","NY","CT","DE","NJ")
plot(pop.1000, cases.per.1000,
     main = "COVID-19 Case Rates by State Population",
     xlab = "Population (in Thousands)",
     ylab = "Cases per Thousand",
     pch = 16,
     type="n",
     xlim = c(0,25000))
text(pop.1000,cases.per.1000,
     labels = state.names)

#g. Which state been the most affected by COVID-19? The least affected?

# See Word document

#h. One more piece of info about COVID-19 to determine how affected a state is?

# See Word document


# Question 2

#a.Creating 5 vectors: age, gender, hours.daily, pref.network, and much.time
age <- c(19,20,59,47,46,30,41,60,44,33,54,26,35,47,39)

gender <- c("M","F","M","F","M","M","M","F",
            "F","F","F","F","M","M","M")

hours.daily <- c(6,6,0,2,3,3,0,0,1,2,5,5,3,3,0)

pref.network <- c("tiktok","tiktok","none","instagram","twitter","twitter",
                  "none","none","instagram","twitter","facebook","tiktok",
                  "instagram","facebook","none")

much.time <- c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,
               FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)

#b. Creating an object soc.media, which combines these 5 vectors into data frame
#   using cbind.data.frame(), then reporting the class of each variable

soc.media <- cbind.data.frame(age,gender,hours.daily,pref.network,much.time)

class(soc.media[,1])
class(soc.media[,2])
class(soc.media[,3])
class(soc.media[,4])
class(soc.media[,5])

#c. How many men spend 3 or more hours per day on social media? 
#   Preferred networks of these men?

dim(soc.media[soc.media[,2] == "M"
          & soc.media[,3] >= 3,])

soc.media[soc.media[,2] == "M"
          & soc.media[,3] >= 3,]

# There are 5 men who spend 3+ hours on social media, 
# and their preferred networks are tiktok, twitter, twitter, instagram,
# and facebook, respectively.

#d. How many women over 40 say their favorite network is facebook or instagram?

dim(soc.media[soc.media[,1] > 40
              & soc.media[,2] == "F"
              & (soc.media[,4] == "facebook" | soc.media[,4] == "instagram"),])

soc.media[soc.media[,1] > 40
          & soc.media[,2] == "F"
          & (soc.media[,4] == "facebook" | soc.media[,4] == "instagram"),]

# There are 3 women over 40 whose preferred network is either facebook 
# or instagram.

#e. Creating an object soc.media.much that subsets the data to include only
#   individuals who spend too much time on social media (much.time == T).
#   How many people in this subset?

soc.media.much <- soc.media[soc.media[,5] == T,]
soc.media.much
dim(soc.media.much)

# There are 7 people in this subset. 

#f. Creating a scatter plot of ages by daily social media use in soc.media.much

plot(soc.media.much[,3], soc.media.much[,1],
     main = "Age by Daily Social Media Use",
     xlab = "Daily Social Media Use (Hours)",
     ylab = "Age",
     pch = 16,
     col = "darkblue")


#g. Creating a scatter plot of ages by daily social media use in soc.media

plot(soc.media[,3], soc.media[,1],
     main = "Age by Daily Social Media Use",
     xlab = "Daily Social Media Use (Hours)",
     ylab = "Age",
     pch = 16,
     col = "red")

#h. Looking at the plots created for g and h:

# See Word document

##Bonus: Generating one plot that visualizes the two relationships and 
#        highlights the differences


plot(soc.media[,3], soc.media[,1], type="n",
     xlab="Daily Social Media Use (Hours)",
     ylab="Age",
     main="Age by Daily Social Media Use")
points(soc.media[soc.media[,5] == TRUE,3],
       soc.media[soc.media[,5] == TRUE,1],
       col="darkblue",
       pch=16)
points(soc.media[soc.media[,5] == FALSE,3],
       soc.media[soc.media[,5] == FALSE,1],
       col="red",
       pch=16)

legend("topright", c("Too Much Time", "Not Too Much Time"),
       pch=c(16,16),
       col=c("darkblue","red"))

# Question 3

# See Word document





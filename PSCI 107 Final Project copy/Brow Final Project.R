#### PSCI 107
## Final Project
## Rachel Brow 21249856
## May 4, 2021

setwd("~/Dropbox/PSCI107")

library(rio)
anes2020 <- import("RawData/anes_timeseries_2020_stata_20210324.dta")
View(anes2020)

anes2020.untouched <- anes2020

#Feeling thermometer: Asians
table(anes2020$V202478)
length(anes2020$V202478[anes2020$V202478 == -9 | anes2020$V202478 == -7 | 
                          anes2020$V202478 == -6 | anes2020$V202478 == -5 |
                          anes2020$V202478 == -1])

#Approve/disapprove Trump handling COVID-19
attributes(anes2020$V201144x)
table(anes2020$V201144x)

library(tidyverse)
anes2020 <- rename(anes2020,
                   pres.covid = V201144x,
                   asian.feeling = V202478)

table(anes2020$pres.covid)

table(anes2020$asian.feeling)

#Recoding asian.feeling to exclude missing values
length(anes2020$asian.feeling[anes2020$asian.feeling == -9 | 
                              anes2020$asian.feeling == -7 | 
                              anes2020$asian.feeling == -6 | 
                              anes2020$asian.feeling == -5 |
                              anes2020$asian.feeling == -1])
anes2020$asian.feeling[anes2020$asian.feeling == -9 | 
                         anes2020$asian.feeling == -7 | 
                         anes2020$asian.feeling == -6 | 
                         anes2020$asian.feeling == -5 |
                         anes2020$asian.feeling == -1] <- NA
table(anes2020$asian.feeling,useNA = "always")

#Recoding pres.covid to exclude missing values
length(anes2020$pres.covid[anes2020$pres.covid == -2])
anes2020$pres.covid[anes2020$pres.covid == -2] <- NA
table(anes2020$pres.covid,useNA = "always")

#Recoding pres.covid to a more intuitive scale:
##1 being "Strongly Disapprove" and 4 being "Strongly Approve"
anes2020$pres.covid.num <- NA
anes2020$pres.covid.num[anes2020$pres.covid == 1] <- 4
anes2020$pres.covid.num[anes2020$pres.covid == 2] <- 3
anes2020$pres.covid.num[anes2020$pres.covid == 3] <- 2
anes2020$pres.covid.num[anes2020$pres.covid == 4] <- 1

#Recoding pres.covid to meaningful labels
attributes(anes2020$pres.covid)
anes2020$pres.covid.names <- NA
anes2020$pres.covid.names[anes2020$pres.covid == 1] <- "Strongly Approve"
anes2020$pres.covid.names[anes2020$pres.covid == 2] <- "Approve"
anes2020$pres.covid.names[anes2020$pres.covid == 3] <- "Disapprove"
anes2020$pres.covid.names[anes2020$pres.covid == 4] <- "Strongly Disapprove"

anes2020$pres.covid.names = factor(anes2020$pres.covid.names, 
                                   levels = c("Strongly Disapprove", "Disapprove", 
                                              "Approve","Strongly Approve"))

#Recoding partyid to excluding missing/don't know and Independents
anes2020 <- rename(anes2020,
                   partyid = V201231x)
table(anes2020$partyid)
length(anes2020$partyid[anes2020$partyid == -9 | 
                        anes2020$partyid == -8 | 
                        anes2020$partyid == 4])
anes2020$partyid[anes2020$partyid == -9 | 
                        anes2020$partyid == -8 | 
                        anes2020$partyid == 4] <- NA
table(anes2020$partyid,useNA = "always")

#Recoding partyid to Democrat/Republican
anes2020$partyid.dr <- NA
anes2020$partyid.dr[anes2020$partyid == 1 | 
                   anes2020$partyid == 2 | 
                   anes2020$partyid == 3] <- "Democrat"
anes2020$partyid.dr[anes2020$partyid == 5 | 
                   anes2020$partyid == 6 | 
                   anes2020$partyid == 7] <- "Republican"

#Creating dummy variable of partyid.dr
anes2020$partyid.dr.rep <- NA
anes2020$partyid.dr.rep[anes2020$partyid.dr == "Democrat"] <- 0
anes2020$partyid.dr.rep[anes2020$partyid.dr == "Republican"] <- 1

#Respondent's race:
attributes(anes2020$V201549x)
table(anes2020$V201549x)
anes2020 <- rename(anes2020,
                   race = V201549x)
table(anes2020$race)

#Recoding race to exclude missing/don't know
length(anes2020$race[anes2020$race == -9 | 
                          anes2020$race == -8])
anes2020$race[anes2020$race == -9 | anes2020$race == -8] <- NA
table(anes2020$race,useNA = "always")

#Recoding race to race name
anes2020$race.names <- NA
anes2020$race.names[anes2020$race == 1] <- "White"
anes2020$race.names[anes2020$race == 2] <- "Black"
anes2020$race.names[anes2020$race == 3] <- "Hispanic"
anes2020$race.names[anes2020$race == 4] <- "Asian"
anes2020$race.names[anes2020$race == 5 | anes2020$race == 6] <- "Other"
table(anes2020$race.names)

anes2020$race.names = factor(anes2020$race.names, 
                                   levels = c("White", "Black", 
                                              "Hispanic","Asian",
                                              "Other"))
#Descriptive statistics
summary(anes2020$asian.feeling[anes2020$pres.covid.names == "Strongly Disapprove"])
summary(anes2020$asian.feeling[anes2020$pres.covid.names == "Disapprove"])
summary(anes2020$asian.feeling[anes2020$pres.covid.names == "Approve"])
summary(anes2020$asian.feeling[anes2020$pres.covid.names == "Strongly Approve"])

library(ggplot2)
#Boxplot
box <- ggplot(data = subset(anes2020,!is.na(pres.covid.names)),
              aes(x=pres.covid.names,y=asian.feeling,fill=pres.covid.names)) + 
  geom_boxplot()
box + labs(x = "Response to President Handling COVID-19",
           y="Feeling towards Asians (0-100)",
           fill = "Response",
           title="Feeling Thermometer Towards Asians",
           subtitle = "by Response to Trump's Handling of COVID-19") + 
  theme(legend.position = "none")


#Bivariate Regression: numbered pres.covid
summary(lm(asian.feeling~pres.covid.num,data = anes2020))
#Bivariate Regression: factored pres.covid
summary(lm(asian.feeling~pres.covid.names,data = anes2020))
#Multivariate Regression with Controls
summary(lm(asian.feeling~pres.covid.num + race.names + partyid.dr.rep,data = anes2020))


#Saving means of asian.feeling by pres.covid.names into a vector
mean.feeling <- NA
mean.feeling[1] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Strongly Disapprove"],na.rm = T)
mean.feeling[2] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names
                             == "Disapprove"],na.rm = T)
mean.feeling[3] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names
                             == "Approve"],na.rm = T)
mean.feeling[4] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names
                             == "Strongly Approve"],na.rm = T)
mean.feeling

#Simple Barplot
bar <- ggplot(data = subset(anes2020,!is.na(pres.covid.names)),
              aes(x= pres.covid.names, y= asian.feeling,fill = pres.covid.names))
barStats <- bar+stat_summary(fun= mean, geom = "bar", position="dodge")
bar.plus <- barStats + labs(x = "Response to President Handling COVID-19",
                            y="Mean Feeling towards Asians (0-100)",
                            fill = "Response",
                            title="Feeling Thermometer Towards Asians",
                            subtitle = "by Response to Trump's Handling of COVID-19") +
  theme(plot.title = element_text(hjust = 0),
        legend.position = "none") +
  coord_cartesian(ylim = c(0,80))
bar.plus + annotate(geom="text", x=1, y=69, label=round(mean.feeling[1],2)) +
           annotate(geom="text", x=2, y=64, label=round(mean.feeling[2],2)) +
           annotate(geom="text", x=3, y=63, label=round(mean.feeling[3],2)) +
           annotate(geom="text", x=4, y=64, label=round(mean.feeling[4],2))


#Saving means of asian.feeling by pres.covid and partyid into a vector
mean.feeling.2 <- NA
mean.feeling.2[1] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Strongly Disapprove" & 
                               anes2020$partyid.dr == "Democrat"],na.rm = T)
mean.feeling.2[2] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Strongly Disapprove" & 
                              anes2020$partyid.dr == "Republican"],na.rm = T)
mean.feeling.2[3] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Disapprove" & 
                              anes2020$partyid.dr == "Democrat"],na.rm = T)
mean.feeling.2[4] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Disapprove" & 
                              anes2020$partyid.dr == "Republican"],na.rm = T)
mean.feeling.2[5] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Approve" & 
                              anes2020$partyid.dr == "Democrat"],na.rm = T)
mean.feeling.2[6] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Approve" & 
                              anes2020$partyid.dr == "Republican"],na.rm = T)
mean.feeling.2[7] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Strongly Approve" & 
                              anes2020$partyid.dr == "Democrat"],na.rm = T)
mean.feeling.2[8] <- mean(anes2020$asian.feeling[anes2020$pres.covid.names 
                             == "Strongly Approve" & 
                              anes2020$partyid.dr == "Republican"],na.rm = T)
mean.feeling.2


#Barplot: Fill by Race
bar <- ggplot(data = subset(anes2020,!is.na(pres.covid.names) & !is.na(race.names)), 
              aes(pres.covid.names, asian.feeling,fill = race.names))
barStats <- bar+stat_summary(fun= mean, geom = "bar", position="dodge", colour = "white")
bar.plus <- barStats + labs(x = "Response to President Handling COVID-19", 
                            y="Mean Feeling towards Asians (0-100)",
                            fill = "Race",
                            title="Feeling Thermometer Towards Asians",
                            subtitle = "by Response to Trump's Handling of COVID-19, by Race") + 
  theme(plot.title = element_text(hjust = 0)) +
  coord_cartesian(ylim = c(0,80))
bar.plus
#Barplot: Facet by Race
bar <- ggplot(data = subset(anes2020,!is.na(pres.covid.names) & !is.na(race.names)), 
              aes(pres.covid.names, asian.feeling,fill = pres.covid.names))
barStats <- bar+stat_summary(fun= mean, geom = "bar", position="dodge")
bar.plus <- barStats + labs(x = "Response to President Handling COVID-19", 
                            y="Mean Feeling towards Asians (0-100)",
                            fill = "Race",
                            title="Feeling Thermometer Towards Asians",
                            subtitle = "by Response to Trump's Handling of COVID-19, per Race") + 
  theme(plot.title = element_text(hjust = 0)) +
  coord_cartesian(ylim = c(0,80))
bar.plus + facet_wrap(~race.names) + theme(axis.text.x = element_blank()) +
    theme(legend.position = "bottom") + labs(fill = "Response")

#White respondent who strongly disapproves
mean(anes2020$asian.feeling[anes2020$pres.covid.names == "Strongly Disapprove" &
                              anes2020$race.names == "White"],na.rm = T)
#White respondent who strongly approves
mean(anes2020$asian.feeling[anes2020$pres.covid.names == "Strongly Approve" &
                              anes2020$race.names == "White"],na.rm = T)

#Black respondent who strongly disapproves
mean(anes2020$asian.feeling[anes2020$pres.covid.names == "Strongly Disapprove" &
                              anes2020$race.names == "Black"],na.rm = T)
#Black respondent who strongly approves
mean(anes2020$asian.feeling[anes2020$pres.covid.names == "Strongly Approve" &
                              anes2020$race.names == "Black"],na.rm = T)
#Black respondent who approves
mean(anes2020$asian.feeling[anes2020$pres.covid.names == "Approve" &
                              anes2020$race.names == "Black"],na.rm = T)


#Barplot: Fill by partyid
bar <- ggplot(data = subset(anes2020,!is.na(pres.covid.names) & !is.na(partyid.dr)), 
              aes(pres.covid.names, asian.feeling,fill = partyid.dr))
barStats <- bar+stat_summary(fun= mean, geom = "bar", position="dodge")
bar.plus <- barStats + labs(x = "Response to President Handling COVID-19", 
                            y="Mean Feeling towards Asians (0-100)",
                            fill = "Party ID",
                            title="Feeling Thermometer Towards Asians",
                            subtitle = "by Response to Trump's Handling of COVID-19, for Democrats and Republicans") + 
  theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = c("#00BBDB","#F8766D")) +
  coord_cartesian(ylim = c(0,80))
bar.plus + annotate(geom="text", x=.78, y=70, label=round(mean.feeling.2[1],2)) +
  annotate(geom="text", x=1.22, y=65, label=round(mean.feeling.2[2],2)) +
  annotate(geom="text", x=1.78, y=65.6, label=round(mean.feeling.2[3],2)) +
  annotate(geom="text", x=2.22, y=64, label=round(mean.feeling.2[4],2)) +
  annotate(geom="text", x=2.78, y=59.72, label=round(mean.feeling.2[5],2)) +
  annotate(geom="text", x=3.22, y=63.95, label=round(mean.feeling.2[6],2)) +
  annotate(geom="text", x=3.78, y=61.43, label=round(mean.feeling.2[7],2)) +
  annotate(geom="text", x=4.22, y=64.59, label=round(mean.feeling.2[8],2))
#
#
#
#
#Discrimination against Asians
table(anes2020$V202529)
attributes(anes2020$V202529)
anes2020$V202529[anes2020$V202529 == -9 |
                   anes2020$V202529 == -7 |
                   anes2020$V202529 == -6 |
                   anes2020$V202529 == -5] <- NA
table(anes2020$V202529,useNA = "always")
#Renaming discrimination against Asians
anes2020 <- rename(anes2020,
                   asian.discrim = V202529)

#Recoding asian.discrim to a more intuitive scale:
## 0 being "Not at all" and 4 being "A great deal"
anes2020$asian.discrim.num <- NA
anes2020$asian.discrim.num[anes2020$asian.discrim == 1] <- 4
anes2020$asian.discrim.num[anes2020$asian.discrim == 2] <- 3
anes2020$asian.discrim.num[anes2020$asian.discrim == 3] <- 2
anes2020$asian.discrim.num[anes2020$asian.discrim == 4] <- 1
anes2020$asian.discrim.num[anes2020$asian.discrim == 5] <- 0

#Descriptive statistics
summary(anes2020$asian.discrim.num[anes2020$pres.covid.names == "Strongly Disapprove"])
summary(anes2020$asian.discrim.num[anes2020$pres.covid.names == "Disapprove"])
summary(anes2020$asian.discrim.num[anes2020$pres.covid.names == "Approve"])
summary(anes2020$asian.discrim.num[anes2020$pres.covid.names == "Strongly Approve"])

#Bivariate regression: numbered pres.covid
summary(lm(asian.discrim.num~pres.covid.num,data = anes2020))
#Bivariate regression: factored pres.covid
summary(lm(asian.discrim.num~pres.covid.names,data = anes2020))
#Multivariate regression: numbered pres.covid
summary(lm(asian.discrim.num~pres.covid.num + race.names + partyid.dr.rep,data = anes2020))


#Saving means of asian.discrim.num by pres.covid.names into a vector
mean.discrim <- NA
mean.discrim[1] <- mean(anes2020$asian.discrim.num[anes2020$pres.covid.names 
                                               == "Strongly Disapprove"],na.rm = T)
mean.discrim[2] <- mean(anes2020$asian.discrim.num[anes2020$pres.covid.names
                                               == "Disapprove"],na.rm = T)
mean.discrim[3] <- mean(anes2020$asian.discrim.num[anes2020$pres.covid.names
                                               == "Approve"],na.rm = T)
mean.discrim[4] <- mean(anes2020$asian.discrim.num[anes2020$pres.covid.names
                                               == "Strongly Approve"],na.rm = T)
mean.discrim

#Barplot: asian.discrim.num by pres.covid
bar <- ggplot(data = subset(anes2020,!is.na(pres.covid.names)),
              aes(x= pres.covid.names, y= asian.discrim.num,fill = pres.covid.num))
barStats <- bar+stat_summary(fun= mean, geom = "bar", position="dodge")
bar.plus <- barStats + labs(x = "Response to President Handling COVID-19",
                            y="Mean Perception of Discrimination Against Asians (0-4)",
                            title="Perception of Discrimination Against Asians",
                            subtitle = "by Response to Trump's Handling of COVID-19") +
  theme(plot.title = element_text(hjust = 0),
        legend.position = "none") +
  coord_cartesian(ylim = c(0,2))
bar.plus + annotate(geom="text",colour = "white", x=1, y=1.8, label=round(mean.discrim[1],2)) +
  annotate(geom="text", colour = "white", x=2, y=1.43, label=round(mean.discrim[2],2)) +
  annotate(geom="text", colour = "white", x=3, y=1.31, label=round(mean.discrim[3],2)) +
  annotate(geom="text", colour = "white", x=4, y=1.26, label=round(mean.discrim[4],2))



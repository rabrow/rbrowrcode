#### PSCI 107
## Problem Set #5
## 21249856
## April 28, 2021

setwd("~/Dropbox/PSCI107")

load("RawData/ANESData.Rdata")

#Feeling thermometer towards Donald Trump

attributes(anes$V162079)
table(anes$V162079)
#Renaming V162079 to trump.feeling
library(tidyverse)
anes <- rename(anes,
               trump.feeling = V162079)
table(anes$trump.feeling)
#Recoding trump.feeling to exclude missing/don't know
anes$trump.feeling[anes$trump.feeling == -9 | 
                     anes$trump.feeling == -6 |
                     anes$trump.feeling == 998] <- NA
table(anes$trump.feeling,useNA = "always")

#Using the independent variable political ideology, V161126
attributes(anes$V161126)
table(anes$V161126)
#Renaming V161126 to pol.ideology
anes <- rename(anes,
               pol.ideology = V161126)
table(anes$pol.ideology)
#Recoding pol.ideology to exclude missing/don't know
anes$pol.ideology[anes$pol.ideology == -9 |
                    anes$pol.ideology == -8 |
                    anes$pol.ideology == 99] <- NA
table(anes$pol.ideology,useNA = "always")


#Bivariate regression: trump.feeling by pol.ideology
summary(lm(trump.feeling~pol.ideology,data = anes))


#For a meaningful intercept: 
##Recoding pol.ideology to start at 0
anes$pol.ideology.num <- NA
anes$pol.ideology.num[anes$pol.ideology == 1] <- 0
anes$pol.ideology.num[anes$pol.ideology == 2] <- 1
anes$pol.ideology.num[anes$pol.ideology == 3] <- 2
anes$pol.ideology.num[anes$pol.ideology == 4] <- 3
anes$pol.ideology.num[anes$pol.ideology == 5] <- 4
anes$pol.ideology.num[anes$pol.ideology == 6] <- 5
anes$pol.ideology.num[anes$pol.ideology == 7] <- 6


##Bivariate regression: trump.feeling by pol.ideology.num
summary(lm(trump.feeling~pol.ideology.num,data = anes))


##Exporting a table
library(stargazer)
reg1 <- lm(trump.feeling~pol.ideology.num,data = anes)
stargazer(reg1,type = "html",style = "apsr",title = "Bivariate Regression",
          dep.var.labels = "Feeling towards Trump (0-100)",
          covariate.labels = c("Political Ideology","Constant"),
          out = "bivariate.html")


#For a variable with ideology names:
##Recoding pol.ideology from "Extremely Liberal" to "Extremely Conservative"
anes$pol.ideology.names <- NA
anes$pol.ideology.names[anes$pol.ideology == 1] <- "Extremely Liberal"
anes$pol.ideology.names[anes$pol.ideology == 2] <- "Liberal"
anes$pol.ideology.names[anes$pol.ideology == 3] <- "Slightly Liberal"
anes$pol.ideology.names[anes$pol.ideology == 4] <- "Moderate"
anes$pol.ideology.names[anes$pol.ideology == 5] <- "Slightly Conservative"
anes$pol.ideology.names[anes$pol.ideology == 6] <- "Conservative"
anes$pol.ideology.names[anes$pol.ideology == 7] <- "Extremely Conservative"
table(anes$pol.ideology.names)
anes$pol.ideology.names = factor(anes$pol.ideology.names, 
                                   levels = c("Extremely Liberal", "Liberal", 
                                              "Slightly Liberal","Moderate",
                                              "Slightly Conservative","Conservative",
                                              "Extremely Conservative"))

#Creating a bar graph of mean trump.feeling by pol.ideology

#First saving means to label bars
mean.feeling <- NA
mean.feeling[1] <- mean(anes$trump.feeling[anes$pol.ideology.names 
                                           == "Extremely Liberal"],na.rm = T)
mean.feeling[2] <- mean(anes$trump.feeling[anes$pol.ideology.names 
                                           == "Liberal"],na.rm = T)
mean.feeling[3] <- mean(anes$trump.feeling[anes$pol.ideology.names 
                                           == "Slightly Liberal"],na.rm = T)
mean.feeling[4] <- mean(anes$trump.feeling[anes$pol.ideology.names 
                                           == "Moderate"],na.rm = T)
mean.feeling[5] <- mean(anes$trump.feeling[anes$pol.ideology.names 
                                           == "Slightly Conservative"],na.rm = T)
mean.feeling[6] <- mean(anes$trump.feeling[anes$pol.ideology.names 
                                           == "Conservative"],na.rm = T)
mean.feeling[7] <- mean(anes$trump.feeling[anes$pol.ideology.names 
                                           == "Extremely Conservative"],na.rm = T)
#Barplot
library(ggplot2)
bar <- ggplot(data = subset(anes,!is.na(pol.ideology)),
              aes(x= pol.ideology.names, y= trump.feeling,fill = pol.ideology))
barStats <- bar+stat_summary(fun= mean, geom = "bar", position="dodge") +
  stat_summary(fun.data = mean_cl_normal, geom="errorbar",
               position=position_dodge(width=0.90), width=0.2)
bar.plus <- barStats + labs(x = "Political Ideology",
                            y="Mean Feeling towards Trump (0-100)",
                            fill = "Ideology",
                            title="Feeling Thermometer Towards Trump",
                            subtitle = "by Political Ideology") +
  theme(plot.title = element_text(hjust = 0),
        legend.position = "none") +
  coord_cartesian(ylim = c(0,100))
bar.plus + annotate(geom="text", x=1, y=21, label=round(mean.feeling[1],2)) +
  annotate(geom="text", x=2, y=22, label=round(mean.feeling[2],2)) +
  annotate(geom="text", x=3, y=35, label=round(mean.feeling[3],2)) +
  annotate(geom="text", x=4, y=51, label=round(mean.feeling[4],2)) +
  annotate(geom="text", x=5, y=62, label=round(mean.feeling[5],2)) +
  annotate(geom="text", x=6, y=77, label=round(mean.feeling[6],2)) +
  annotate(geom="text", x=7, y=85, label=round(mean.feeling[7],2))


#Controlling for other variables

#Gender of respondent: V161002
attributes(anes$V161002)
table(anes$V161002)
#Renaming V161002 to gender
anes <- rename(anes,
               gender = V161002)
table(anes$gender)
#Recoding gender to a dummy variable
anes$male <- NA
anes$male[anes$gender == 1] <- 1
anes$male[anes$gender == 2] <- 0
table(anes$male)

#Are things in the country on the right track: V161081
attributes(anes$V161081)
table(anes$V161081)
#Renaming V161002 to right.track
anes <- rename(anes,
               right.track = V161081)
table(anes$right.track)
#For regression: Recoding right.track to a dummy variable,wrong.track, 
##and excluding missing/don't know
anes$right.track[anes$right.track == -9 |
                   anes$right.track == -8] <- NA
anes$wrong.track <- NA
anes$wrong.track[anes$right.track == 1] <- 0
anes$wrong.track[anes$right.track == 2] <- 1
table(anes$wrong.track,useNA = "always")

#President Obama approval: V161082
attributes(anes$V161082)
table(anes$V161082)
#Renaming V161082 to pres.approval
anes <- rename(anes,
               pres.approval = V161082)
table(anes$pres.approval)
#For regression: Recoding pres.approval to a dummy variable,pres.disapproval,
##and excluding missing/don't know
anes$pres.approval[anes$pres.approval == -9 |
                     anes$pres.approval == -8] <- NA
anes$pres.disapproval <- NA
anes$pres.disapproval[anes$pres.approval == 1] <- 0
anes$pres.disapproval[anes$pres.approval == 2] <- 1
table(anes$pres.disapproval)


#Multivariate regression with controls
summary(lm(trump.feeling~pol.ideology.num + male + 
             wrong.track + pres.disapproval,data = anes))

##Exporting a table
reg2 <- lm(trump.feeling~pol.ideology.num + male + 
             wrong.track + pres.disapproval,data = anes)
stargazer(reg1,reg2,type = "html",style = "apsr",title = "Bivariate and Multivariate Regressions",
          dep.var.labels = "Feeling towards Trump (0-100)",
          covariate.labels = c("Political Ideology","Male",
                               "Country on Wrong Track","President Disapproval","Constant"),
          out = "multivariate.html")


#Determining substantive significance
m <- lm(trump.feeling~pol.ideology.num + male + 
          wrong.track + pres.disapproval,data = anes)
summary(m)
#Finding sd for each variable using a loop
vars <- c("pol.ideology.num","male","wrong.track","pres.disapproval")
sds <- rep(NA, length(vars))
for(i in 1:length(vars)){
  sds[i] <- sd(anes[,vars[i]],na.rm = T)
}
#Using coef() to pull coefficients from regression
c <- coef(m)[vars]
#Standardized impacts
c*sds




#install.packages("tidyverse")
library(tidyverse)

dataset <- read_csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv")

#---- Question 1 ----
reg1 <- lm(voteshare ~ difflog, data = dataset)
summary(reg1)

plot1 <- ggplot(dataset, aes(x = difflog, y = voteshare)) + geom_point() + geom_smooth(method = "lm") +
  labs(title = "Impact of campaign spending on vote share", x = "Difference in campaign spending", y = "Incumbent vote share")
plot1

resid1 <- reg1$residuals

#prediction equation: y = 0.579 + 0.042x + e


#---- Question 2 ----
reg2 <- lm(presvote ~ difflog, data = dataset)
summary(reg2)

plot2 <- ggplot(dataset, aes(x = difflog, y = presvote)) + geom_point() + geom_smooth(method = "lm") +
  labs(title = "Impact of campaign spending on incumbent party candidate vote share", x = "Difference in campaign spending", y = "Incumbent party candidate vote share")
plot2

resid2 <- reg2$residuals

#prediction equation: y = 0.508 + 0.024x + e


#---- Question 3 ----
reg3 <- lm(voteshare ~ presvote, data = dataset)
summary(reg3)

plot3 <- ggplot(dataset, aes(x = presvote, y = voteshare)) + geom_point() + geom_smooth(method = "lm") +
  labs(title = "Incumbent Party vote share and electoral success of incumbent party", x = "Vote share of incumbent party for President",
       y = "Incumbent candidate vote shares")
plot3

#prediction equation: y = 0.441 + 0.388x + e


#---- Question 4 ----
reg4 <- lm(resid1 ~ resid2)
summary(reg4)

plot4 <- ggplot(aes(x = resid2, y = resid1), data = NULL) + geom_point() + geom_smooth(method = "lm") +
  labs(title = "Residuals of the first two regressions", x = "Residuals from Question 2", 
          y = "Residuals from Question 3")
plot4


#---- Question 5 ----
reg5 <- lm(voteshare ~ difflog + presvote, dataset)
summary(reg5)

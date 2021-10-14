#install.packages("tidyverse")

library(tidyverse)

#---- Question 1: Political Science ----

#entering the data manually
data <- matrix(c(14, 7, 6, 7, 7, 1), nrow = 2)
print(data)

#Chi-squared statistic = sum of (observed frequency - expected frequency)^2/expected frequency. Write function to do it by hand:
#expected frequency = row total/grand total * column total

chi.stat <- function(freqtab){
  total <- sum(colSums(freqtab))
  chisq <- 0
  for(r in 1:nrow(freqtab)){
    for(c in 1:ncol(freqtab)){
      fobs <- freqtab[r,c]
      fexp <- (sum(freqtab[r,])/total) * sum(freqtab[,c])
      c <- (fobs-fexp)^2/fexp
      chisq <- chisq + c
    }
  }
  return(chisq)
}

chi.stat(data)

#b) calculate p-value
#Degrees of freedom = rows-1*cols-1 = 2-1 * 3-1 = 2

pchisq(chi.stat(data), df=2, lower.tail = F)

#p = 0.15 suggests no association if alpha = 0.1

#c) Standardised residuals

cstest <- chisq.test(data)
cstest$residuals


#---- Question 2: Economics ----

women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
summary(women)

#More women complain about water = water facilities are "pro woman"
#HA: mu water(reserved = 1) =/= mu water(reserved = 0)
#H0: mu water(reserved = 1) = mu water(reserved = 0)

#run a bivariate regression (why tho)
reg <- lm(water ~ reserved, data = women)
summary(reg)

#coefficient of 9.25 = on average a woman-reserved GP will have 9.25 more water facilities.
#p = 0.01 ==> significant correlation on alpha = 0.05
#We can reasonably reject the null hypothesis

#---- Question 3: Biology ----
fruitfly <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/fruitfly.csv")
summary(fruitfly)

lifespanplot <- ggplot(fruitfly) + geom_bar(aes(x = lifespan)) + labs(x = "Lifespan in days", y = "", title = "Lifespan of fruitflies")
lifespanplot

#Part 2: Lifespan vs thorax length

thoraxplot <- ggplot(fruitfly) + geom_point(aes(x = thorax, y = lifespan)) + 
  geom_smooth(aes(x = thorax, y = lifespan), method = "lm") +
  labs(x = "Thorax length in mm", y = "Lifespan in days", title = "Regression of lifespans on thorax lengths")
thoraxplot

r <- cor(fruitfly$thorax, fruitfly$lifespan)

#Part 3: Regress thorax on lifespan

thoraxreg <- lm(lifespan ~ thorax, data = fruitfly)
summary(thoraxreg)

#4: test for a linear relationship

#we have an estimation of r above: 0.636 (saved in variable r); n = 125

#test statistic:
t_stat <- (r*sqrt(123))/(sqrt(1-r^2))
#p-value:
pt(t_stat, 123, lower.tail = F)

#5: Confidence interval
margin.error <- qt(p = 0.1/2, df = 123, lower.tail = F)
upperbound <- thoraxreg$coefficients["thorax"] + (margin.error*15.77)
lowerbound <- thoraxreg$coefficients["thorax"] - (margin.error*15.77)

confint(thoraxreg, level = 0.9)

#6: Prediction

predframe <- data.frame(thorax = c(0.8))
predict(thoraxreg, newdata = predframe, interval = "prediction")
predict(thoraxreg, newdata = predframe, interval = "confidence")

#7: Plot

predframe_2 <- data.frame(thorax = seq(0.64, 0.94, 0.025))
confint_frame <- data.frame(predict(thoraxreg, newdata = predframe_2, interval = "confidence"))
predint_frame <- data.frame(predict(thoraxreg, newdata = predframe_2, interval = "prediction"))

predplot <- ggplot() + geom_line(aes(x = predframe_2$thorax, y = confint_frame$fit), data = NULL) +  
  geom_line(aes(x = predframe_2$thorax, y = confint_frame$lwr), col = "blue", linetype = "dashed", data = NULL) +
  geom_line(aes(x = predframe_2$thorax, y = confint_frame$upr), col = "blue", linetype = "dashed", data = NULL) +
  geom_line(aes(x = predframe_2$thorax, y = predint_frame$lwr), col = "blue", linetype = "dotted", data = NULL) +
  geom_line(aes(x = predframe_2$thorax, y = predint_frame$upr), col = "blue", linetype = "dotted", data = NULL) +
  labs(x = "Thorax length in mm", y = "Predicted lifespan in days", title = "Predicted lifespan and confidence intervals")
predplot


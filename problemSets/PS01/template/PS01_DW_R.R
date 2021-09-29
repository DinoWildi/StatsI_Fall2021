#Package installation if required (commented out since they only need to be installed once, and I have them installed already)
#install.packages("cowplot")
#install.packages("tidyverse")

library(tidyverse)
library(cowplot)

# ---- Section 1: Education ----

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# 1. Find a 90% confidence interval for the average student IQ in the school.
# As we have a sample size of only 25 a t-distribution appears more appropriate than a z-score:

tscore <- qt(p=0.1/2, df = 24, lower.tail = F)
margin <- tscore * (sd(y)/sqrt(25))
lower_ci <- mean(y) - margin
upper_ci <- mean(y) + margin

# 2. Next, the school counselor was curious whether the average student IQ in her school is higher than the average IQ score (100) among all the schools in the country.
# testing a mean = t-test; one-sided (H1: mu > 100)

t.test(y, mu = 100, alternative = "greater")

#We cannot reject H0; chances are that the average student IQ in the counselor's school is not higher than all the schools in the country.


# ---- Section 2: Political Economy ----

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

# Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations

y.x1 <- ggplot(data = expenditure,aes(x = X1, y = Y)) + geom_point() + geom_smooth(method = "lm") +
 xlab("Personal income per capita") + ylab("Expenditure on shelters/housing assistance per capita")
y.x2 <- ggplot(data = expenditure,aes(x = X2, y = Y)) + geom_point() + geom_smooth(method = "lm") +
  xlab("Financially insecure people per 100'000") + ylab("Expenditure on shelters/housing assistance per capita")
y.x3 <- ggplot(data = expenditure,aes(x = X3, y = Y)) + geom_point() + geom_smooth(method = "lm") +
  xlab("People residing in urban areas per 100'000") + ylab("Expenditure on shelters/housing assistance per capita")
x1.x2 <- ggplot(data = expenditure,aes(x = X1, y = X2)) + geom_point() + geom_smooth(method = "lm") +
  xlab("Personal income per capita") + ylab("Financially insecure people per 100'000")
x1.x3 <- ggplot(data = expenditure,aes(x = X1, y = X3)) + geom_point() + geom_smooth(method = "lm") +
  xlab("Personal income per capita") + ylab("People residing in urban areas per 100'000")
x2.x3 <- ggplot(data = expenditure,aes(x = X2, y = X3)) + geom_point() + geom_smooth(method = "lm") +
  xlab("Financially insecure people per 100'000") + ylab("People residing in urban areas per 100'000")


plot_grid(y.x1, y.x2, y.x3, x1.x2, x1.x3, x2.x3, nrow = 2)


# Please plot the relationship between Y and Region? On average, which region has the highest per capita expenditure on housing assistance?

plot2 <- ggplot(data = expenditure, aes(x = as.factor(Region), y = Y)) + geom_boxplot() + xlab("Region") + 
  ylab("Expenditure on shelters/housing assistance per capita") + scale_x_discrete(labels = c("Northeast","North Central", "South", "West")) 
plot2

#Answer: Region 4 (West) has the biggest per capita expenditure on average

# Graph for Y and X1: see plot 1
# Reproduce the above graph including one more variable Region and display different regions with different types of symbols and colors.

plot3 <- ggplot(data = expenditure,aes(x = X1, y = Y)) + geom_point(aes(col = as.factor(Region), shape = as.factor(Region))) + 
  geom_smooth(method = "lm", se = F, aes(col = as.factor(Region))) +  xlab("Personal income per capita") + 
  ylab("Expenditure on shelters/housing assistance per capita") + labs(col = "Region", shape = "Region") +
  scale_color_discrete(breaks = c(1,2,3,4), labels = c("Northeast","North Central", "South", "West")) + 
  scale_shape_discrete(breaks = c(1,2,3,4), labels = c("Northeast","North Central", "South", "West"))
plot3
  
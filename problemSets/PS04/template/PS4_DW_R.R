#install.packages(car)
#install.packages(tidyverse)

library(tidyverse)
library(car)
data(Prestige)
help(Prestige)

#---- Question 1 ----

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

reg1 <- lm(prestige ~ income + professional + income*professional, data = Prestige)
summary(reg1)

#---- Question 2 ----

pt(2.625, df = 131-4)
pt(3.231, df = 131-4)

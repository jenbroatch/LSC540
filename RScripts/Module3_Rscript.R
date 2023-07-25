library(ggplot2)  # Loads the ggplot2 library
library(dplyr)  # Loads the dplyr library


heart <- read.csv("C:/Users/jenni/Dropbox (ASU)/Courses/LSC540/Data Sets/heart.csv") 


#Overly a normal curve 

ggplot(data = heart, aes(x = MaxHR)) + 
  geom_histogram(aes(y=..density..), bins=25) + 
  ggtitle("Distribution of Max Heart Rate") +
  xlab("Max Heart Rate")+
  stat_function(fun = dnorm, args = list(mean = mean(heart$MaxHR), sd = sd(heart$MaxHR)))


#Probability within one standard deviation
pnorm(231.7, 191, 40.7)- pnorm(150.3, 191, 40.7)

#Probability less than 200 and then 310 
pnorm(200, 191, 40.7)
pnorm(310, 191, 40.7)

#Find value from percentile
qnorm(.80, 191, 40.7)

pbinom(2, 20, 0.08)

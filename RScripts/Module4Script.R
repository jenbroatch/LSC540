#Confidence Interval Example
.099*(1-.099)/1432 

install.packages("binom", dependencies = TRUE) # only if not yet installed
library(binom) 
.099*1432 
binom.confint(142, n = 1432)

binom.confint(142, n = 1432, conf.level=.95, methods="asymptotic")
binom.confint(142, n = 1432, conf.level=.99, methods="asymptotic")
binom.confint(142, n = 1432, conf.level=.90, methods="asymptotic")


X=0.099*1432000
binom.confint(X, n = 1432000, conf.level=.95, methods="asymptotic")

library(samplingbook)
sample.size.prop(0.01, P = 0.97, N = Inf, level = 0.95) 


Flintdata<- read.csv("https://raw.githubusercontent.com/jenbroatch/LSC540/main/DataSets/Flint.csv")
View(Flintdata)
counts<- table(Flintdata$Contaminated) 

barplot(counts, main="Flint Water Contamination",
        xlab="Number of Contaminated Homes") 
barplot(counts, main="Flint Water Contamination",
        xlab="Number of Contaminated Homes", names.arg=c("No Lead Contamination Evident", 
                                                         "Lead Contamination Evident"), 
        col=c("red", "blue"))


prop.test(54, 271, p = 0.10, alternative = "greater")

prop.test(25, 271, p = 0.10, alternative = "greater")

#No Continuity Correction
prop.test(54, 271, p = 0.10, alternative = "greater", correct=F)

Teststat=((54/271)-.1)/sqrt(.1*.9/271)
Teststat

#Note that the square of a variable with a standard normal distribution 
#is a Chi-squared distribution.
Teststat^2


heart <- heart <- read.csv("C:/Users/jenn9/Dropbox (ASU)/Courses/LSC540/Data Sets/heart.csv")
View(heart)
library(dplyr)
library(ggplot2)

#Contingency Table 
my.ctable <- table(heart$ExerciseAngina, heart$HeartDisease,dnn=c("Heart Disease (1=yes, 0=no)","Exercise Angina 1=yes,0=no" ))
my.ctable
addmargins(my.ctable)
#Side by Side Bar Graph
heart %>% 
  ggplot( aes(x = ExerciseAngina, fill =as.factor(HeartDisease) )) + 
  geom_bar(position="dodge") + 
  labs(
    title = "Side by Side Bar Graph", 
    x = "Exercise Angina", 
    fill= "Heart Disease (1=Yes, 0=No)"
  ) + 
  theme_bw() 

tally(~ExerciseAngina | HeartDisease, data=heart)

prop.test(~ExerciseAngina | HeartDisease, data=heart,
          conf.level=0.95, success='N')


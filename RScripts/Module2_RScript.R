#Module2 Script 
#LSC540
#Jennifer Broatch


#Read in the heart.csv from a file 
heart <- read.csv("C:/Users/jenni/Dropbox (ASU)/Courses/LSC540/Data Sets/heart.csv") 
View(heart)

my.table <- table(heart$ChestPainType, dnn="Chest Pain Type")
my.table
addmargins(my.table)

#relative frequency table
my.table2 <- table(heart$HeartDisease, dnn="Heart Disease (1=yes, 0=no)")
prop.table(my.table2)
round(prop.table(my.table2), 4) # Rounded 
knitr::kable(my.table)


#plots for one categorical variable
library(ggplot2)  # Loads the ggplot2 library
library(dplyr)  # Loads the dplyr library

heart %>% 
  ggplot( aes(x = ChestPainType)) + 
  geom_bar(fill = "blue") + 
  labs(
    title = "Distribution of Chest Pain Type", 
    x = "Chest Pain Type"
  ) + 
  theme_bw()

#horizontal 
heart %>% 
  ggplot( aes(x = ChestPainType)) + 
  geom_bar(fill = "blue") + 
  labs(
    title = "Distribution of Chest Pain Type", 
    x = "Chest Pain Type"
  ) + 
  theme_bw() + 
  coord_flip()

heart %>% 
  ggplot( aes(x = ChestPainType, y = after_stat(count / sum(count)))) + 
  geom_bar(fill = "blue") + 
  labs(
    title = "Relative Frequency Distribution of Chest Pain Type", 
    x = "Chest Pain Type", 
    y= "Relative Frequency"
  ) + 
  theme_bw() 

#Contingency Table 
my.ctable <- table(heart$ChestPainType, heart$HeartDisease,dnn=c("Heart Disease (1=yes, 0=no)","Chest Pain Type" ))
my.ctable
knitr::kable(my.ctable)
addmargins(my.ctable) # Add Row and Column Totals 
prop.table(my.ctable)

#Side by Side Bar Graph
heart %>% 
  ggplot( aes(x = ChestPainType, fill =as.factor(HeartDisease) )) + 
  geom_bar(position="dodge") + 
  labs(
    title = "Side by Side Bar Graph", 
    x = "Chest Pain Type", 
    fill= "Heart Disease (1=Yes, 0=No)"
  ) + 
  theme_bw() 

#Stacked 
heart %>% 
  ggplot( aes(x = ChestPainType, fill =as.factor(HeartDisease) )) + 
  geom_bar() + 
  labs(
    title = "Side by Side Bar Graph", 
    x = "Chest Pain Type", 
    fill= "Heart Disease (1=Yes, 0=No)"
  ) + 
  theme_bw() 

#module 2.2
mean(heart$Age)
median(heart$Age)

#Measures of spread
sd(heart$Age)
var(heart$Age)
diff(range(heart$Age))
range(heart$Age)
summary(heart$Age)

library(mosaic) # Loads the mosaic package
favstats(Age ~ HeartDisease, data = heart)
favstats(MaxHR ~ HeartDisease, data = heart)

ggplot(data = heart, aes(x = MaxHR)) + 
  geom_histogram() + 
  ggtitle("Distribution of Max Heart Rate") +
  xlab("Max Heart Rate")


ggplot(data = heart, aes(x = MaxHR)) + 
  geom_histogram(bins=35) + 
  ggtitle("Distribution of Max Heart Rate") +
  xlab("Max Heart Rate")

#Too few bins
ggplot(data = heart, aes(x = MaxHR)) + 
  geom_histogram(bins=10) + 
  ggtitle("Distribution of Max Heart Rate") +
  xlab("Max Heart Rate")

#Too many bins
ggplot(data = heart, aes(x = MaxHR)) + 
  geom_histogram(bins=65) + 
  ggtitle("Distribution of Max Heart Rate") +
  xlab("Max Heart Rate")


ggplot(heart, aes(x=ChestPainType, y=MaxHR)) + geom_boxplot()

ggplot(heart, aes(x=as.factor(HeartDisease), y=MaxHR)) + geom_boxplot() +
  ggtitle("Distribution of MaxHR ") +
  ylab("Max Heart Rate") + 
  xlab("Heart Disease (1=Yes, 0=No)") 

#Module 2.5
my.plot <- ggplot(data = heart, aes(x = Age, y = MaxHR)) + geom_point(color = "blue") + 
  ggtitle("Relationship between Age and Max HR") +
  xlab("Age (in years) ") + 
  ylab("Max HR (beats/minute)") 
my.plot 

my.plot2 <- ggplot(data = heart, aes(x = Age, y = MaxHR)) + geom_point(color = "blue") + 
  geom_point(aes(color = factor(HeartDisease)) ) + 
  ggtitle("Relationship between Age and Max HR") +
  xlab("Age (in years) ") + 
  ylab("Max HR (beats/minute)") +
  scale_color_discrete(name="Heart Disease")
my.plot2

cor(heart$Age, heart$MaxHR)













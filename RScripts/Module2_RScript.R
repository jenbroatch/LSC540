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




#Read in healthcare-stroke.csv from URL
stroke <- read.csv("https://raw.githubusercontent.com/jenbroatch/LSC540/main/DataSets/healthcare-stroke.csv") 
str(stroke)
head(stroke)
View(stroke)

#Identify 1/0 as factors, so R knows that it is not a number 
stroke$stroke <- as.factor(stroke$stroke)
stroke$hypertension <- as.factor(stroke$hypertension)
stroke$heart_disease <-factor(stroke$heart_disease)

#Correct issue with N/A in bmi 
stroke <- transform(stroke, bmi= as.double(bmi))
str(stroke)












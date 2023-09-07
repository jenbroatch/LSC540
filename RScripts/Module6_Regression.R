#Module 6 
#Regression 
gestation <- read.csv("https://raw.githubusercontent.com/jenbroatch/LSC540/main/DataSets/Animals.csv")
reg1=lm(gestation$Longevity~gestation$Gestational.Period )
plot(gestation$Gestational.Period, gestation$Longevity, xlab="Gestational Period (days)", ylab="Longevity (years)")
abline(reg1)
summary(reg1)

library(Lock5Data)
View(AllCountries)
AllCountries1 <- AllCountries %>% 
  filter (!is.na(Developed))
AllCountries1$Developed<- as.factor(AllCountries1$Developed)

plot( AllCountries1$Hunger, AllCountries1$LifeExpectancy, xlab="% of the population considered undernourished", ylab="Life Expectancy (years)"  )


ggplot(data = AllCountries1, aes(x =LifeExpectancy)) + 
  geom_histogram() + 
  ggtitle("Distribution of LifeExpectancy") +
  xlab("LifeExpectancy (in years)")
reg1<- lm(AllCountries1$LifeExpectancy~AllCountries1$Hunger)
summary(reg1)
plot(reg1, 2)




plot( AllCountries1$GDP, AllCountries1$LifeExpectancy, xlab="GDP", ylab="Life Expectancy (years)"  )
plot( log(AllCountries1$GDP), AllCountries1$LifeExpectancy,  xlab="ln(GDP)", ylab="Life Expectancy (years)"  )
reg2<- lm(AllCountries1$LifeExpectancy~AllCountries1$Hunger+ log(AllCountries1$GDP))
summary(reg2)

ggplot(AllCountries1, aes(x=as.factor(Developed), y=LifeExpectancy)) + geom_boxplot() +
  ggtitle("Distribution Average Life expectancy by Development") +
  ylab("LifeExpectancy (years)") + 
  xlab("Categories for kilowatt hours per capita, 1= under 2500, 2=2500 to 5000, 3=over 5000") 

reg3<-lm(AllCountries1$LifeExpectancy~AllCountries1$Hunger+ log(AllCountries1$GDP)+AllCountries1$Developed)
summary(reg3)

reg4<-lm(AllCountries1$LifeExpectancy~AllCountries1$Hunger+ AllCountries1$Developed)
summary(reg4)

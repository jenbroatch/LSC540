#Module5 Script 
#LSC540
#Jennifer Broatch


#Read in the data
crab<- read.csv("https://raw.githubusercontent.com/jenbroatch/LSC540/main/DataSets/chap11q08crabFemales.csv")
View(crab)

#point estimate
mean(crab$gRatePer100days)
sd(crab$gRatePer100days)

summary(crab$gRatePer100days)
t.test(crab$gRatePer100days)
t.test(crab$gRatePer100days, mu=0.30)
#plot 

ggplot(data = crab, aes(x = gRatePer100days)) + 
  geom_histogram(bins=12) + 
  ggtitle("Distribution of Growth Rate") +
  xlab("Growth Rate Per 100 days")

#Module 5.2
library(mosaic)
library(mosaicData)
View(Gestation)
Gestation2 <-Gestation %>%
  mutate(smoke_now = if_else(smoke == "now",
                        "yes", 
                        "no")) %>%
  filter(!is.na(smoke)) 
View(Gestation2)
ggplot(Gestation2, aes(x=smoke_now, y=wt)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")+
  xlab("Smokes Now during Pregnancy")+
  ylab("Birth weight (oz)")
favstats(wt~smoke_now, data=Gestation2)


t.test(wt~smoke_now, data=Gestation2)

#module 5.3

climate<- read.csv("https://raw.githubusercontent.com/jenbroatch/LSC540/main/DataSets/climate.csv")
View(climate)

firstData <- data.frame(Averagetemp = climate$AverageTemp1901_1950, 
                        Station=climate$Station,
                  
                         time = "First Half 1901-1950")
secondData <- data.frame(Averagetemp = climate$AverageTemp1951_2000, 
                         Station=climate$Station,
                        time = "Second Half 1951-2000")
climate2 <- rbind(firstData, secondData)
climate2$time <- factor(climate2$time, levels = c("First Half 1901-1950", "Second Half 1951-2000"))


ggplot(climate2, aes(x = time, y = Averagetemp)) +  
  geom_point(size = 5, col = "firebrick", alpha = 0.5) + 
  geom_line(aes(group = Station)) +
  labs(x = "Time Period", y = "Temperature in Celcius") + 
  theme_classic()

hist(climate$Difference, xlab="Difference in Temperature 1951-2000 to 1901-1950", main=" ")

t.test(climate$AverageTemp1951_2000, climate$AverageTemp1901_1950, paired =T)
t.test(climate$Difference)


qqnorm(crab$gRatePer100days)
wilcox.test(crab$gRatePer100days, conf.int=TRUE)
crabgrowth_boot <- do(2000) * mean( ~gRatePer100days , data = resample(crab))
confint(crabgrowth_boot, level = 0.95, method = "quantile")

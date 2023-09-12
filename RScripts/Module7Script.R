#Module 7
library(dplyr)

anorexia<- read.csv("https://raw.githubusercontent.com/jenbroatch/LSC540/main/DataSets/Anorexia.csv")
View(anorexia)
boxplot(anorexia$Cognitive.Therapy, anorexia$Family.Therapy, anorexia$Control,
        names=c("Cogn. Therapy", "Family Therapy", "Control"), 
        ylab="Weight Loss/Gain (lbs)") 

anorexia_stack=na.omit(stack(anorexia))
View(anorexia_stack)


anovamod =aov(values~ind, data=anorexia_stack)
summary(anovamod) 

#Quick check of normality assumption
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(anovamod$residuals)

# QQ-plot
library(car)
qqPlot(anovamod$residuals,
       id = FALSE # id = FALSE to remove point identification
)

#Another choice
par(mfrow = c(1, 2)) # combine plots

# 1. Homogeneity of variances
plot(anovamod, which = 3)

# 2. Normality
plot(anovamod, which = 2)

#Test for normality
shapiro.test(anovamod$residuals) 
#Levene's Test

library(car)

leveneTest(values~ind, data=anorexia_stack)


#Module 7.2 Multiple comparisons

TukeyHSD(anovamod, conf.level=.95) 
plot(TukeyHSD(anovamod, conf.level=.95), las = 2)

library(multcomp)

# Tukey HSD test:
post_test <- glht(anovamod,
                  linfct = mcp(ind = "Tukey")
)
summary(post_test)
par(mfrow = c(1, 1)) # do not combine plots
plot(post_test)

TukeyHSD(anovamod)

#Chisquared tests 
#Chi squared test of independence from counts 

Observed <- cbind(c(2, 39, 131, 175, 78), c(8, 113, 138, 129, 32), 
                  c(46, 202, 120, 65,  8))

Observed
rownames(Observed)=c("Post-Grad Degree", "College Degree", "Some College", 
                     "HS Grad", "No HS Degree")

colnames(Observed)=c("<$30k","$30k to $75k", " >$75k")
Observed

prop.table(Observed,2)
barplot(prop.table(Observed,2),beside=TRUE,legend.text=TRUE,
        ylim=c(0,1),ylab="Proportions")

x=addmargins(Observed)
x
res1=chisq.test(Observed)
res1
res1$expected 
res1$stdres

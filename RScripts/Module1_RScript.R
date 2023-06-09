#Module1 Script 
#LSC540
#Jennifer Broatch

#Reading in Data from a URL

#Read in healthcare-stroke.csv from URL
stroke <- read.csv("https://raw.githubusercontent.com/jenbroatch/LSC540/main/DataSets/healthcare-stroke.csv") 
str(stroke)
head(stroke)
View(stroke)

#Read in data from textbook webiste 
#https://whitlockschluter3e.zoology.ubc.ca/ 
tigerdeath <- read.csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter02/chap02e2aDeathsFromTigers.csv")
View(tigerdeath)


#Reading in Data from a .csv file

#Read in the heart.csv from a file 
heart <- read.csv("C:/Users/jenni/Dropbox (ASU)/Courses/LSC540/Data Sets/heart.csv") 
View(heart)

#Correct issue with N/A in bmi 
stroke <- transform(stroke, bmi= as.double(bmi))
str(stroke)
#Identify as factors for later analysis 
stroke$stroke <- as.factor(stroke$stroke)
stroke$hypertension <- as.factor(stroke$hypertension)
stroke$heart_disease <-factor(stroke$heart_disease)













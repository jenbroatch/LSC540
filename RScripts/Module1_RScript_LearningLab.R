#Module1 Script 
#LSC540
#Jennifer Broatch

#Reading in Data from a URL

#Read in counties.csv from URL
countries <- read.csv("https://raw.githubusercontent.com/jenbroatch/LSC540/main/DataSets/Textbook/countries.csv") 
str(countries)
head(countries)
View(countries)


#Sometimes we want to analyze a specific group within the data, to do so we need to filter the dataset.
#Using the dplyr library answer the following questions about the countries in Europe, Africa, or Asia. 

#Select Tools --> Install Package --> Select dplyr (image)  Only once
#Additional help see R Lab - Part 2 

library(dplyr)
AsianCountries <- countries %>% filter(continent =="Asia")
View(AsianCountries)

#There are 44 Asian Countries starting with Afghanistan and ending with Yemen 

#Add a new column to your countries dataframe that is the difference 
#in ecological footprint between 2012 and 2000. Let's call this new variable: footprintdifference

countries_withdifference <- countries %>% mutate(footprintdifference =ecological_footprint_2012-ecological_footprint_2000)
View(countries_withdifference)

#The difference in ecological footprint from 2000 to 2012 for Albania is = -0.06.  Hint- Scroll over










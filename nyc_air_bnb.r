# Add your name to this file
# Jadyn Gonzalez



######################
# Nov. 12, Submission#
######################
# Setting working directory and importing data
setwd("~/Documents/MGSC310-Project-master")
nycAB = read.csv("AB_NYC_2019.csv")
# Dropping name and host name as these variables will not provide any insights
nycAB = nycAB[ , !(names(nycAB) %in% c("name","host_name"))]

# Check for missing values
sapply(nycAB, function(x) sum(is.na(x)))
# It looks like the only missing value is in reviews per month
# We can handle this by just setting NaN values to 0
nycAB[is.na(nycAB)] = 0
sum(is.na(nycAB))

# Check structure of data
str(nycAB)
# Summary of data
summary(nycAB)

# Checking distributions of data
hist(nycAB$price)
# Not a good distrubution, try log and squared
hist(log1p(nycAB$price))
hist((nycAB$price)^2)
# Log looks much better so well add the log variable to the set
nycAB$log_price = log1p(nycAB$price)

hist(nycAB$number_of_reviews)
# Skewed distribution as well
hist(log1p(nycAB$number_of_reviews))
hist((nycAB$number_of_reviews)^2)
# Again log distribution is much better so we'll add that
nycAB$log_num_reviews = log1p(nycAB$number_of_reviews)

# Investigate listings on the neighborhood groups
levels(nycAB$neighbourhood_group)
# Looking at average price, max, and min for each group
mean(nycAB$price[nycAB$neighbourhood_group=="Bronx"])
max(nycAB$price[nycAB$neighbourhood_group=="Bronx"])
min(nycAB$price[nycAB$neighbourhood_group=="Bronx"])

mean(nycAB$price[nycAB$neighbourhood_group=="Brooklyn"])
max(nycAB$price[nycAB$neighbourhood_group=="Brooklyn"])
min(nycAB$price[nycAB$neighbourhood_group=="Brooklyn"])

mean(nycAB$price[nycAB$neighbourhood_group=="Manhattan"])
max(nycAB$price[nycAB$neighbourhood_group=="Manhattan"])
min(nycAB$price[nycAB$neighbourhood_group=="Manhattan"])

mean(nycAB$price[nycAB$neighbourhood_group=="Queens"])
max(nycAB$price[nycAB$neighbourhood_group=="Queens"])
min(nycAB$price[nycAB$neighbourhood_group=="Queens"])

mean(nycAB$price[nycAB$neighbourhood_group=="Staten Island"])
max(nycAB$price[nycAB$neighbourhood_group=="Staten Island"])
min(nycAB$price[nycAB$neighbourhood_group=="Staten Island"])

# The Bronx has the lowest average price as well as the lowest max price
# Manhattan has the highest average price
library(ggplot2)
# Summary from the means/max/min reflects in the boxplot
# Have to use log transformed for best visualization
ggplot(nycAB,aes(x=neighbourhood_group, y=log_price, fill=neighbourhood_group)) + geom_boxplot()

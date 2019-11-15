# Add your name to this file
# Jadyn Gonzalez



######################
# Nov. 12, Submission#
######################
# Setting working directory and importing data
setwd("~/Downloads/MGSC310-Project-master")
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
nycAB$id = as.factor(nycAB$id)
nycAB$host_id = as.factor(nycAB$host_id)
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
# Remove outliers with log price > 7 and < 3
nycAB = nycAB[!(nycAB$log_price < 3),]
nycAB = nycAB[!(nycAB$log_price > 7),]
hist(nycAB$log_price)


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
# Have to use log transformed to get best visualization
ggplot(nycAB,aes(x=neighbourhood_group, y=log_price, fill=neighbourhood_group)) + geom_boxplot()
# We see that Manhattan has the higher price range compared to Queens and Staten Island, this
# makes sense as Manhattan is more of a city/tourist/business area while Queens and Staten Island are
# more residential areas, not sure about Bronx and Brooklyn.
# Let's check the count of listings, this may influence the distributions.
barplot(table(nycAB$neighbourhood_group))
# We see that Brooklyn and Manhattan have far more listings than the other cities.


# Lets check for correlations
library(corrplot)
corrplot(cor(nycAB[, c(5,6,8,9,10,12,13,14,15,16)]))
# We see that count of host listings and number of days available per year have some positive correlation
# with price, number of reviews and reviews per month shows some negative correlation but this may be
# insignificant as there  is no way to distinguise between positive and negative reviews
View(nycAB)

ggplot(nycAB,aes(x=room_type, y=log_price, fill = room_type)) + geom_boxplot()
# We can see that listings where the entire home/apartment
#is offered generally has the highest price range, whereas
#a private room has the second highest price range,
#with a shared room falling in the lowest price range

nycAB$log_minimum_nights = log1p(nycAB$minimum_nights)
nycAB$log_reviews_per_month = log1p(nycAB$reviews_per_month)

ggplot(nycAB,aes(x=minimum_nights, y=reviews_per_month)) + geom_point() + geom_smooth(size = .5)
#This plot shows a relationship between minimum nights and 
#reviews per month. It shows that there is a negative 
#relationship up to a certain point (as minimum nights increases,
#reviews per month decreases) and then it begins to even out, 
#as there is little to no relationship between the two after
#a certain point

ggplot(nycAB,aes(x=log_minimum_nights, y=log_reviews_per_month)) + geom_point() + geom_smooth(size = .5)
#this plot shows the relationship a between the two variables
#a bit more cleanly, as we have found the log of both. 

       
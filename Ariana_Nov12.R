#Ariana Bucio
#2299408
#bucio@chapman.edu
#MGSC310-04
#November12_Task

install.packages("ggplot2")

getwd()
setwd("~/Documents/Chapman_Stuff/Junior/Semester_1/MGSC310/Project/Airbnb")
airbnb <- read.csv("AB_NYC_2019.csv")

#Get names
names(airbnb)

#Variables we do not need: name, latitude, longitude, last review, host_id, host_name
airbnb <- subset(airbnb, select = -c(name, latitude, longitude, last_review, id, host_name))
#Set NaN values equal to 0
airbnb[is.na(airbnb)] = 0
sum(is.na(airbnb))

#Convert factor variables to factors
airbnb$neighbourhood_group <- factor(airbnb$neighbourhood_group)
airbnb$room_type <- factor(airbnb$room_type)
airbnb$calculated_host_listings_count <- factor(airbnb$calculated_host_listings_count)

#Check structure and get summary statistics
str(airbnb)
summary(airbnb)

#Check distributions of variables
#We only want to look at numeric variables
numerics <- unlist(lapply(airbnb, is.numeric))
numerics
sum(numerics) #there are 6 total

#par(mar=c(1,1,1,1))
par(mfrow=c(2,3))
hist(airbnb$host_id)
hist(airbnb$number_of_reviews)
hist(airbnb$availability_365)
hist(airbnb$price)
hist(airbnb$reviews_per_month)
hist(airbnb$minimum_nights)

#Price, reviews_per_month, minimum_nights, & number_of_reviews
#do not have good distributions. Let's try some transformations. 

#For price
par(mfrow=c(1,2))
hist(log(airbnb$price))
hist((airbnb$price)^2)
#Log looks better, so let's use that.

#For reviews_per_month
par(mfrow=c(1,2))
hist(log(airbnb$reviews_per_month))
hist((airbnb$reviews_per_month)^2)
#Log looks better, so let's use that.

#For minimum_nights
par(mfrow=c(1,2))
hist(log(airbnb$minimum_nights))
hist((airbnb$minimum_nights)^2)
#Log looks better, so let's use that.

#For number_of_reviews
par(mfrow=c(1,2))
hist(log(airbnb$number_of_reviews))
hist((airbnb$number_of_reviews)^2)
#Log looks better, so let's use that.


















# Ariana Bucio
# Jadyn Gonzalez
# Matt Mead
# Bryce Viorst


######################
# Nov. 14, Submission#
######################
# Setting working directory and importing data
setwd("~/Documents/MGSC310-Project-master")
nycAB = read.csv("AB_NYC_2019.csv")
# Dropping name and host name as these variables will not provide any insights
nycAB = nycAB[ , !(names(nycAB) %in% c("name","id","host_name", "latitude", "longitude", "last_review", "calculated_host_listings_count"))]

# Check for missing values
sapply(nycAB, function(x) sum(is.na(x)))
# It looks like the only missing value is in reviews per month
# We can handle this by just setting NaN values to 0
nycAB[is.na(nycAB)] = 0
sum(is.na(nycAB))

# Check structure of data
str(nycAB)
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

par(mfrow=c(1,2))
hist(log(nycAB$reviews_per_month))
hist((nycAB$reviews_per_month)^2)
nycAB$reviews_per_month_log = log1p(nycAB$reviews_per_month)

par(mfrow=c(1,2))
hist(log(nycAB$minimum_nights))
hist((nycAB$minimum_nights)^2)
#Log looks better, so let's use that.
nycAB$minimum_nights_log = log1p(nycAB$minimum_nights)

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
corrplot(cor(nycAB[, c(6,7,8,9,10,11,12,13)]))
# We see that count of host listings and number of days available per year have some positive correlation
# with price, number of reviews and reviews per month shows some negative correlation but this may be
# insignificant as there  is no way to distinguise between positive and negative reviews

ggplot(nycAB,aes(x=room_type, y=log_price, fill = room_type)) + geom_boxplot()
# We can see that listings where the entire home/apartment
#is offered generally has the highest price range, whereas
#a private room has the second highest price range,
#with a shared room falling in the lowest price range

ggplot(nycAB,aes(x=minimum_nights, y=reviews_per_month)) + geom_point() + geom_smooth(size = .5)
#This plot shows a relationship between minimum nights and 
#reviews per month. It shows that there is a negative 
#relationship up to a certain point (as minimum nights increases,
#reviews per month decreases) and then it begins to even out, 
#as there is little to no relationship between the two after
#a certain point

ggplot(nycAB,aes(x=minimum_nights_log, y=reviews_per_month_log)) + geom_point() + geom_smooth(size = .5)
#this plot shows the relationship a between the two variables
#a bit more cleanly, as we have found the log of both.

###########################
### Nov. 21, Submission ###
###########################

# We'll run a regression model using a decision tree
# First we'll set the seed and split the data
set.seed(310)
trainidx = sample(1:nrow(nycAB),size=0.75*nrow(nycAB))
train = nycAB[trainidx,]
test = nycAB[-trainidx,]

# Next well run two tree models, one with price and one with the log transform price
library(tree)
regMod = tree(price ~ neighbourhood_group + room_type 
              + minimum_nights + number_of_reviews + reviews_per_month_log
              + availability_365 + log_num_reviews + reviews_per_month_log 
              + minimum_nights_log,
              data = train)

logMod = tree(log_price ~ neighbourhood_group + room_type 
              + minimum_nights + number_of_reviews + reviews_per_month_log
              + availability_365 + log_num_reviews + reviews_per_month_log 
              + minimum_nights_log,
              data = train)

# We'll plot both
plot(regMod)
text(regMod, pretty=0)

plot(logMod)
text(logMod, pretty = 0)

# In both trees we can see that the variable that affects price the most is room type,
# more specifically if the room type is the whole house or not. The next most important node 
# for both trees is neighbourhood_group, if it is Manhattan or not. The model using log_price only
# gives these nodes. The model using the given price variable includes a node if the room is the 
# whole house and is in manhattan it looks at availability.

# We'll use cross-validation to find the best tree size for both models.
cvTreeR = cv.tree(regMod)
cvTreeR
bestIdx = which.min(cvTreeR$dev)
cvTreeR$size[bestIdx]
# Best size is 5

cvTreeL = cv.tree(logMod)
cvTreeL
bestIdx = which.min(cvTreeL$dev)
cvTreeL$size[bestIdx]
# Best size is 4

# Now we'll prune the trees using the best size we got from CV and generate predictions
prunedTreeR = prune.tree(regMod, best = 5)
predsTrainR = predict(prunedTreeR)
predsTestR = predict(prunedTreeR, newdata = test)

prunedTreeL = prune.tree(logMod, best = 4)
predsTrainL = predict(prunedTreeL)
predsTestL = predict(prunedTreeL, newdata = test)

# Now we'll calculate MSE for both models
MSE = function(p,t){
  mean((t-p)^2)
}

MSE(predsTrainR, train$price) #9975.12
MSE(predsTestR,test$price) #9570.264

MSE(predsTrainL, train$log_price) #0.231
MSE(predsTestL, test$log_price) #0.228

# MSE is much lower when we use the log transformation
# we do get a lower MSE in the test set which may be an indication that 
# the model is overfitting the data.
plot(prunedTreeL)
text(prunedTreeL, pretty = 0)






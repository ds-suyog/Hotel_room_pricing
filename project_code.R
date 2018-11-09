# Project Title: Hotel Room Pricing In The Indian Market

## Task-1 
## Read data
cities42.data.raw.df     <- read.csv(paste("Cities42.csv", sep=""))
cities42Temp.df   <- read.csv(paste("Cities42.csv", sep="")) #a copy

# Output the number of missing values for each column (8 missing)
sapply(cities42.data.raw.df,function(x) sum(is.na(x)))

# Cleaning Data: omitting rows which have value NA in column HotelDescription
cities42.df <- na.omit(cities42.data.raw.df)
cities42Temp.df <- na.omit(cities42.data.raw.df)

#checking new dimensions (now 13224 rows, before 13332 rows)
dim(cities42.df)

# Cleaning Data: Merging duplicate levels of Date,i.e. same dates in different format
ha <- list(
  "04-Jan-17" = c("04-Jan-17", "Jan 04 2017", "Jan 4 2017"),
  "08-Jan-17" = c("08-Jan-17", "Jan 08 2017", "Jan 8 2017"),
  "18-Dec-16" = c("18-Dec-16", "Dec 18 2016"),
  "21-Dec-16" = c("21-Dec-16", "Dec 21 2016"),
  "24-Dec-16" = c("24-Dec-16", "Dec 24 2016"),
  "25-Dec-16" = c("25-Dec-16", "Dec 25 2016"),
  "28-Dec-16" = c("28-Dec-16", "Dec 28 2016"),
  "31-Dec-16" = c("31-Dec-16", "Dec 31 2016")
)
for (i in 1:length(ha)) levels(cities42.df$Date)[levels(cities42.df$Date)%in%ha[[i]]] <- names(ha)[i]
levels(cities42.df$Date)

## Task-2
## Summarize the Data
describe(cities42.df)

#considering selected fields only
cities42Temp.df <- cities42.df[,c(2, 4:7,10:12, 16:19)]   
describe(cities42Temp.df)

# Data Types
str(cities42.df)


## Task-3 
# Y = F(x1, x2, x3, ....)
# Y is dependednt variable;  x1, x2, x3.. are independent variables
# RoomRent = F {StarRating, CityName, IsMetroCity,.....}


## Task-4
# Y = RoomRent

## Task-5
# Picking 3 most important independent variables
# These seem of high importance: StarRating, IsTouristDestination, HotelName,	HasSwimmingPool, IsMetroCity, ISNewYearEve.  
# Factor HotelName is accounted in StarRating
# 3 most imp: StarRating,IsTouristDestination, HasSwimmingPool 

#RoomRent = Function (StarRating, IsTouristDestination, HasSwimmingPool)


## TASK-6 
# Visualizing  StarRating, IsTouristDestination and HasSwimmingPool independently

bwplot(jitter(cities42.df$StarRating),  horizontal=TRUE,
       main = "Distribution of star ratings", xlab="StarRating")

table(cities42.df$IsTouristDestination)

table(cities42.df$HasSwimmingPool)



## TASK-7
# Scatter Plots/ box plots to understand how are RoomRent, StarRating 
# IsTouristDestination and HasSwimmingPool correlated pair-wise

# Checking relation between RoomRent and StarRating
#Using Jitter
scatterplot ( jitter(RoomRent) ~ jitter(StarRating), data = cities42.df,
              spread=FALSE, pch=19,  
              ylab="RoomRent (in Rs)",xlab = "Rating in stars (0-5)", 
              main = "RoomRent Vs Ratings in Stars")

# Checking relation between RoomRent and Tourist Destination
#Using Jitter
scatterplot (jitter(RoomRent)~jitter(IsTouristDestination), data = cities42.df,
             spread=FALSE, pch=19,
       ylab="RoomRent (in Rs)", xlab = "IsTOUristDestination (1-Yes;0-No)", 
             main = "RoomRent Vs Tourist Destination")

aggregate (RoomRent ~ IsTouristDestination, data = cities42.df, mean)


# Checking  relation between RoomRent and HasSwimmingPool
#Using Jitter
scatterplot (jitter(RoomRent)~jitter(HasSwimmingPool), data = cities42.df,
             spread=FALSE, pch=19,
             ylab="RoomRent (in Rs)", xlab = "HasSwimmingPool (1-Yes;0-No)", 
             main = "RoomRent Vs SwimmingPool")

aggregate (RoomRent ~ HasSwimmingPool, data = cities42.df, mean)


# Checking  relation between StarRating and HasSwimmingPool
#Using Jitter
scatterplot ( jitter(StarRating) ~ jitter(HasSwimmingPool), data = cities42.df,
              spread=FALSE, pch=19, 
              ylab="StarRating (1-5)", xlab = "HasSwimmingPool (1-Yes;2-No)", 
              main = "StarRating Vs Swimming Pool")

aggregate (StarRating ~ HasSwimmingPool, data = cities42.df, mean)


scatterplotMatrix(~RoomRent + StarRating + IsTouristDestination + 
          HasSwimmingPool, data=cities42.df,
          main="RoomRent and predictors correlation")


## TASK-8 
# Corrgram of RoomRent, StarRating, IsTouristDestination and HasSwimmingPool

cities42Subset <- cities42.df[,c(5,10:11, 19)]

corrgram(cities42Subset, order=TRUE, lower.panel=panel.pts,
         upper.panel=panel.pie, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Corrgram of RoomRent and predictors intercorrelations")


## Task-9
# Variance-Covariance Matrix for RoomRent, StarRating, IsTouristDestination
# and HasSwimmingPool

corr.test(cities42Subset, use="complete")

#-----------------------------------------




## Task-10
# Hypoyhesis 1 = H1 =  Hotels in metro-cities charge higher rent for room than those which are not in metro cities.
# Hypothesis 2 = H2 = Hotel with higher ratings (in stars) charge higher  rent for room than those of lower rating


## Task-11
#Appropriate T-test to check hypothesis

#T-Test for H1
# Testing with Original cleaned Data-set Cities42.df (Cities42B.df is used for Regression Model)
t.test(RoomRent ~ IsMetroCity, cities42.df) 
# Now, using Data-set (cities42B.df), i.e. after removing very high RoomRent values (quantiles above .95). RegressionModel is based on this data-set.
t.test(RoomRent ~ IsMetroCity, cities42B.df) 

#T-Test for H2
#Testing with Original cleaned Data-set Cities42.df (Cities42B.df is used for Regression Model)
t.test(RoomRent ~ HasSwimmingPool, cities42.df) 
#Now, using Data-set (cities42B.df), i.e. after removing very high RoomRent values (quantiles above .95). RegressionModel is based on this data-set.
t.test(RoomRent ~ HasSwimmingPool, cities42B.df) 


##Task-12 
#Formulating Regression Model
# y = RoomRent
# x = {x1, x2, ..} = {CityName + StarRating + ISMetroCity + Date +  ... }
# RoomRent = b0 + b1*Date + b2*CityName + b3*IsMetroCity + b4*StarRating ...+  ???




##  Task-13
#                   Fitting Linear Regression Model

# Model containing all predictors
Model1 <- RoomRent ~ CityName + Population + CityRank + IsMetroCity + IsTouristDestination + IsWeekend + IsNewYearEve + Date + 
  HotelName + StarRating + Airport + HotelAddress + HotelPincode + HotelDescription + FreeWifi + 
  FreeBreakfast + HotelCapacity + HasSwimmingPool  

# Model containing all predictors, except HotelDescription, HotelAddress, HotelPincode
Model1 <- RoomRent ~ CityName + Population + CityRank + IsMetroCity + IsTouristDestination + IsWeekend + IsNewYearEve + Date + 
  HotelName + StarRating + Airport + HotelPincode + FreeWifi + 
  FreeBreakfast + HotelCapacity + HasSwimmingPool  
fit1 <-lm(Model1, data = cities42.df)
summary(fit1)   #Multiple R-squared:  0.765,	Adjusted R-squared:  0.731 


# Removing outlier in RoomRent, which are six digit values
# cities42.df <- na.omit(cities42.data.raw.df)
cityOmitHighRent.df <- cities42.df[which(cities42.df$RoomRent <99999),]

Model1 <- RoomRent ~ CityName + Population + CityRank + IsMetroCity + IsTouristDestination + IsWeekend + IsNewYearEve + Date + 
  HotelName + StarRating + Airport + HotelPincode + FreeWifi + 
  FreeBreakfast + HotelCapacity + HasSwimmingPool  
fit1 <-lm(Model1, data = cityOmitHighRent.df)
summary(fit1)  #Multiple R-squared:  0.881,	Adjusted R-squared:  0.864

# Visualizing the Beta coefficients
#library (coefplot)
#coefplot(fit1)

# Final model after filtering it
Model1 <- RoomRent ~  StarRating + HotelName + Date
fit1 <-lm(Model1, data = cityOmitHighRent.df)
summary(fit1) #Multiple R-squared:  0.88,	Adjusted R-squared:  0.862 
          # summary(fit1)$coefficients[1:4,]

# Result: But I can't interpret HotelName in Final Regression Model (y = b0 + b1*x1 + b2*x2...), so I am rejecting this Model


# Testing model based on 3 most important metrics chosen in Task-5 
Model1 <- RoomRent ~  StarRating + IsTouristDestination + HasSwimmingPool
fit1 <-lm(Model1, data = cityOmitHighRent.df)
summary(fit1)   #Multiple R-squared:  0.234 

# Result: It is poor model. I conclude that it is not sensitive to variations.



# Again starting from zero by involving all predictors, except those which can't be interpreted.
# Removing Columns (HotelDescription, HotelAddress, HotelName, Pincode) whose impact can't be interpreted in model
Model1 <- RoomRent ~ CityName + Population + CityRank + IsMetroCity + IsTouristDestination + IsWeekend + IsNewYearEve + Date + 
   StarRating + Airport + FreeWifi + FreeBreakfast + HotelCapacity + HasSwimmingPool 
fit1 <-lm(Model1, data = cityOmitHighRent.df)
summary(fit1) # Multiple R-squared: 0.326


# Excluding outliers which aren't important exceptions; like frauds in bank data anlysis, which we must keep
# Excluding RoomRent quantile over 0.95 (at 0.95 value is 13398)
describe(cities42.df$RoomRent)
cities42B.df <- cities42.df[which(cities42.df$RoomRent < 13500), ]
fit1 <-lm(Model1, data = cities42B.df)
summary(fit1)   # Multiple R-squared: 0.4911


# Final filtered Model (after removing non important predictors)
Model1 <- RoomRent ~ CityName + Date + StarRating + HasSwimmingPool 
fit1 <-lm(Model1, data = cities42B.df)
summary(fit1)   # Multiple R-squared: 0.481, Adjusted R-squared:  0.48 

#RoomRent = -1940 + b1*Date + b2*CityName + 1679*StarRating+1037*HasSwimmingPool  
# Interpreting b2 *CityName:
# b2*CityName = b21*Agra + b22*Ahmedabad + b23*Amritsar+...+ b242*Varanasi
# In b22, first "2" represents mother coefficient b2, latter "2" represent level of city Ahmedabad
# In b242, first "2" represents mother coefficient b2, latter "42" represent level of city Varanasi
# For example: If city is Varanasi, then Varanasi = 1, rest city = 0. Only coefficient of Varanasi remains.
# b2*CityName = b242*1 , in which b242 is coefficient for Varanasi.
# Similarly b1*Date  is Interpreted

#Comparing RoomRent with Fitted Values
#Comparing  RoomRent predicted by the model with the actual RoomRent given in the data
predictedRoomRent = data.frame(fitted(fit1)) 
actualRoomRent = data.frame(cities42B.df$RoomRent) 
RoomRentComparison = cbind(actualRoomRent, predictedRoomRent) 
View(RoomRentComparison)

hist(residuals(fit1), col = "gray")   # majority of residuals are between -2500 to +2500

          

#Final Framework to test cities individually (StarRating and HasSwimmingPool are strongly dependent, so removing HasSwimmingPool)
# 95% rents ae below 13500

#Reading Data
cityMumbai.df     <- read.csv(paste("0 Mumbai.csv", sep=""))
cityIndore.df     <- read.csv(paste("11 Lucknow.csv", sep=""))
cityLucknow.df    <- read.csv(paste("14 Indore.csv", sep=""))
cityDelhi.df <- read.csv(paste("1 Delhi.csv", sep=""))
cityJaipur.df <- read.csv(paste("9 Jaipur.csv", sep=""))
cityBangalore.df <- read.csv(paste("2 Bangalore.csv", sep=""))

#Cleaning of NA value
cityMumbai.df <- na.omit(cityMumbai.df)
cityIndore.df <- na.omit(cityIndore.df)
cityLucknow.df <- na.omit(cityLucknow.df)
cityDelhi.df <- na.omit(cityDelhi.df)
cityJaipur.df <- na.omit(cityJaipur.df)
cityBangalore.df <- na.omit(cityBangalore.df)

# Now predictors are {Date, StarRating, SwimmingPool}

cityMumbaiB.df <- cityMumbai.df[which(cityMumbai.df$RoomRent < 13500),]
Model1 <- RoomRent ~ Date + StarRating +  HasSwimmingPool 
fit1 <-lm(Model1, data = cityMumbaiB.df)
summary(fit1)  # Multiple R-squared: 0.42

cityIndoreB.df <- cityIndore.df[which(cityIndore.df$RoomRent < 13500),]
Model1 <- RoomRent ~ Date + StarRating +  HasSwimmingPool 
fit1 <-lm(Model1, data = cityIndoreB.df)
summary(fit1)  # Multiple R-squared: 0.55


cityLucknowB.df <- cityLucknow.df[which(cityLucknow.df$RoomRent < 13500),]
Model1 <- RoomRent ~ Date + StarRating +  HasSwimmingPool 
fit1 <-lm(Model1, data = cityLucknowB.df)
summary(fit1)  # Multiple R-squared: 0.48

cityDelhiB.df <- cityDelhi.df[which(cityDelhi.df$RoomRent < 13500),]
Model1 <- RoomRent ~ Date + StarRating +  HasSwimmingPool 
fit1 <-lm(Model1, data = cityDelhiB.df)
summary(fit1) # Multiple R-squared: 0.60

cityJaipurB.df <- cityJaipur.df[which(cityJaipur.df$RoomRent < 13500),]
Model1 <- RoomRent ~ Date + StarRating +  HasSwimmingPool 
fit1 <-lm(Model1, data = cityJaipurB.df)
summary(fit1) # Multiple R-squared: 0.46

cityBangaloreB.df <- cityBangalore.df[which(cityBangalore.df$RoomRent < 13500),]
Model1 <- RoomRent ~ Date + StarRating +  HasSwimmingPool 
fit1 <-lm(Model1, data = cityBangaloreB.df)
summary(fit1)   # Multiple R-squared: 0.56


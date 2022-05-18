setwd("C:\\Users\\danie\\Documents\\CIS 3920 Final Project")

df_airbnb = read.csv("listings.csv.gz", stringsAsFactors = FALSE)

# Checking to make sure its a dataframe. 
# class(df_airbnb)

# Importing necessary libraries
library(dplyr)
library(stringr)
library(ISLR)
library(factoextra)
library(caret)
library(rpart)

set.seed(5552471)

str(df_airbnb)

# Data Cleaning - Removing unnecessary columns in df_airbnb

drop <- c("listing_url", "scrape_id", "last_scraped", "neighborhood_overview", "description", "picture_url",
         "host_url", "host_since", "host_location", "host_about", "host_thumbnail_url", 
         "host_picture_url", "host_neighbourhood", "host_listing_count", "host_verifications",
         "host_has_profile_pic", "host_identity_verified", "neighbourhood", "latitude",
         "longitude", "bathrooms", "minimum_minimum_nights", "maximum_minimum_nights", 
         "minimum_maximum_nights", "maximum_maximum_nights", "calendar_updated", "calendar_last_scraped",
         "number_of_reviews_ltm", "number_of_reviews_l30d","first_review", "last_review", "license", "id", "name",
         "host_id", "host_name", "neighbourhood_cleansed", "host_response_time", "host_response_rate", "minimum_nights_avg_ntm",
         "maximum_nights_avg_ntm", 'calculated_host_listings_count','calculated_host_listings_count_entire_homes', 
         'calculated_host_listings_count_private_rooms','calculated_host_listings_count_shared_rooms','reviews_per_month',
         "instant_bookable","amenities")

df_airbnb_clean = df_airbnb[,!(names(df_airbnb) %in% drop)]

df_airbnb_clean = as.data.frame(df_airbnb_clean)

# head(df_airbnb_cleaned,5)
# names(df_airbnb_clean)

#------------------------------------------------------------------------------------------------------------------------------

# Data Cleaning - Removing whitespace from df_airbnb_clean:
df_airbnb_clean %>% mutate_if(is.character, str_trim)

#------------------------------------------------------------------------------------------------------------------------------

# Data Cleaning - Changing the datatype to numeric for the 'price' column. 

# head(df_airbnb_clean$price) 
#Initially, the price column's datatype is factor; need to change to numeric.

# Removing the dollar signs for the 'price' column's row values. 
df_airbnb_clean$price = gsub("\\$", "", df_airbnb_clean$price)

# head(df_airbnb_clean$price) # Confirming the removal of the $ signs.

# Converting 'price' column in dataframe to numeric datatype
df_airbnb_clean$price = as.numeric(df_airbnb_clean$price)

# str(df_airbnb_clean$price) # Checking to see if the price column changed to numeric.

# Removing any N.A values that resulted from this datatype conversion process. 
dim(df_airbnb_clean)

df_airbnb_clean = na.omit(df_airbnb_clean)

# dim(df_airbnb_clean)
# unique(df_airbnb_clean$price)

#------------------------------------------------------------------------------------------------------------------------------

# Data Cleaning - Removing any rows in dataframe with has_availability == false and N.A values. 

# summary(df_airbnb_clean["has_availability"])
df_airbnb_clean = subset(df_airbnb_clean, has_availability == 't')

# removing any rows in dataframe with N.A. values <- don't serve our research purpose. 
df_airbnb_clean = na.omit(df_airbnb_clean)

# Dropping 'has_availability' column
df_airbnb_clean$has_availability = NULL

#------------------------------------------------------------------------------------------------------------------------------

# Data Cleaning - Converting 'host_acceptance_rate' to numeric

# unique(df_airbnb_clean$host_acceptance_rate)
df_airbnb_clean = na.omit(df_airbnb_clean)

# Removing the percentage symbol at the end of the values. 
df_airbnb_clean$host_acceptance_rate = as.numeric(gsub(".{1}$","", df_airbnb_clean$host_acceptance_rate))
df_airbnb_clean = na.omit(df_airbnb_clean)
# unique(df_airbnb_clean$host_acceptance_rate)

#------------------------------------------------------------------------------------------------------------------------------

# Checking the datatypes of each column in dataframe
str(df_airbnb_clean)

attach(df_airbnb_clean)

# Different room types offered:
unique(room_type)

# Different Property Types listed:
unique(property_type)

# Unique number of bathrooms:
unique(bathrooms_text)

# Overview of the cleaned dataset: df_airbnb_clean
summary(df_airbnb_clean)

# # Finding the average price of the listings in NY. 
# avg_price_bnb = sum(df_airbnb_clean$price, na.rm=TRUE)/(dim(df_airbnb_clean)[1])
# avg_price_bnb

# # Could also just use this.
# summary(df_airbnb_clean$price)

names(df_airbnb_clean)

# Creating a separate dataframe with all numerical values
dfBnbNum = subset(df_airbnb_clean, select=c("host_acceptance_rate", "host_listings_count",
                                         "host_total_listings_count", "accommodates", 
                                         "bedrooms", "beds", "price", "minimum_nights",
                                         "maximum_nights", "availability_30", "availability_60",
                                         "availability_90", "availability_365", "number_of_reviews",
                                         "review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness",
                                         "review_scores_checkin", "review_scores_communication", "review_scores_location",
                                         "review_scores_value", "neighbourhood_group_cleansed"))

head(dfBnbNum)

# Multiple Linear Regression

attach(df_airbnb_clean)


df_airbnb_clean$bathrooms_text = as.factor(df_airbnb_clean$bathrooms_text)
df_airbnb_clean$neighbourhood_group_cleansed = as.factor(df_airbnb_clean$neighbourhood_group_cleansed)
df_airbnb_clean$property_type = as.factor(df_airbnb_clean$property_type)
df_airbnb_clean$host_is_superhost = as.factor(df_airbnb_clean$host_is_superhost)
df_airbnb_clean$room_type = as.factor(df_airbnb_clean$room_type)

# lin_reg = lm(price~review_scores_accuracy+review_scores_cleanliness+review_scores_checkin + 
#              review_scores_communication+review_scores_location + review_scores_value, data = df_airbnb_clean)

# lin_reg = lm(price~host_acceptance_rate+neighbourhood_group_cleansed+property_type+room_type+accommodates+bathrooms_text+
#              bedrooms+beds+minimum_nights+maximum_nights+availability_30+availability_60+availability_90+
#              availability_365+review_scores_accuracy+review_scores_cleanliness+review_scores_checkin + 
#              review_scores_communication+review_scores_location + review_scores_value, data = df_airbnb_clean)

# lin_reg = lm(price~host_acceptance_rate+neighbourhood_group_cleansed+accommodates+bathrooms_text+bedrooms+minimum_nights+
#             review_scores_cleanliness+review_scores_checkin+review_scores_location+review_scores_value, data = df_airbnb_clean)

# lin_reg = lm(price~neighbourhood_group_cleansed+accommodates+bathrooms_text+bedrooms+minimum_nights+review_scores_accuracy
#              +review_scores_cleanliness+review_scores_checkin+review_scores_location+review_scores_value, data = df_airbnb_clean)

lin_reg = lm(price~., data = df_airbnb_clean)

summary(lin_reg)
# plot(lin_reg)

# Multiple Linear Regression Model - Is there a linear relationship between price and the listing's characteristics?
lin_reg_house = lm(price~bathrooms_text+bedrooms+accommodates+neighbourhood_group_cleansed+review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+ 
                   review_scores_location+review_scores_value, data = df_airbnb_clean)

summary(lin_reg_house)

# Using PCA for Regression - Dimensionality Reduction while maximizing variance
# Using only numerical variables = dfBnbNum

# Splitting training and testing set
set.seed(21)

df_pcr = subset(dfBnbNum, select = c("host_acceptance_rate", "host_listings_count",
                                     "host_total_listings_count", "accommodates", 
                                     "bedrooms", "beds", "price", "minimum_nights",
                                     "maximum_nights", "availability_30", "availability_60",
                                     "availability_90", "availability_365", "number_of_reviews",
                                     "review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness",
                                     "review_scores_checkin", "review_scores_communication", "review_scores_location",
                                     "review_scores_value"))

idx = floor(0.75 * nrow(df_pcr))
train_ind = sample(seq_len(nrow(df_pcr)), size=idx)

train = df_pcr[train_ind,]
test = df_pcr[-train_ind,]

simple.lm = lm(price~., data=df_pcr)
summary(simple.lm)

library(Metrics)

lm.pred = predict(simple.lm, test)
lm.pred

# Extremely high error in the predictions of price for the simple linear regression model. 
# This indicates a terrible model....
rmse(actual = test$price, predicted=as.numeric(lm.pred))

library(pls) # Library relating to principle components + factor analysis

pcr_model = pcr(price~.,
               data = train,
               scale = TRUE,
               validation = "CV")

summary(pcr_model)

pcr_pred = predict(pcr_model, test, ncomp = 16)
rmse(actual = test$price, predicted= as.numeric(pcr_pred))
# Higher RMSE because we are using less components!

library(psych)

pc_fit = prcomp(~
                host_acceptance_rate+host_listings_count+
                host_total_listings_count+accommodates+ 
                bedrooms+beds+price+minimum_nights+
                maximum_nights+availability_30+availability_60+
                availability_90+availability_365+number_of_reviews+
                review_scores_rating+review_scores_accuracy+review_scores_cleanliness+
                review_scores_checkin+review_scores_communication+review_scores_location+
                review_scores_value,
                data = train, scale = TRUE)

summary(pc_fit)
screeplot(pc_fit, type='l', main="Screeplot for Price Features")

# Elbow forms at around 5 or 6. 

trans_test = as.data.frame(predict(pc_fit, test)[,1:5]) 
# Capturing the first 5 variables <- Elbow

str(trans_test) # Checking for 5 variables in test set. 

new_train = as.data.frame(cbind(train$price, pc_fit$x[,1:5]))
str(new_train)
# We have V1 (price) = what we want to predict
# and we have the 5 independent features

colnames(new_train)[1] = "price" # Changing V1 column name to Price
str(new_train)

pcr_lm_model = lm(price~., data=new_train)
summary(pcr_lm_model)

# Using our new PCR model, predicting price. 
pcr_predictions = predict(pcr_lm_model, trans_test)
rmse(actual = test$price, predicted = as.numeric(pcr_predictions))

# RMSE is still high, but it is better than RMSE from simple linear regression model. 

# Confidence Intervals of the Multiple Linear Regression Model:
confint(lin_reg)

# Residual Standard Error (RSE) of Multiple Linear Regression Model:
sigma(lin_reg)/mean(df_airbnb_clean$price)

# Logistic Regression - Predicting a superhost

attach(df_airbnb_clean)

# glm.fit = glm(host_is_superhost~host_listings_count+number_of_reviews+
#               host_acceptance_rate+review_scores_rating+
#               review_scores_accuracy+review_scores_checkin+
#               review_scores_communication+review_scores_accuracy+
#               review_scores_location+ review_scores_cleanliness, 
#               data = df_airbnb_clean, family=binomial)

# Removing review_scores_accuracy <- not significant. 
glm.fit = glm(host_is_superhost~ host_listings_count+number_of_reviews+
              host_acceptance_rate+review_scores_rating+review_scores_checkin+
              review_scores_communication+review_scores_location+
              review_scores_cleanliness, 
              data = df_airbnb_clean, family="binomial")

summary(glm.fit)

# Training the Logistic Regression Model

# Splitting observations in two halves
train = sample(dim(df_airbnb_clean)[1], dim(df_airbnb_clean)[1]/2)

glm.fit = glm(host_is_superhost ~ host_listings_count+number_of_reviews+
              host_acceptance_rate+review_scores_rating+review_scores_checkin+
              review_scores_communication+review_scores_location+
              review_scores_cleanliness, 
              subset = train, family=binomial)

glm.probs = predict(glm.fit, newdata = df_airbnb_clean[-train,], type="response")

glm.pred = rep("f", length(glm.probs))
glm.pred[glm.probs > 0.5] = "t" # Setting cutoff point to 0.5.

# Confusion Matrix to show accuracy of logistic regression model:
table(glm.pred, df_airbnb_clean[-train,]$host_is_superhost)

# Validation (Testing) Set Error <- 
mean(glm.pred != df_airbnb_clean[-train,]$host_is_superhost)



# Creating Training and Test Samples - Alternative Method
# set.seed(1)

# sample = sample(c(TRUE,FALSE), nrow(df_airbnb_clean), replace=TRUE, prob=c(0.7,0.3))
# train = df_airbnb_clean[sample,] # Creating training data
# test = df_airbnb_clean[!sample,] # Creating test data

# K-Means Clustering - Round 1

set.seed(23)

bnb.bor = dfBnbNum$neighbourhood_group_cleansed
table(bnb.bor)

# Creating a new dataframe without the boroughs column. 
bnb_data = dfBnbNum[, -which(names(dfBnbNum) == "neighbourhood_group_cleansed")]

# Scaling bnb_data
bnb_data_scale = scale(bnb_data)

# Calculating Euclidean Distance
bnb_data = dist(bnb_data_scale) # dist function defaults to Euclidean

# Calculating Number of clusters to use for k-means clustering
fviz_nbclust(bnb_data_scale, kmeans, method="wss")+labs(subtitle="Elbow Method")

# Although the optimal number of clusters to use as shown in the elbow chart is
# 6, we wil use 5 since there are 5 boroughs in NY. 

# K-Means Implementation
km_bnb = kmeans(bnb_data_scale, centers=5, nstart=1000)
print(km_bnb)

# K-Means Algorithm Visualization
km_bnb.clusters = km_bnb$cluster


rownames(bnb_data_scale) = paste(dfBnbNum$neighbourhood_group_cleansed,
                                 1:dim(dfBnbNum)[1], sep="-")

fviz_cluster(list(data=bnb_data_scale, cluster=km_bnb.clusters), ellipse.alpha=0.1, labelsize = 10, axes=c(1,2), repel = TRUE,
            show.clust.center = TRUE, geom = "point")

# Better Understanding of the K-Means Visualization
table(km_bnb.clusters, dfBnbNum$neighbourhood_group_cleansed)
# We can see that a lot of airbnb property listings have been clustered incorrectly...

# K-Means Clustering - Round 2

set.seed(45)

bnb.bor = dfBnbNum$neighbourhood_group_cleansed
table(bnb.bor)

# Creating a new dataframe without the boroughs column. 
bnb_data = dfBnbNum[, -which(names(dfBnbNum) == "neighbourhood_group_cleansed")]

# Scaling bnb_data
bnb_data_scale = scale(bnb_data)

# Calculating Distance
bnb_data = dist(bnb_data_scale)

# K-Means Implementation
km_bnb = kmeans(bnb_data_scale, centers=6, nstart=100) # Now trying optimal 6 clusters

# K-Means Algorithm Visualization
km_bnb.clusters = km_bnb$cluster


rownames(bnb_data_scale) = paste(dfBnbNum$neighbourhood_group_cleansed,
                                 1:dim(dfBnbNum)[1], sep="-")

fviz_cluster(list(data=bnb_data_scale, cluster=km_bnb.clusters), ellipse.alpha=0.1, labelsize = 10, axes=c(1,2), repel = TRUE,
            show.clust.center = TRUE, geom="point")

table(km_bnb.clusters, dfBnbNum$neighbourhood_group_cleansed)

# K-Means Clustering  - Round 3

set.seed(3920)

bnb.bor = dfBnbNum$neighbourhood_group_cleansed
table(bnb.bor)

# Creating a new dataframe without the boroughs column. 
bnb_data = dfBnbNum[, -which(names(dfBnbNum) == "neighbourhood_group_cleansed")]

# Scaling bnb_data
bnb_data_scale = scale(bnb_data)

# Calculating Distance
bnb_data = dist(bnb_data_scale)

# K-Means Implementation
km_bnb = kmeans(bnb_data_scale, centers=8, nstart=100) # Now trying 8 clusters

# K-Means Algorithm Visualization
km_bnb.clusters = km_bnb$cluster


rownames(bnb_data_scale) = paste(dfBnbNum$neighbourhood_group_cleansed,
                                 1:dim(dfBnbNum)[1], sep="-")

fviz_cluster(list(data=bnb_data_scale, cluster=km_bnb.clusters), ellipse.alpha=0.1, labelsize = 10, axes=c(1,2), repel = TRUE,
            show.clust.center = TRUE)

table(km_bnb.clusters, dfBnbNum$neighbourhood_group_cleansed)


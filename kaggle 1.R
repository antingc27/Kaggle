install.packages('ISLR');install.packages('ggplot2'); install.packages('caret'); install.packages('caTools'); install.packages('stringr'); install.packages('plyr'); install.packages('gbm')
library(lattice)
library(caret)
library(ggplot2)
library(lattice)
library(caTools)
library(stringr)
library(plyr)
library(gbm)

setwd("/Users/antingc/Documents/")
data = read.csv('analysisData.csv')
scoringData = read.csv('scoringData.csv')
nrow(data)
str(data)
colnames(data)


# find names of columns which contain missing values
colnames(data)[colSums(is.na(data)) > 0]

# find names of rows which contain missing values
rownames(data)[rowSums(is.na(data)) > 0]

mean_square_feet1 = mean(data[,"square_feet"], na.rm = TRUE) # average square feet 
na_square_feet1 = is.na(data[,"square_feet"]) # the location of NA in the column of average square feet
data[na_square_feet1, "square_feet"] = mean_square_feet1

mean_security_deposit1= mean(data[,"security_deposit"], na.rm = TRUE)
na_security_deposit1 = is.na(data[,"security_deposit"])
data[na_security_deposit1, "security_deposit"] = mean_security_deposit1
mean_security_deposit

cleaning_fee_cleaned <- replace(data$cleaning_fee,is.na(data$cleaning_fee),"0")
cleaning_fee_cleaned <- as.integer(cleaning_fee_cleaned)
data$cleaning_fee <- cleaning_fee_cleaned

#mean_cleaing_fee1= mean(data[,"cleaning_fee"], na.rm = TRUE)
#na_cleaning_fee1= is.na(data[,"cleaning_fee"])
#data[na_cleaning_fee1, "cleaning_fee"] = mean_cleaing_fee1

mean_reviews_per_month1 = mean(data[,"reviews_per_month"], na.rm = TRUE) # average reviews per month 
na_reviews_per_month1 = is.na(data[,"reviews_per_month"]) # the location of NA in the column of reviews per month
data[na_reviews_per_month1, "reviews_per_month"] = mean_reviews_per_month1
mean_reviews_per_month

mean_beds1 = mean(data[,"beds"], na.rm = TRUE) # average reviews per month 
na_beds1 = is.na(data[,"beds"]) # the location of NA in the column of reviews per month
data[na_beds1, "reviews_per_month"] = mean_beds1
mean_reviews_per_month


data$amenities <- as.character(data$amenities)
data$amenities <- sapply(strsplit(data$amenities, ","), length)
data$amenities <- as.numeric(data$amenities)

adata$summary <- as.character(data$summary)
data$summary <- sapply(strsplit(data$summary, " "), length)

data$name <- as.numeric(str_detect(data$name, 'Luxury'))

data$zipcode<- as.numeric(data$zipcode)
scoringData$zipcode<- as.numeric(scoringData$zipcode)

data$room_type <- as.factor(data$room_type)

data$cancellation_policy <- as.factor(data$cancellation_policy)
data$host_is_superhost <- as.factor(data$host_is_superhost)
data$host_has_profile_pic <- as.factor(data$host_has_profile_pic)

# replace the NA value in average square_feet in scoringData with the average value
mean_square_feet = mean(scoringData[,"square_feet"], na.rm = TRUE) # average square feet 
na_square_feet = is.na(scoringData[,"square_feet"]) # the location of NA in the column of average square feet
scoringData[na_square_feet, "square_feet"] = mean_square_feet
mean_square_feet

mean_security_deposit= mean(scoringData[,"security_deposit"], na.rm = TRUE)
na_security_deposit = is.na(scoringData[,"security_deposit"])
scoringData[na_security_deposit, "security_deposit"] = mean_security_deposit
mean_security_deposit

cleaning_fee_cleaned <- replace(scoringData$cleaning_fee,is.na(scoringData$cleaning_fee),"0")
cleaning_fee_cleaned <- as.integer(cleaning_fee_cleaned)
scoringData$cleaning_fee <- cleaning_fee_cleaned

#mean_cleaing_fee= mean(scoringData[,"cleaning_fee"], na.rm = TRUE)
#na_cleaning_fee= is.na(scoringData[,"cleaning_fee"])
#scoringData[na_cleaning_fee, "cleaning_fee"] = mean_cleaing_fee

mean_reviews_per_month = mean(scoringData[,"reviews_per_month"], na.rm = TRUE) # average reviews per month 
na_reviews_per_month = is.na(scoringData[,"reviews_per_month"]) # the location of NA in the column of reviews per month
scoringData[na_reviews_per_month, "reviews_per_month"] = mean_reviews_per_month
mean_reviews_per_month

mean_beds = mean(scoringData[,"beds"], na.rm = TRUE) # average reviews per month 
na_beds = is.na(scoringData[,"beds"]) # the location of NA in the column of reviews per month
scoringData[na_beds, "beds"] = mean_beds

scoringData$amenities <- as.character(scoringData$amenities)
scoringData$amenities <- sapply(strsplit(scoringData$amenities, ","), length)
scoringData$amenities <- as.numeric(scoringData$amenities)

scoringData$summary <- as.character(scoringData$summary)
scoringData$summary <- sapply(strsplit(scoringData$summary, " "), length)

scoringData$name <- as.numeric(str_detect(scoringData$name, "Luxury"))

scoringData$room_type <- as.factor(scoringData$room_type)

scoringData$cancellation_policy <- as.factor(scoringData$cancellation_policy)
scoringData$host_is_superhost <- as.factor(scoringData$host_is_superhost)
scoringData$host_has_profile_pic <- as.factor(scoringData$host_has_profile_pic)

scoringData$property_type[scoringData$property_type=='Hut'] <- 'Other'
scoringData$property_type[scoringData$property_type=='Cottage'] <- 'Other'
scoringData$neighbourhood_cleansed[scoringData$neighbourhood_cleansed=='Hollis Hills'] <- 'Jamaica'
scoringData$neighbourhood_cleansed[scoringData$neighbourhood_cleansed=='Westerleigh'] <- 'Castleton Corners'
scoringData$calendar_updated[scoringData$calendar_updated=='46 months ago'] <- '37 months ago'
scoringData$calendar_updated[scoringData$calendar_updated=='47 months ago'] <- '37 months ago'
scoringData$calendar_updated[scoringData$calendar_updated=='60 months ago'] <- '37 months ago'


#data$security_deposit[is.na(data$security_deposit)]<-0
#scoringData$security_deposit[is.na(scoringData$security_deposit)]<-0
#data$cleaning_fee[is.na(data$cleaning_fee)]<-0
#scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)]<-0
data$host_is_superhost <- as.factor(data$host_is_superhost)
scoringData$host_is_superhost <- as.factor(scoringData$host_is_superhost)
scoringData$host_is_superhost<-factor(scoringData$host_is_superhost,levels=levels(data$host_is_superhost))
data$host_has_profile_pic <- as.factor(data$host_has_profile_pic)
scoringData$host_has_profile_pic <- as.factor(scoringData$host_has_profile_pic)
scoringData$host_has_profile_pic<-factor(scoringData$host_has_profile_pic,levels=levels(data$host_has_profile_pic))
data$host_identity_verified <- as.factor(data$host_identity_verified)
scoringData$host_identity_verified <- as.factor(scoringData$host_identity_verified)
scoringData$host_identity_verified<-factor(scoringData$host_identity_verified,levels=levels(data$host_identity_verified))
data$is_location_exact <- as.factor(data$is_location_exact)
scoringData$is_location_exact <- as.factor(scoringData$is_location_exact)
scoringData$is_location_exact<-factor(scoringData$is_location_exact,levels=levels(data$is_location_exact))
data$property_type <- as.factor(data$property_type)
scoringData$property_type <- as.factor(scoringData$property_type)
scoringData$property_type<-factor(scoringData$property_type,levels=levels(data$property_type))

model40=lm(price~zipcode+id+name+summary+summary+cleaning_fee+amenities+name+is_location_exact+host_identity_verified+calendar_updated+host_has_profile_pic+host_response_rate+host_is_superhost+cancellation_policy+property_type+beds+bed_type+neighbourhood_cleansed+square_feet+security_deposit+calculated_host_listings_count+require_guest_phone_verification+require_guest_profile_picture+is_business_travel_ready+instant_bookable+room_type+host_id+host_listings_count+latitude+longitude+accommodates+bathrooms+bedrooms+guests_included+extra_people+minimum_nights+maximum_nights
           +number_of_reviews+review_scores_rating+availability_30+availability_60+availability_90+availability_365
           +review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value
           ,data)
pred40=predict(model40,newdata=scoringData)
rmse40=sqrt(mean((model40$residuals)^2));rmse40
submissionFile = data.frame(id = scoringData$id, price = pred25)
write.csv(submissionFile, 'sample_submission_1130.csv',row.names = F)

model41=gbm(price~name+summary+neighbourhood_group_cleansed+summary+cleaning_fee+amenities+name+is_location_exact+host_identity_verified+calendar_updated+host_has_profile_pic+host_response_rate+id+host_is_superhost+cancellation_policy+property_type+beds+bed_type+neighbourhood_cleansed+square_feet+security_deposit+calculated_host_listings_count+require_guest_phone_verification+require_guest_profile_picture+is_business_travel_ready+instant_bookable+room_type+host_id+host_listings_count+latitude+longitude+accommodates+bathrooms+bedrooms+guests_included+extra_people+minimum_nights+maximum_nights
            +number_of_reviews+review_scores_rating+availability_30+availability_60+availability_90+availability_365
            +review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value
            ,data=data,distribution = "gaussian",n.trees = 1000,interaction.depth = 1,shrinkage = 0.04)
pred41=predict(model41,newdata=scoringData, n.trees=1000)
submissionFile = data.frame(id = scoringData$id, price = pred41)
write.csv(submissionFile, 'sample_submission_112701.csv',row.names = F)

model6 = gbm(price~host_has_profile_pic+name+neighbourhood_group_cleansed+security_deposit+summary+amenities+host_is_superhost+square_feet+reviews_per_month+property_type+host_listings_count+zipcode+latitude+longitude+room_type+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_location+review_scores_value+accommodates+bathrooms+bedrooms+beds+cleaning_fee+minimum_nights+extra_people+cancellation_policy+reviews_per_month+availability_30+availability_60+availability_90+availability_365
             +review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value, data=data,distribution = "gaussian", n.trees = 1000, interaction.depth = 1, shrinkage = 0.01)
pred=predict(model6,newdata = scoringData,n.trees = 1000)
summary(model6)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission_112702.csv',row.names = F)



model6 = lm(price~host_has_profile_pic+name+neighbourhood_group_cleansed+security_deposit+summary+amenities+host_is_superhost+square_feet+reviews_per_month+property_type+host_listings_count+zipcode+latitude+longitude+room_type+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_location+review_scores_value+accommodates+bathrooms+bedrooms+beds+cleaning_fee+minimum_nights+extra_people+cancellation_policy+reviews_per_month+availability_30, data)
pred6=predict(model6,newdata = scoringData)
rmse6=sqrt(mean((model6$residuals)^2));rmse6
summary(model6)

# For the following code to work, ensure analysisData.csv and scoringData.csv are in your working directory.
library(rpart)
library(gbm)

## INITIAL EXPLORATION STARTS HERE
setwd('/Users/chenningz/Desktop/predictlala2020')
# Read data and construct a simple model
data = read.csv('analysisData.csv')
str(data)
## INITIAL EXPLORATION ENDS HERE



## DATA CLEANING STARTS HERE
data$bedrooms[is.na(data$bedrooms)] <- mean(data$bedrooms, na.rm = T)
data$guests_included[is.na(data$guests_included)] <- mean(data$guests_included, na.rm = T)
data$bathrooms[is.na(data$bathrooms)] <- mean(data$bathrooms, na.rm = T)
#data$review_scores_accuracy[is.na(data$data$review_scores_accuracy)] <- mean(data$data$review_scores_accuracy, na.rm = T)
#data$minimum_nights[is.na(data$data$minimum_nights)] <- mean(data$data$minimum_nights, na.rm = T)
data$beds[is.na(data$beds)] <- mean(data$beds, na.rm = T)
data$accommodates[is.na(data$accommodates)] <- mean(data$accommodates, na.rm = T)
data$square_feet[is.na(data$square_feet)] <- mean(data$square_feet, na.rm = T)
data$monthly_price[is.na(data$monthly_price)] <- mean(data$monthly_price, na.rm = T)
data$weekly_price[is.na(data$weekly_price)] <- mean(data$weekly_price, na.rm = T)
data$security_deposit[is.na(data$security_deposit)] <- mean(data$security_deposit, na.rm = T)
data$host_listings_count[is.na(data$host_listings_count)] <- mean(data$host_listings_count, na.rm = T)
data$host_total_listings_count[is.na(data$host_total_listings_count)] <- mean(data$host_total_listings_count, na.rm = T)
data$cleaning_fee[is.na(data$cleaning_fee)] <- mean(data$cleaning_fee, na.rm = T)
data$extra_people[is.na(data$extra_people)] <- mean(data$extra_people, na.rm = T)
data$minimum_nights[is.na(data$minimum_nights)] <- mean(data$minimum_nights, na.rm = T)
data$maximum_nights[is.na(data$maximum_nights)] <- mean(data$maximum_nights, na.rm = T)
data$minimum_minimum_nights[is.na(data$minimum_minimum_nights)] <- mean(data$minimum_minimum_nights, na.rm = T)
data$minimum_maximum_nights[is.na(data$minimum_maximum_nights)] <- mean(data$minimum_maximum_nights, na.rm = T)
data$maximum_maximum_nights[is.na(data$maximum_maximum_nights)] <- mean(data$maximum_maximum_nights, na.rm = T)
data$maximum_minimum_nights[is.na(data$maximum_minimum_nights)] <- mean(data$maximum_minimum_nights, na.rm = T)
data$reviews_per_month[is.na(data$reviews_per_month)] <- mean(data$reviews_per_month, na.rm = T)
data$calculated_host_listings_count_shared_rooms[is.na(data$calculated_host_listings_count_shared_rooms)] <- mean(data$calculated_host_listings_count_shared_rooms, na.rm = T)
data$calculated_host_listings_count_entire_homes[is.na(data$calculated_host_listings_count_entire_homes)] <- mean(data$calculated_host_listings_count_entire_homes, na.rm = T)
data$review_scores_checkin[is.na(data$review_scores_checkin)] <- mean(data$review_scores_checkin, na.rm = T)
data$review_scores_cleanliness[is.na(data$review_scores_cleanliness)] <- mean(data$review_scores_cleanliness, na.rm = T)
data$review_scores_accuracy[is.na(data$review_scores_accuracy)] <- mean(data$review_scores_accuracy, na.rm = T)
data$review_scores_rating[is.na(data$review_scores_rating)] <- mean(data$review_scores_rating, na.rm = T)
#data$first_review[is.na(data$first_review)] <- mean(data$first_review, na.rm = T)
data$number_of_reviews[is.na(data$number_of_reviews)] <- mean(data$number_of_reviews, na.rm = T)
data$number_of_reviews_ltm[is.na(data$number_of_reviews_ltm)] <- mean(data$number_of_reviews_ltm, na.rm = T)
data$availability_30[is.na(data$availability_30)] <- mean(data$availability_30, na.rm = T)
data$availability_60[is.na(data$availability_60)] <- mean(data$availability_60, na.rm = T)
data$availability_90[is.na(data$availability_90)] <- mean(data$availability_90, na.rm = T)
data$availability_365[is.na(data$availability_365)] <- mean(data$availability_365, na.rm = T)
#data$has_availability[is.na(data$has_availability)] <- c('t')
data$minimum_nights_avg_ntm[is.na(data$minimum_nights_avg_ntm)] <- mean(data$minimum_nights_avg_ntm, na.rm = T)
data$maximum_nights_avg_ntm[is.na(data$maximum_nights_avg_ntm)] <- mean(data$maximum_nights_avg_ntm, na.rm = T)

# Read scoring data and apply model to generate predictions
scoringData = read.csv('scoringData.csv')
scoringData$bedrooms[is.na(scoringData$bedrooms)] <- mean(scoringData$bedrooms, na.rm = T)
scoringData$guests_included[is.na(scoringData$guests_included)] <- mean(scoringData$guests_included, na.rm = T)
scoringData$bathrooms[is.na(scoringData$bathrooms)] <- mean(scoringData$bathrooms, na.rm = T)
#data$review_scores_accuracy[is.na(data$data$review_scores_accuracy)] <- mean(data$data$review_scores_accuracy, na.rm = T)
#data$minimum_nights[is.na(data$data$minimum_nights)] <- mean(data$data$minimum_nights, na.rm = T)
scoringData$beds[is.na(scoringData$beds)] <- mean(scoringData$beds, na.rm = T)
scoringData$accommodates[is.na(scoringData$accommodates)] <- mean(scoringData$accommodates, na.rm = T)
scoringData$square_feet[is.na(scoringData$square_feet)] <- mean(scoringData$square_feet, na.rm = T)
scoringData$monthly_price[is.na(scoringData$monthly_price)] <- mean(scoringData$monthly_price, na.rm = T)
scoringData$weekly_price[is.na(scoringData$weekly_price)] <- mean(scoringData$weekly_price, na.rm = T)
scoringData$security_deposit[is.na(scoringData$security_deposit)] <- mean(scoringData$security_deposit, na.rm = T)
scoringData$host_listings_count[is.na(scoringData$host_listings_count)] <- mean(scoringData$host_listings_count, na.rm = T)
scoringData$host_total_listings_count[is.na(scoringData$host_total_listings_count)] <- mean(scoringData$host_total_listings_count, na.rm = T)
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- mean(scoringData$cleaning_fee, na.rm = T)
scoringData$extra_people[is.na(scoringData$extra_people)] <- mean(scoringData$extra_people, na.rm = T)
scoringData$minimum_nights[is.na(scoringData$minimum_nights)] <- mean(scoringData$minimum_nights, na.rm = T)
scoringData$maximum_nights[is.na(scoringData$maximum_nights)] <- mean(scoringData$maximum_nights, na.rm = T)
scoringData$minimum_minimum_nights[is.na(scoringData$minimum_minimum_nights)] <- mean(scoringData$minimum_minimum_nights, na.rm = T)
scoringData$minimum_maximum_nights[is.na(scoringData$minimum_maximum_nights)] <- mean(scoringData$minimum_maximum_nights, na.rm = T)
scoringData$maximum_maximum_nights[is.na(scoringData$maximum_maximum_nights)] <- mean(scoringData$maximum_maximum_nights, na.rm = T)
scoringData$maximum_minimum_nights[is.na(scoringData$maximum_minimum_nights)] <- mean(scoringData$maximum_minimum_nights, na.rm = T)
scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)] <- mean(scoringData$reviews_per_month, na.rm = T)
scoringData$calculated_host_listings_count_shared_rooms[is.na(scoringData$calculated_host_listings_count_shared_rooms)] <- mean(scoringData$calculated_host_listings_count_shared_rooms, na.rm = T)
scoringData$calculated_host_listings_count_entire_homes[is.na(scoringData$calculated_host_listings_count_entire_homes)] <- mean(scoringData$calculated_host_listings_count_entire_homes, na.rm = T)
scoringData$review_scores_checkin[is.na(scoringData$review_scores_checkin)] <- mean(scoringData$review_scores_checkin, na.rm = T)
scoringData$review_scores_cleanliness[is.na(scoringData$review_scores_cleanliness)] <- mean(scoringData$review_scores_cleanliness, na.rm = T)
scoringData$review_scores_accuracy[is.na(scoringData$review_scores_accuracy)] <- mean(scoringData$review_scores_accuracy, na.rm = T)
scoringData$review_scores_rating[is.na(scoringData$review_scores_rating)] <- mean(scoringData$review_scores_rating, na.rm = T)
#scoringData$first_review[is.na(scoringData$first_review)] <- mean(scoringData$first_review, na.rm = T)
scoringData$number_of_reviews[is.na(scoringData$number_of_reviews)] <- mean(scoringData$number_of_reviews, na.rm = T)
scoringData$number_of_reviews_ltm[is.na(scoringData$number_of_reviews_ltm)] <- mean(scoringData$number_of_reviews_ltm, na.rm = T)
scoringData$availability_30[is.na(scoringData$availability_30)] <- mean(scoringData$availability_30, na.rm = T)
scoringData$availability_60[is.na(scoringData$availability_60)] <- mean(scoringData$availability_60, na.rm = T)
scoringData$availability_90[is.na(scoringData$availability_90)] <- mean(scoringData$availability_90, na.rm = T)
scoringData$availability_365[is.na(scoringData$availability_365)] <- mean(scoringData$availability_365, na.rm = T)
#scoringData$has_availability[is.na(data$has_availability)] <- c('t')
scoringData$minimum_nights_avg_ntm[is.na(scoringData$minimum_nights_avg_ntm)] <- mean(scoringData$minimum_nights_avg_ntm, na.rm = T)
scoringData$maximum_nights_avg_ntm[is.na(scoringData$maximum_nights_avg_ntm)] <- mean(scoringData$maximum_nights_avg_ntm, na.rm = T)
## DATA CLEANING ENDS HERE




## MODELING AND FEATURE SELECTION STARTS HERE
## FEATURE SELECTION 1: SUBSET FORWARD SEARCH STARTS HERE
start_mod = lm(price~1,data=data)
empty_mod = lm(price~1,data=data)
full_mod = lm(price~ minimum_nights+review_scores_accuracy+bedrooms+bathrooms+guests_included+city+property_type,data=data)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
## FEATURE SELECTION 1: SUBSET FORWARD SEARCH ENDS HERE

## MODEL 1: LINEAR REGRESSION WITH FEATURES FROM FEATURE SELECTION 1 STARTS HERE
model = lm(price~minimum_nights+review_scores_accuracy,data)
## MODEL 1: LINEAR REGRESSION WITH FEATURES FROM FEATURE SELECTION 1 ENDS HERE

## MODEL 2: LINEAR REGRESSION WITH MORE FEATURES STARTS HERE
model2 =lm(price ~ bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights+beds, data)
## MODEL 2: LINEAR REGRESSION WITH MORE FEATURES  ENDS HERE

## MODEL 3: DECISION TREE WITH FEATURES FROM FEATURE SELECTION MODEL2 STARTS HERE
model3_tree =  rpart(price ~ bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights+beds, data = data, method = 'anova')
## MODEL 3: DECISION TREE WITH FEATURES FROM FEATURE SELECTION MODEL2 ENDS HERE

## MODEL 4: BOOSTING WITH FEATURES FROM FEATURE SELECTION MODEL2 STARTS HERE
model4_boost = gbm(price ~ bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights+beds,
            data=data,
            distribution="gaussian",
            n.trees = 500,
            interaction.depth = 2,
            shrinkage = 0.01)
## MODEL 4: BOOSTING WITH FEATURES FROM FEATURE SELECTION MODEL2 ENDS HERE

## MODEL 5: RANDOM FOREST WITH MAX NUMBER FEATURES WE HAVE STARTS HERE
library(randomForest)
set.seed(617)
forest = randomForest(price ~ bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights+beds+ accommodates+ square_feet + weekly_price+ monthly_price + security_deposit+host_neighbourhood + host_listings_count+host_total_listings_count+ host_is_superhost + host_acceptance_rate+ host_response_time + host_response_rate +host_has_profile_pic+ host_identity_verified + is_location_exact+ host_since + host_name+ neighbourhood+ neighbourhood_cleansed+ neighbourhood_group_cleansed+city+ state+property_type+room_type+ bed_type+ cleaning_fee+extra_people+minimum_nights+maximum_nights+minimum_minimum_nights+maximum_minimum_nights+minimum_maximum_nights+minimum_nights_avg_ntm+maximum_nights_avg_ntm+has_availability+availability_30+availability_60+availability_90+availability_365+number_of_reviews+number_of_reviews_ltm+first_review+last_review+review_scores_rating+review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+instant_bookable+requires_license+is_business_travel_ready+cancellation_policy+require_guest_profile_picture+require_guest_phone_verification+calculated_host_listings_count+calculated_host_listings_count_entire_homes+calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms+reviews_per_month,data=data,ntree = 1000)
## MODEL 5: RANDOM FOREST WITH MAX NUMBER FEATURES WE HAVE ENDS HERE

## MODELING AND FEATURE SELECTION ENDS HERE


#pred = predict(model,newdata=scoringData)
#pred3 = predict(model3, newdata=scoringData)
#pred3_tree = predict(model3_tree, newdata = scoringData)
#pred3_boost = predict(boost, newdata = scoringData)
pred3_forest = predict(forest, newdata = scoringData)
# Construct submission from predictions
submissionFile3_forest = data.frame(id = scoringData$id, price = pred3_forest)
write.csv(submissionFile3_forest, 'sample_submission_forest.csv',row.names = F)
pred_train3 = predict(forest)
rmse2 = sqrt(mean((pred_train3 - data$price)^2));rmse2
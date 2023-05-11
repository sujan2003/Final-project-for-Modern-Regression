library(fastDummies)
library(corrplot)
library(leaps)
library(glmnet)
library(boot)
library(tree)
library(randomForest)

data <- read.csv("Final-project-for-Modern-Regression/Airbnb_Open_Data.csv")


# DATA CLEANING ################################################################

# Create dummy variables
data <- dummy_cols(data,
                   select_columns = c("host_identity_verified", 
                                      "neighbourhood.group",
                                      "cancellation_policy",
                                      "room.type"))

# Remove irrelevant and repeat columns
data <- data[, -c(1:14, 20, 25:27, 34, 38)]

# Rename columns to remove spaces
colnames(data)[18] = "neighbourhood.group_Staten_Island"
colnames(data)[22] = "room.type_Entire_home_apt"
colnames(data)[23] = "room.type_Hotel_room"
colnames(data)[24] = "room.type_Private_room"
colnames(data)[25] = "room.type_Shared_room"

# Remove dollar signs from data$price and data$service.fee
data[] <- lapply(data, gsub, pattern="$", fixed=TRUE, replacement="")

# Convert character types to numeric
char_to_num <- c(2:9)

for (i in char_to_num) {
  data[ , i] <- as.numeric(data[ , i])
}

# Remove rows with null values
data <- na.omit(data)

# Change review.rate.number to binary variable
colnames(data)[7] = "good_review"
data$good_review[data$good_review < 4] <- 0
data$good_review[data$good_review >= 4] <- 1

attach(data)


# DATA EXPLORATION #############################################################

# Find the correlation between only numeric features
numeric_features = data.frame(price, service.fee, minimum.nights, 
                              number.of.reviews, reviews.per.month, good_review, 
                              calculated.host.listings.count, availability.365)
num_correlations <- cor(numeric_features)
corrplot(num_correlations,
         title = "Correlation of Numeric Features",
         tl.col = "black",
         mar = c(0,0,2,0))


# MLR ##########################################################################

# Split data
train      <- 1:56108
test       <- 56109:70136
test2      <- data[test, ]
mlr.target <- price[test]

# Use Lasso to perform shrinkage and variable selection
x <- model.matrix(price ~ ., data = data)[, -1]
y <- data$price

mlr.lasso.mod <- glmnet(x[train, ], price[train], alpha = 1)
plot(mlr.lasso.mod)

set.seed(1)
mlr.cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
plot(mlr.cv.out)
mlr.bestlam <- mlr.cv.out$lambda.min

mlr.lasso.pred <- predict(mlr.lasso.mod, s = mlr.bestlam, newx = x[test, ])
mean((mlr.lasso.pred - mlr.target)^2)

mlr.out <- glmnet(x, y, alpha = 1)
mlr.lasso.coef <- predict(mlr.out, type = "coefficients", s = mlr.bestlam)[1:43, ]
mlr.lasso.coef[mlr.lasso.coef != 0]
# This works but our only feature != 0 is service.fee

# Attempting subset selection to verify Lasso results
mlr.regfit <- regsubsets(price ~ .,
                         data = data,
                         nbest = 1,
                         nvmax = 25)
mlr.regfit.summary <- summary(mlr.regfit)

par(mfrow = c(1, 3))
plot(mlr.regfit, main = "Variable Selection")
plot(mlr.regfit.summary$rss, main="# of Variables vs RSS", xlab="Number of Variables", ylab="RSS", type = "l", col = "red")
plot(mlr.regfit.summary$rsq, main="# of Variables vs RSq", xlab="Number of Variables", ylab="RSq", type = "l", col = "blue")
coef(mlr.regfit, 10)

# Fit MLR model
mlr.fit <- lm(price ~ service.fee,
              data = data)
par(mfrow = c(2,4))
plot(mlr.fit, main = "MLR Diagnostic Plot")
summary(mlr.fit)

# Choose random observation to predict
random <- floor(runif(1, min=1, max=70136)) # 51452
data[51452, ] # row for random index
data[51452, ]$price # goal to predict

# Create data frame to predict
df.predict <- data.frame(Construction.year=as.character(2017),
                         service.fee=63,
                         minimum.nights=1,
                         number.of.reviews=9,
                         reviews.per.month=2.84,
                         good_review=1,
                         calculated.host.listings.count=2,
                         availability.365=22,
                         host_identity_verified_unconfirmed=as.character(1),
                         host_identity_verified_verified=as.character(0),
                         neighbourhood.group_=as.character(0),
                         neighbourhood.group_Bronx=as.character(0),
                         neighbourhood.group_brookln=as.character(0),
                         neighbourhood.group_Brooklyn=as.character(1),
                         neighbourhood.group_Manhattan=as.character(0),
                         neighbourhood.group_Queens=as.character(0),
                         neighbourhood.group_Staten_Island=as.character(0),
                         cancellation_policy_flexible=as.character(1),
                         cancellation_policy_moderate=as.character(0),
                         cancellation_policy_strict=as.character(0),
                         room.type_Entire_home_apt=as.character(0),
                         room.type_Hotel_room=as.character(0),
                         room.type_Private_room=as.character(1),
                         room.type_Shared_room=as.character(0)
)

# Predict price of specific observation
predict(mlr.fit, df.predict) # 314.9686
predict(mlr.fit, df.predict, interval = "confidence")
predict(mlr.fit, df.predict, interval = "prediction")

# Find model MSE
mlr.fit.pred <- predict(mlr.fit, test2)
mean((mlr.fit.pred - mlr.target)^2) # 2.0148

# Predict price using decision tree
tree.mlr <- tree(price ~ .,
                 data,
                 subset = train)
predict(tree.mlr, df.predict) # 352.8062
tree.mlr.pred <- predict(tree.mlr, test2)
mean((tree.mlr.pred - mlr.target)^2) # 1184.596


# CLASSIFICATION ###############################################################

lr.target <- good_review[56109:70136]

# Use Lasso to perform shrinkage and variable selection
x <- model.matrix(good_review ~ ., data = data)[, -1]
y <- data$good_review

lr.lasso.mod <- glmnet(x[train, ], good_review[train], alpha = 1)
plot(lr.lasso.mod, main = "Lasso Shrinkage for Logistic Regression")

set.seed(1)
lr.cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
plot(lr.cv.out)
lr.bestlam <- lr.cv.out$lambda.min

lr.lasso.pred <- predict(lr.lasso.mod, s = lr.bestlam, newx = x[test, ])
mean((lr.lasso.pred - lr.target)^2)

lr.out <- glmnet(x, y, alpha = 1)
lr.lasso.coef <- predict(lr.out, type = "coefficients", s = lr.bestlam)[1:43, ]
lr.predictors <- names(lr.lasso.coef[lr.lasso.coef != 0])
lr.predictors

# Fit model
lr.fit <- glm(good_review ~ Construction.year + number.of.reviews + 
                reviews.per.month + calculated.host.listings.count +
                availability.365 + neighbourhood.group_brookln +
                neighbourhood.group_Queens + 
                neighbourhood.group_Staten_Island + 
                cancellation_policy_moderate + room.type_Private_room,
              data = data,
              family = "binomial",
              subset = train)
summary(lr.fit)
plot(lr.fit, main = "LR Diagnostic Plot")

# Make predictions using logistic regression
lr.prob <- predict(lr.fit, test2, type = "response")

lr.pred <- rep(0, 14028)
lr.pred[lr.prob > .5] <- 1

table(lr.pred, lr.target)
mean(lr.pred == lr.target) # 0.5507556

# Predict good_review using decision tree
rf.lr <- randomForest(good_review ~ Construction.year + number.of.reviews + 
                        reviews.per.month + calculated.host.listings.count +
                        availability.365 + neighbourhood.group_brookln +
                        neighbourhood.group_Queens + 
                        neighbourhood.group_Staten_Island + 
                        cancellation_policy_moderate + room.type_Private_room,
                      data = data,
                      subset = train,
                      importance = TRUE)
rf.lr.prob <- predict(rf.lr, test2, type = "class")
rf.lr.pred <- rep(0, 14028)
rf.lr.pred[rf.lr.prob > .5] <- 1
table(rf.lr.pred, lr.target)
mean(rf.lr.pred == lr.target) # 0.8952096
varImpPlot(rf.lr)
mean(lr.pred == lr.target)



###########SVM####################
library(e1071)
library(caret)
library(tidyverse)

# Create binary variable for good reviews
data$good_review <- ifelse(data$review_scores_rating >= 3, 1, 0)


# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$good_review, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train SVM model
svm_model <- svm(good_review ~ ., data = train_data, kernel = "linear", cost = 1, scale = FALSE)

# Predict on test set
svm_preds <- predict(svm_model, test_data)

# Evaluate performance
confusionMatrix(svm_preds, test_data$good_review)

# Data Analysis on good_review and construction year
test_test <- table(good_review,Construction.year)
prop.table(test_test,1)  
barplot(prop.table(test_test, 2))




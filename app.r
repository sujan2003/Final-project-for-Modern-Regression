# install.packages("fastDummies")
library(fastDummies)

data <- read.csv("Final-project-for-Modern-Regression/Airbnb_Open_Data.csv")

dim(data)
names(data)


# DATA CLEANING ###############################################################

# Create dummy variables
data <- dummy_cols(data,
                   select_columns = c("host_identity_verified", 
                                      "neighbourhood.group",
                                      "cancellation_policy",
                                      "room.type"))
dim(data)

# Remove irrelevant and repeat columns
data <- data[, -c(1:14, 20, 25:27, 34, 38)]
dim(data)
head(data) 

# Rename columns to remove spaces
colnames(data)[18] = "neighbourhood.group_Staten_Island"
colnames(data)[22] = "room.type_Entire_home_apt"
colnames(data)[23] = "room.type_Hotel_room"
colnames(data)[24] = "room.type_Private_room"
colnames(data)[25] = "room.type_Shared_room"

# Remove dollar signs from data$price and data$service.fee
data[] <- lapply(data, gsub, pattern="$", fixed=TRUE, replacement="")
head(data)

# Convert character types to numeric
char_to_num <- c(2:9)

for (i in char_to_num) {
  data[ , i] <- as.numeric(data[ , i])
}

# Remove rows with null values
sum(is.na(data))
data <- na.omit(data)
sum(is.na(data))

dim(data)

attach(data)


# MLR #########################################################################

# Fit MLR model
mlr.fit <- lm(price ~ .,
              data = data)

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
                         review.rate.number=4,
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


# CLASSIFICATION ##############################################################

# Split data
train  <- 1:56108
test   <- data[56109:70136, ]
target <- review.rate.number[56109:70136]

# Fit model
lr.fit <- glm(review.rate.number ~ .,
              data = data,
              family = "binomial",
              subset = train)

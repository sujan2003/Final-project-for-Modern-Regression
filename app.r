# install.packages("fastDummies")
library(fastDummies)

data <- read.csv("Final-project-for-Modern-Regression/Airbnb_Open_Data.csv")

dim(data)
names(data)


# DATA CLEANING ###############################################################
# create dummy variables
data <- dummy_cols(data,
                   select_columns = c("host_identity_verified", 
                                      "neighbourhood.group",
                                      "cancellation_policy",
                                      "room.type"))
dim(data)

# remove irrelevant and repeat columns
data <- data[, -c(1:14, 20, 25:27, 34, 38)]
dim(data)
head(data) 

# remove dollar signs from data$price and data$service.fee
data[] <- lapply(data, gsub, pattern="$", fixed=TRUE, replacement="")
head(data)

# convert character types to numeric
char_to_num <- c(2:9)

for (i in char_to_num) {
  data[ , i] <- as.numeric(data[ , i])
}

# remove rows with null values
sum(is.na(data))
data <- na.omit(data)
sum(is.na(data))

attach(data)


# RUN MODELS ##################################################################

# Testing to run MLR
mlr.fit <- lm(price ~ .,
              data = data)

# install.packages("fastDummies")
library(fastDummies)

data <- read.csv("data/Airbnb_Open_Data.csv")

dim(data)
names(data)

# remove null rows
sum(is.na(data))
data <- na.omit(data)
sum(is.na(data))

# create dummy variables
data <- dummy_cols(data,
                   select_columns = c("host_identity_verified", 
                                      "neighbourhood.group",
                                      "cancellation_policy",
                                      "room.type"))

# remove irrelevant and repeat columns
data <- data[, -c(2:14, 20, 25:27)]
dim(data)
names(data)                   

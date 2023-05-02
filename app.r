data <- read.csv("Airbnb_Open_Data.csv")

dim(data)
names(data)

# remove irrelevant columns
data <- data[, -c(2, 3, 5, 7, 8, 9, 10, 11, 20, 25, 26)]
names(data)

# remove null rows
sum(is.na(data))
data <- na.omit(data)
sum(is.na(data))

attach(data)
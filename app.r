data <- read.csv("Airbnb_Open_Data.csv")

# remove irrelevant columns
data <- data[]

attach(data)

glm.fit <- glm(price ~ id + host_identity_verified + neighbourhood.group + instant_bookable + cancellation_policy
               + room.type + Construction.year + service.fee + minimum.nights + number.of.reviews + reviews.per.month
               + review.rate.number + calculated.host.listings.count + availability.365, 
               data = data, family = binomial)

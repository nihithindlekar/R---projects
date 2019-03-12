data <- read.csv("Ames_data.csv")
load("project1_testIDs.R")

j <- 1
test.dat <- data[testIDs[,j], ]
train.dat <- data[-testIDs[,j], ]


remove.var <- c('PID', 'Sale_Price')
train.y <- log(train.dat$Sale_Price)
train.x <- train.dat[ , !(names(train.dat) %in% remove.var)]
test.y <- log(test.dat$Sale_Price)
test.PID <- test.dat$PID
test.x <- test.dat[ , !(names(test.dat) %in% remove.var)]

#Set cat
train.x$Mo_Sold <- as.factor(train.x$Mo_Sold)
train.x$Year_Sold <- as.factor(train.x$Year_Sold)
test.x$Mo_Sold <- as.factor(test.x$Mo_Sold)
test.x$Year_Sold <- as.factor(test.x$Year_Sold)

#remove NaN
train.x[is.na(train.x)] <- 0
test.x[is.na(test.x)] <- 0

#dim(train.x)
#dim(test.x)
#summary(test.x$Garage_Yr_Blt)

train.x <- model.matrix(~., train.x)[, -1]
test.x <- model.matrix(~., test.x)[, -1]

install.packages("xgboost")
set.seed(100)
xgb.model <- xgboost(data = train.x, label = train.y, verbose=1, nrounds=20)
tmp <- predict(xgb.model, test.x)
sqrt(mean((tmp - test.y)^2))



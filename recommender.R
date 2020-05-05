



library(recommenderlab)
library(data.table)

# read in the data
data <- fread('rec_dataset_NA.csv')

data <- round(data)

data[ , cust_id := paste('cum', cust_id, sep = "_")]


# data[data>0]=1
# transfer the data to object required by recommenderlab package
train <- as.matrix(data[1:1420, ])

# train <- as.matrix(data)


test <- as.matrix(data[1421:3079, ])
train.ratingMatrix <- as(train, "realRatingMatrix")
test.ratingMatrix <- as(test, "realRatingMatrix")

# apply corss-validation method to check which method is the best
# model.eval <- evaluationScheme(train.ratingMatrix, method = "split", train = 0.9, given = 15, goodRating = 5)

model.eval <- evaluationScheme(train.ratingMatrix, method = "cross-validation", k=10, given = 15, goodRating = 5)

# build 3 different methods
model.random <- Recommender(getData(model.eval, "train"), method = "RANDOM")
model.ubcf <- Recommender(getData(model.eval, "train"), method = "UBCF")
model.ibcf <- Recommender(getData(model.eval, "train"), method = "IBCF")

# predict based on 3 methods
predict.random <- predict(model.random, getData(model.eval, "known"), type = "ratings")
predict.ubcf <- predict(model.ubcf, getData(model.eval, "known"), type = "ratings")
predict.ibcf <- predict(model.ibcf, getData(model.eval, "known"), type = "ratings")

# caculate the error of 3 models
error <- rbind(
  calcPredictionAccuracy(predict.random, getData(model.eval, "unknown")),
  calcPredictionAccuracy(predict.ubcf, getData(model.eval, "unknown")),
  calcPredictionAccuracy(predict.ibcf, getData(model.eval, "unknown")))
rownames(error) <- c("RANDOM", "UBCF", "IBCF")

# print out the error
error

# build up the best model
recommModel <- Recommender(train.ratingMatrix, method = "IBCF")

# show the model
recommModel

# predict
predict <- predict(recommModel, test.ratingMatrix, type = "ratings")
predict 

# get the result
result <- round(as(predict, "matrix"))[ , 431:450]

# relabel the customers
data <- fread('rec_dataset_NA.csv')
result <- cbind(data$cust_id[1421:3079], result)
colnames(result)[1] <- 'cust_id'

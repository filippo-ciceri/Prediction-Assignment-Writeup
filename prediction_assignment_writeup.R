library(caret)
set.seed(2111)

training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
training2 <- training[,-grep('kurtosis|skewness|max|min|avg|var|stddev|total|amplitude', names(training))]
training2 <- training2[,-grep('X|user_name|timestamp|window', names(training))]
names(training2)

preproc <- preProcess(training2[,-49], method=c("center","scale"))
training_preproc <- predict(preproc,training2)
apply(training_preproc[,-49], FUN=mean, MARGIN=2)
apply(training_preproc[,-49], FUN=sd, MARGIN=2)

model <- train(classe ~ ., 
               method="rpart", 
               data=training_preproc,
               trControl = trainControl(method="cv", number=10))
model
predictions <- predict(model, training_preproc[,-49])
table(training$classe,predictions)
confusionMatrix(table(training$classe,predictions))

preproc_PCA <- prcomp(training2[,-49],rank=10)
summary(preproc_PCA)
training_PCA <- data.frame(preproc_PCA$x)
training_PCA <- cbind(training2[,49],training_PCA)
names(training_PCA)[1] <- 'classe'

object.size(training_preproc)
object.size(training_PCA)
str(training_PCA)

model <- train(classe ~ ., 
               method="gbm", 
               data=training_PCA,
               trControl = trainControl(method="cv", number=10),
               verbose=FALSE)
model
predictions <- predict(model, training_PCA[,-1])
table(training$classe,predictions)
confusionMatrix(table(training$classe,predictions))

model <- train(classe ~ ., 
               method="rf", 
               data=training_PCA,
               trControl = trainControl(method="cv", number=10),
               ntree=100)
model
predictions <- predict(model, training_PCA[,-1])
table(training$classe,predictions)
confusionMatrix(table(training$classe,predictions))

testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
testing_PCA <- predict(preproc_PCA, testing)
final <- predict(model, testing_PCA)
final



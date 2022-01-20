
p1 <- read.csv("C:\\Users\\DELL\\Downloads\\weather.csv")
p1
summary(p1)
plot(p1)
library(performance)
library(caTools)
library(rpart)
head(p1)
cat("no of missing value",sum(is.na(p1)))
x <- prop.table(table(p1$RainTomorrow))
b <- barplot(x,col="black", main = "Target calss proportion diagram")
set.seed(2)
split <- sample.split(p1, SplitRatio = 0.7)
split
train <- subset(p1, split="TRUE")
test <- subset(p1, split="FALSE")
train
test
Model <- glm(RainTomorrow ~.,data=train)
summary(Model)
pred <- predict(Model, test)
pred
plot(test$RainTomorrow,type = "l",lty=1.8,col="red")
lines(pred,type = "l",col="blue")
plot(pred,type = "l",lty=1.8,col="blue")
lines(test$RainTomorrow,type = "l",lty=1.8,col="red")
rmse <- sqrt(mean((pred-p1$RainTomorrow)^2))
rmse
cm <- table(Actual_value=train$RainTomorrow, Predicted_value=pred>0.5)
cm

(cm[[1,1]]+cm[[2,2]])/sum(cm)
library(rpart)
model2 <- rpart(RISK_MM ~.,data=train,method = "class")
plot(model2, uniform=TRUE, main=" Claassification Tree")
treePred <- predict(model2,test)
text(model2,all=TRUE,cex=1.0)
mean(treePred==test$RISK_MM)

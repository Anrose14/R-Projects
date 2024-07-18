
install.packages("mlbench")
library(mlbench)
#loading dataset
data("PimaIndiansDiabetes")

#summary of dataset
str(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)

#visualization
library(ggplot2)
ggplot(PimaIndiansDiabetes,aes(x=age))+geom_histogram(bins=20)

library(dplyr)
count=PimaIndiansDiabetes %>% filter(age==60) %>% summarise(count = n())
count

ggplot(PimaIndiansDiabetes,aes(x=insulin,y=glucose))+geom_point()

#null values
sum(is.na(PimaIndiansDiabetes))

#normalizing
PimaIndiansDiabetes$glucose = scale(PimaIndiansDiabetes$glucose)
PimaIndiansDiabetes$insulin = scale(PimaIndiansDiabetes$insulin)
PimaIndiansDiabetes$triceps = scale(PimaIndiansDiabetes$triceps)

#splitting
library(caret)
set.seed(123)
i=sample(nrow(PimaIndiansDiabetes),size=0.8*nrow(PimaIndiansDiabetes))
train=PimaIndiansDiabetes[i,]
test=PimaIndiansDiabetes[-i,]

#model training Logistic Regression
model=glm(diabetes~.,data=train,family="binomial")

#model evaluation
prediction=predict(model,newdata=test,type="response")
class=ifelse(prediction > 0.5,1,0)
names(class) <- NULL
class <- ifelse(class == 1, "pos", "neg")
confusionMatrix(table(class,test$diabetes))

#model training RandomForest
library(randomForest)
modelrf <- randomForest(diabetes ~ ., data = train)
prediction=predict(modelrf,newdata=test)
class=factor(prediction,levels=levels(test$diabetes))
names(class) <- NULL
confusionMatrix(table(class,test$diabetes))

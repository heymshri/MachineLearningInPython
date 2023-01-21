library(ggplot2)
library(dplyr)
library(tidyr)

iris<-read.csv("/content/Iris.csv")
head(iris)
summary(iris)
dim(iris)
str(iris)
names(iris)
attributes(iris)
var(iris$SepalLength)
sd(iris$SepalLength)

install.packages("corrplot")
library(corrplot)
iris2<- iris[,-6]
cor(iris2)
corrplot(cor(iris2), method="number", type="lower")
hist(iris$SepalLength)
plot(density(iris$SepalLength))

ggplot(iris, aes(Species, PetalLengthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Petal Length (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Petal Length Box Plot", x = "Species")

ggplot(iris, aes(Species, SepalLengthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Sepal Length (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Sepal Length Box Plot", x = "Species")

table(iris$Species)
barplot(table(iris$Species))


n <- nrow(iris)
ntrain <- round(n*0.6) #60% for training set
set.seed(333)
tindex <- sample(n,ntrain)
train_iris <- iris[tindex,] 
test_iris <- iris[-tindex,] 

newcol <- data.frame(isVersicolor=(train_iris$Species =="Iris-versicolor"))
train_iris <- cbind(train_iris,newcol)
str(train_iris)
tree_data <- train_iris[,2:6]
str(tree_data)

tree_data[, 'Species'] <- as.factor(tree_data[, 'Species'])
str(tree_data)

install.packages("tree")
library(tree)#loading the library
tree1 <- tree(Species~.,data = tree_data)

plot(tree1)
text(tree1)

prediction <- predict(tree1,newdata = test_iris,type = 'class')
table(prediction,test_iris$Species)
#if we check the correlation
(20+22+15)/60 #=0.95%

iris$Petal.Width.Cat <- cut(iris$PetalWidth, breaks = quantile(iris$PetalWidth, probs = seq(0, 1, 0.5)), include.lowest = TRUE)
levels(iris$Petal.Width.Cat) <- c("Below", "Above")
head(iris)

iris <- iris[,!(names(iris) %in% "PetalWidth")]
head(iris)

cont <- table(iris$Petal.Width.Cat, iris$Species)
cont

Xsqt <- chisq.test(cont)
Xsqt

x <- iris[iris$Species == "Iris-setosa", ]$SepalLength
y <- iris[iris$Species == "Iris-versicolor", ]$SepalLength
tt <- t.test(x, y, paired = FALSE, alternative = "two.sided", var.equal = FALSE)
tt


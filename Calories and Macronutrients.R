library(tidyverse)
library(ggplot2)
install.packages("corrplot")     
library(corrplot)
install.packages("ggcorrplot")     
library(ggcorrplot)
install.packages("caTools")     
library(caTools)
install.packages('car')
library(car)
install.packages("lmtest")
library (lmtest)
library(readr)

library("readxl")
CNM <- read_excel("/content/Calories and Macronutrients.xlsx")
head(CNM)

CNM <- as.data.frame(apply(CNM, 2, as.numeric))  
sapply(CNM, class)

summary(CNM)
dim(CNM)
sum(is.na(CNM))
colSums(sapply(CNM,is.na))

corrplot(cor(CNM), method = "number")


#Linear Regression
data <- CNM %>% select(-c(Year,`U.S. population, July 12`))

model0<-lm(Kilocalories ~ Carbohydrate, data = data)
summary(model0)
model1=lm(Kilocalories~Fiber,data = data)
summary(model1)
model2=lm(Kilocalories~Protein, data = data)
summary(model2)
model3=lm(Kilocalories~Fat, data = data)
summary(model3)
model4=lm(Kilocalories~`Saturated Fatty Acids`, data = data)
summary(model4)
model5=lm(Kilocalories~`Monounsaturated Fatty Acids`, data = data)
summary(model5)
model6=lm(Kilocalories~`Polyunsaturated Fatty Acids`, data = data)
summary(model6)
model7=lm(Kilocalories~Cholesterol, data = data)
summary(model7)

#Multiple Linear regression
multillm <- lm(Kilocalories ~ ., data = data)
summary(multillm)
plot(multillm)
anova(multillm)

#K-means Clustering
install.packages("factoextra")
library(factoextra)
set.seed(123)
km.res <- kmeans(data, 4, nstart = 25)
print(km.res)
aggregate(data, by=list(cluster=km.res$cluster), mean)
dd <- cbind(data, cluster = km.res$cluster)
head(dd)

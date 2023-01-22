library(ggplot2)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(repr)
install.packages("corrplot")
library(corrplot)
install.packages("Hmisc")
library(Hmisc)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("randomForest")
library(randomForest)
install.packages("forecast")
library(forecast)
install.packages("e1071")
library(e1071)


maliciouswebsite <- read_csv("C:/Users/dell/Downloads/malicious-and-benign-websites1.csv")
View(maliciouswebsite)

head(maliciouswebsite)
summary(maliciouswebsite)
sum(is.na(maliciouswebsite$Type))
dim(maliciouswebsite)
sum(is.na(maliciouswebsite))
cleaned <- na.omit(maliciouswebsite)
sum(is.na(cleaned))
colSums(sapply(maliciouswebsite,is.na))
num = select_if(maliciouswebsite, is.numeric)
colSums(sapply(num, is.na))
glimpse(maliciouswebsite)
cat = select_if(maliciouswebsite, is.factor)
dim(cat)
#exploration
table(maliciouswebsite$Type)

with(maliciouswebsite, table(Type, URL_LENGTH))

ggplot(maliciouswebsite, aes(x = Type)) + geom_bar(color='red ',fill = "pink")

ggplot(maliciouswebsite, aes(y = CHARSET, fill = Type)) + geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(hjust = 1))+scale_fill_gradient(low="blue", high="skyblue")

maliciouswebsite %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +facet_wrap(~ key, scales = "free") + geom_histogram()   #geom_density()

numvalue<- subset(num, select = -c(CONTENT_LENGTH, DNS_QUERY_TIMES))

corrplot(cor(numvalue), addCoef.col = 1, number.cex = 0.5)

# data preparation
filtered= maliciouswebsite[!complete.cases(maliciouswebsite),]

head(filtered)
nrow(filtered)
maliciouswebsite$DNS_QUERY_TIMES=impute(maliciouswebsite$DNS_QUERY_TIMES, 0)
maliciouswebsite$SERVER=impute(maliciouswebsite$SERVER, 'nginx')
maliciouswebsite$CONTENT_LENGTH=impute(maliciouswebsite$CONTENT_LENGTH, mean)
head(maliciouswebsite$CONTENT_LENGTH, 100)
boxplot(maliciouswebsite$URL_LENGTH, data=maliciouswebsite, xlab = 'URL LENGTH',col = 'red')

#handling outliers
capped = maliciouswebsite[,c("REMOTE_APP_PACKETS", "URL_LENGTH", "SOURCE_APP_PACKETS","REMOTE_APP_BYTES","SOURCE_APP_BYTES",
                                        "APP_BYTES", "REMOTE_IPS", "DIST_REMOTE_TCP_PORT","TCP_CONVERSATION_EXCHANGE")]

dfrest = maliciouswebsite[,-c(2,11,12,13,14,15,16,17,18)]
head(dfrest)

pointcap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

dfcapped = pointcap(capped)
summary(dfcapped)
combined = cbind(dfrest,dfcapped)

combined$REMOTE_APP_PACKETS=log(combined$REMOTE_APP_PACKETS+1)
combined$URL_LENGTH=log(combined$URL_LENGTH+1)
combined$SOURCE_APP_PACKETS=log(combined$SOURCE_APP_PACKETS+1)
combined$TCP_CONVERSATION_EXCHANGE=log(combined$TCP_CONVERSATION_EXCHANGE+1)
combined$DIST_REMOTE_TCP_PORT=log(combined$DIST_REMOTE_TCP_PORT+1)
combined$REMOTE_APP_BYTES=log(combined$REMOTE_APP_BYTES+1)
combined$SOURCE_APP_BYTES=log(combined$SOURCE_APP_BYTES+1)
combined$APP_BYTES=log(combined$APP_BYTES+1)
combined$REMOTE_IPS=log(combined$REMOTE_IPS+1)
combined$Type= as.factor(combined$Type)

#data training
indices = sample(1:nrow(combined), size=0.2*nrow(combined))
#spliting
xtst = combined[indices,]
dim(xtst) 

xtrain = combined[-indices,]
dim(xtrain)

capped$REMOTE_APP_PACKETS=log(capped$REMOTE_APP_PACKETS+1)
capped$URL_LENGTH=log(capped$URL_LENGTH+1)
capped$SOURCE_APP_PACKETS=log(capped$SOURCE_APP_PACKETS+1)
capped$TCP_CONVERSATION_EXCHANGE=log(capped$TCP_CONVERSATION_EXCHANGE+1)
capped$DIST_REMOTE_TCP_PORT=log(capped$DIST_REMOTE_TCP_PORT+1)
capped$REMOTE_APP_BYTES=log(capped$REMOTE_APP_BYTES+1)
capped$SOURCE_APP_BYTES=log(capped$SOURCE_APP_BYTES+1)
capped$APP_BYTES=log(capped$APP_BYTES+1)
capped$REMOTE_IPS=log(capped$REMOTE_IPS+1)

ytrain <- combined$Type
y <- xtrain$Type

#decision tree
fit <- rpart(y ~ ., data = ytrain ,method="class",control = rpart.control(cp = 0.01))
predicted= predict(fit,xtst)

#random forest
fit <- randomForest(y ~ ., data=xtrain)
summary(fit)
predicted= predict(fit,xtst)
table(xtst$Type, predicted)

#svm
fit <-svm(y ~ ., data = capped)
summary(fit)
predicted= predict(fit,xtst)
table(xtst$Type, predicted)

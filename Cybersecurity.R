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


malicious_and_benign_website <- read_csv("C:/Users/dell/Downloads/malicious-and-benign-websites1.csv")
View(malicious_and_benign_website)

head(malicious_and_benign_website)
summary(malicious_and_benign_website)
sum(is.na(malicious_and_benign_website$Type))
dim(malicious_and_benign_website)
sum(is.na(malicious_and_benign_website))
cleandf <- na.omit(malicious_and_benign_website)
sum(is.na(cleandf))
colSums(sapply(malicious_and_benign_website,is.na))
numericaldata = select_if(malicious_and_benign_website, is.numeric)
colSums(sapply(numericaldata, is.na))
glimpse(malicious_and_benign_website)
categoricaldata = select_if(malicious_and_benign_website, is.factor)
dim(categoricaldata)
#exploration
table(malicious_and_benign_website$Type)

with(malicious_and_benign_website, table(Type, URL_LENGTH))

ggplot(malicious_and_benign_website, aes(x = Type)) + geom_bar(color='blue ',fill = "blue")

ggplot(malicious_and_benign_website, aes(y = CHARSET, fill = Type)) + geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(hjust = 1))+scale_fill_gradient(low="yellow", high="blue")

malicious_and_benign_website %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +facet_wrap(~ key, scales = "free") + geom_histogram()   #geom_density()

numvalue<- subset(numericaldata, select = -c(CONTENT_LENGTH, DNS_QUERY_TIMES))

corrplot(cor(numvalue), addCoef.col = 1, number.cex = 0.5)

# data preparation
filtered_df= malicious_and_benign_website[!complete.cases(malicious_and_benign_website),]

head(filtered_df)
nrow(filtered_df)
malicious_and_benign_website$DNS_QUERY_TIMES=impute(malicious_and_benign_website$DNS_QUERY_TIMES, 0)
malicious_and_benign_website$SERVER=impute(malicious_and_benign_website$SERVER, 'nginx')
malicious_and_benign_website$CONTENT_LENGTH=impute(malicious_and_benign_website$CONTENT_LENGTH, mean)
head(malicious_and_benign_website$CONTENT_LENGTH, 100)
boxplot(malicious_and_benign_website$URL_LENGTH, data=malicious_and_benign_website, xlab = 'URL LENGTH',col = 'red')

#handling outliers
dfcap = malicious_and_benign_website[,c("REMOTE_APP_PACKETS", "URL_LENGTH", "SOURCE_APP_PACKETS","REMOTE_APP_BYTES","SOURCE_APP_BYTES",
                                        "APP_BYTES", "REMOTE_IPS", "DIST_REMOTE_TCP_PORT","TCP_CONVERSATION_EXCHANGE")]

dfrest = malicious_and_benign_website[,-c(2,11,12,13,14,15,16,17,18)]
head(dfrest)

pointcap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

dfcapped = pointcap(dfcap)
summary(dfcapped)
dfcombine = cbind(dfrest,dfcapped)

dfcombine$REMOTE_APP_PACKETS=log(dfcombine$REMOTE_APP_PACKETS+1)
dfcombine$URL_LENGTH=log(dfcombine$URL_LENGTH+1)
dfcombine$SOURCE_APP_PACKETS=log(dfcombine$SOURCE_APP_PACKETS+1)
dfcombine$TCP_CONVERSATION_EXCHANGE=log(dfcombine$TCP_CONVERSATION_EXCHANGE+1)
dfcombine$DIST_REMOTE_TCP_PORT=log(dfcombine$DIST_REMOTE_TCP_PORT+1)
dfcombine$REMOTE_APP_BYTES=log(dfcombine$REMOTE_APP_BYTES+1)
dfcombine$SOURCE_APP_BYTES=log(dfcombine$SOURCE_APP_BYTES+1)
dfcombine$APP_BYTES=log(dfcombine$APP_BYTES+1)
dfcombine$REMOTE_IPS=log(dfcombine$REMOTE_IPS+1)
dfcombine$Type= as.factor(dfcombine$Type)

#data training
indices = sample(1:nrow(dfcombine), size=0.2*nrow(dfcombine))
#spliting
xtst = dfcombine[indices,]
dim(xtst) 

xtrain = dfcombine[-indices,]
dim(xtrain)

dfcap$REMOTE_APP_PACKETS=log(dfcap$REMOTE_APP_PACKETS+1)
dfcap$URL_LENGTH=log(dfcap$URL_LENGTH+1)
dfcap$SOURCE_APP_PACKETS=log(dfcap$SOURCE_APP_PACKETS+1)
dfcap$TCP_CONVERSATION_EXCHANGE=log(dfcap$TCP_CONVERSATION_EXCHANGE+1)
dfcap$DIST_REMOTE_TCP_PORT=log(dfcap$DIST_REMOTE_TCP_PORT+1)
dfcap$REMOTE_APP_BYTES=log(dfcap$REMOTE_APP_BYTES+1)
dfcap$SOURCE_APP_BYTES=log(dfcap$SOURCE_APP_BYTES+1)
dfcap$APP_BYTES=log(dfcap$APP_BYTES+1)
dfcap$REMOTE_IPS=log(dfcap$REMOTE_IPS+1)

ytrain <- dfcombine$Type
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
fit <-svm(y ~ ., data = dfcap)
summary(fit)
predicted= predict(fit,xtst)
table(xtst$Type, predicted)

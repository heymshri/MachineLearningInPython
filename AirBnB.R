library(tidyverse)
library(ggplot2)
install.packages("psych")
library(psych)
airbnbdata<- read_csv('/content/AB_NYC_2019.csv')
summary(airbnbdata)
null<- filter(airbnbdata,is.na(last_review) == TRUE)
head(null)
airbnbdata$reviews_per_month[is.na(airbnbdata$reviews_per_month) == TRUE] = 0
#split date into year, month and day
df<- tidyr::separate(airbnbdata,last_review,c("Year","Month","Day"),sep = "-")

#Replace NA values with 0 
df$Year[is.na(df$Year) == TRUE] = 0
df$Month[is.na(df$Month) == TRUE] = 0
df$Day[is.na(df$Day) == TRUE] = 0

#Change the datatype of some variables to support calculations
df$neighbourhood_group<- as.factor(df$neighbourhood_group)
df$room_type<- as.factor(df$room_type)
df$Year<- as.integer(df$Year)
df$Month<- as.integer(df$Month)
df$Day<- as.integer(df$Day)

summary(df)
df<-df %>%
  na.omit()		
dim(df)
sum(is.na(df))
#Select numeric data for correlation plot
data_numeric<- select(df,id,host_id,latitude,longitude,price,minimum_nights,number_of_reviews,Year,Month,Day,reviews_per_month,calculated_host_listings_count,availability_365)
corPlot(data_numeric,cex=0.3)
ggplot(df, aes(x = neighbourhood_group)) + geom_bar() + ggtitle("Count of rooms by neigbourhood group and room type")
ggplot(data = df, mapping = aes(x = neighbourhood_group, y = price)) +
  geom_boxplot()
ggplot(df,aes(x=room_type, y=price))+geom_point()
split = createDataPartition(df$price,
                            times = 1,
                            p = 0.8,
                            list = F)
train = df[split, ]
test = df[-split, ]
model <- train %>% select(price, # place target variable first
                       neighbourhood_group,
                       latitude, 
                       longitude,                     
                       room_type,                                               
                       number_of_reviews,             
                       calculated_host_listings_count,
                       availability_365
) %>% lm()  
summary(model)
cat("R-Squared = ", round(as.numeric(summary(m3)[9]),4));
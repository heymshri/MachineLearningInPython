library(tidyverse)
library(ggplot2)

library(readxl)
df <- read_excel('/content/3064091BodyFatData.xlsx')

head(df,5)
summary(df)
dim(df)
sum(is.na(df))

hist(df$BodyFat,xlab = "Body Fat", main = "Body Fat",col = "yellow")
hist(df$Abdomen,xlab = "Abdomen", main = "Abdomen",col = "red")

cor(df$BodyFat, df$Abdomen)
cor.test(df$BodyFat, df$Abdomen)
t.test(df$BodyFat, df$Abdomen, var.equal = TRUE)
linear_relation <- lm(df$BodyFat~df$Abdomen)
print(linear_relation)
plot(df$BodyFat, df$Abdomen, main = "Body Fat vs Abdomen ",
     xlab = "BodyFat", ylab = "Abdomen",
     pch = 19, frame = FALSE)
lines(lowess(df$BodyFat, df$Abdomen), col = "blue")
summary(linear_relation)

dff=df %>% filter(Abdomen >= 100) 
head(dff,3)
summary(dff)
linear_relation <- lm(dff$BodyFat~dff$Abdomen)
summary(print(linear_relation))
plot(dff$BodyFat, dff$Abdomen, main = "Body Fat vs Abdomen ",
     xlab = "BodyFat", ylab = "Abdomen",
     pch = 19, frame = FALSE)
lines(lowess(df$BodyFat, df$Abdomen), col = "blue")
cor(dff$BodyFat, dff$Abdomen)
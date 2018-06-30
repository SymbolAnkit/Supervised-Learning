   ###### clear Environment

    rm(list = ls())

   ###### set working directory

    setwd("C:/Users/Ankit/Desktop/randomForest")

  ###### Load Important Libraries

    library(readr)
    library(lubridate)

   ###### read data

    pre <- read_csv("new_sales_data.csv")
    pos <- read_csv("Dummy data_JAn.csv")

  __* Convert BillingDate to DateTime Format*__

    pre$BillingDate <- as.Date(pre$BillingDate , "%Y-%m-%d")
    pos$BillingDate <- as.Date(pos$BillingDate ,"%m/%d/%Y")
    
   __*rbind() function combines vector, matrix or data frame by rows*__

    res <- rbind(pre,pos)

   __*Compactly Display The Structure Of An Arbitrary R Object*__

    str(res)

res$year <- year(res$BillingDate)
res$month <- month(res$BillingDate)
res$day <- day(res$BillingDate)
res$weekday <- wday(res$BillingDate)

library(dummies)
library(randomForest)

train <- res[ 1:38542 , ]
test <- res[ 38543:44704 , ]

# In random Forest ntree and mtry is the tuning parameter for better model
#  best mtry 
mtry <- tuneRF(train[ ,-3],train$Netvalue,ntreeTry = 500,stepFactor = 1.5,
               improve = 0.1,trace = T,plot = T)
set.seed(111)
fit <- randomForest(train$Netvalue ~ . , data =train[ , -c(1)] 
                    , ntree = 150 ,mtry =3 )
fit
plot(fit)

train$pr <- predict(fit,train[ , -c(1)])
test$pr <- predict(fit,test[ , -c(1)])

# write.csv(res,"mm.csv")

# res2 <- res %>% group_by(BillingDate) %>% 
  # summarise(Netvalue=sum(Netvalue),
            # predicted = sum(pr))
# write.csCv(res2 , "lll.csv")

mp <- rbind(train,test)

mpn <- mp %>% group_by(BillingDate) %>%
    summarise(Netvalue=sum(Netvalue),
              predicted = sum(pr))
write.csv(mpn , "cpl.csv")
RMSE <- function(a,b){
  sqrt(mean((a-b)^2))
}

RMSE(train$pr,train$Netvalue)

acutals_pred <- as.data.frame(cbind(actuals=train$Netvalue,predicteds=train$pr))
min_max_accuracy <- mean(apply(acutals_pred, 1, min) / apply(acutals_pred, 1, max)) 
min_max_accuracy


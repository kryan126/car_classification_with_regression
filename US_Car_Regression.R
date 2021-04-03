# Importing Libraries
library(tidyverse)

# Importing Data 
car <- read_csv("car_data.csv")  #read the car_data dataset in R
names(car)

# Data Exploration
a1 <- car %>%
  ggplot(aes(x=as.factor(IsBadBuy), y=VehOdo)) +
  geom_boxplot(fill = "steelblue", alpha=0.5)

a1 + theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color="grey95"),
        panel.grid.minor.x =element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face="bold"),
        plot.caption = element_text(face="italic")) + 
  labs(title="Boxplot of Vehicle Odometer Reading",
       caption = "Source: http://www.kaggle.com/c/DontGetKicked",
       x="Is It a Bad Buy?", y="Odometer Reading")

a2 <- car %>%
  ggplot(aes(x=as.factor(IsBadBuy), y=VehicleAge)) +
  geom_boxplot(fill = "red", alpha=0.5)

a2 + theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color="grey95"),
        panel.grid.minor.x =element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face="bold"),
        plot.caption = element_text(face="italic")) + 
  labs(title="Boxplot of Vehicle Age",
       caption = "Source: http://www.kaggle.com/c/DontGetKicked",
       x="Is It a Bad Buy?", y="Vehicle Age")

# Notes: 
# As age increases, a car is more likely to be a bad buy as cars classified as 
# bad buys have higher median ages 

tab1 <- table(car$Make, car$IsBadBuy)
ordered_tab1 <- tab1[order(-rowSums(tab1)),]
ordered_tab1

# Based on the above table, it seems cars classified as bad buys are more likely 
# be made by Ford Chrysler, and Chevy 

# Feature Engineering 
MPY <- car$VehOdo/car$VehicleAge
medMPY <- median(MPY, na.rm = TRUE)

car1 <- car %>%
  mutate(MPYind = ifelse(VehOdo/VehicleAge > medMPY, 1, 0),
         Price0 = ifelse((MMRAcquisitionAuctionAveragePrice == 0 |       MMRAcquisitionRetailAveragePrice == 0), 1, 0),
         VehType = case_when(
           Size=="LARGE SUV" |Size=="MEDIUM SUV" |Size=="SMALL SUV" ~ "SUV",
           Size=="LARGE TRUCK"|Size=="MEDIUM TRUCK"|Size=="SMALL TRUCK"~ "Truck",
           Size=="VAN"|Size=="CROSSOVER"|Size=="LARGE"|Size=="MEDIUM"~"Regular",
           Size=="COMPACT"|Size=="SPECIALTY"|Size=="SPORTS"~"Small"
         ))

car2 <- car1 %>% 
  group_by(Make) %>%
  mutate(MMRAcquisitionRetailAveragePrice = ifelse(Price0==1, mean(MMRAcquisitionRetailAveragePrice, na.rm=0), MMRAcquisitionRetailAveragePrice),
         MMRAcquisitionAuctionAveragePrice = ifelse(Price0==1, mean(MMRAcquisitionAuctionAveragePrice, na.rm=0), MMRAcquisitionAuctionAveragePrice)) %>%
  ungroup()

# Linear Regression 
lm_train <- lm(data = car2, IsBadBuy ~ Auction + VehicleAge + Make + Color+WheelType + 
                 VehOdo + MPYind + VehType + MMRAcquisitionAuctionAveragePrice +   MMRAcquisitionRetailAveragePrice)
summary(lm_train)


# Logistic Regression 
log_train <- glm(data = car2,IsBadBuy ~ Auction + VehicleAge + Make + Color+WheelType +  VehOdo + MPYind + VehType + MMRAcquisitionAuctionAveragePrice +   MMRAcquisitionRetailAveragePrice, family = "binomial")

summary(log_train)

# Splitting into Train and Test Sets 
set.seed(1)
train_insts <- sample(nrow(car2), .7*nrow(car2))

train_data <- car2[train_insts,]
test_data <- car2[-train_insts,]

log_model <- glm(data = train_data,IsBadBuy ~ Auction + VehicleAge + Make + Color+WheelType +  VehOdo + MPYind + VehType + MMRAcquisitionAuctionAveragePrice +   MMRAcquisitionRetailAveragePrice, family = "binomial")

lm_model <- lm(data = train_data, IsBadBuy ~ Auction + VehicleAge + Make + Color+WheelType + VehOdo + MPYind + VehType + MMRAcquisitionAuctionAveragePrice +   MMRAcquisitionRetailAveragePrice)

summary(lm_model)

# Model Evaluation : RMSE
RMSE = function(y_m, y){
  sqrt(mean((y_m - y)^2, na.rm = TRUE))
}

lm_train_pred <- predict(lm_model, train_data)
lm_test_pred <- predict(lm_model, test_data)

lm_train_rmse <- RMSE(lm_train_pred, train_data$IsBadBuy)
lm_test_rmse <- RMSE(lm_test_pred, test_data$IsBadBuy)

# Model Evaluation Confusion Matrix 
classifier = function(y_m, cutoff){
  category <- ifelse(y_m > cutoff, 1, 0)
  return(category)
}

lm_pred <- predict(lm_model, test_data)
log_pred <- predict(log_model, test_data, type = "response")

lm_class <- classifier(lm_pred, 0.5)
log_class <- classifier(log_pred, 0.5)

lm_CM <- table(test_data$IsBadBuy, lm_class)
log_CM <- table(test_data$IsBadBuy, log_class)

lm_TP <- lm_CM[2,2]
lm_TN <- lm_CM[1,1]
lm_FP <- lm_CM[1,2]
lm_FN <- lm_CM[2,1]

log_TP <- log_CM[2,2]
log_TN <- log_CM[1,1]
log_FP <- log_CM[1,2]
log_FN <- log_CM[2,1]

lm_TPR <- lm_TP/(lm_TP+lm_FN)
log_TPR <- log_TP/(log_TP+log_FN)
lm_TNR <- lm_TN/(lm_TN + lm_FP)
log_TNR <- log_TN/(log_TN + log_FP)

lm_FPR <- 1 - lm_TNR
log_FPR <- 1 - log_TNR

lm_accuracy <- (lm_TP+lm_TN)/(lm_TP+lm_TN+lm_FP+ lm_FN)
log_accuracy <- (log_TP+log_TN)/(log_TP+log_TN+log_FP+log_FN)

# Notes 
#Linear Accuracy: 0.67240 
#Log Accuracy: 0.66876

#Linear TPR: 0.55429 
#Log TPR: 0.56762

#Linear FPR: 0.210803 
#Log FPR: 0.231225

#Based on the confusion matrix, the Linear model is more accurate.

# Testing Cutoffs 
low_class <- classifier(lm_pred, 0.25)
high_class <- classifier(lm_pred, 0.75)

low_CM <- table(test_data$IsBadBuy, low_class)
high_CM <- table(test_data$IsBadBuy, high_class)

low_TP <- low_CM[2,2]
low_TN <- low_CM[1,1]
low_FP <- low_CM[1,2]
low_FN <- low_CM[2,1]

high_TP <- high_CM[2,2]
high_TN <- high_CM[1,1]
high_FP <- high_CM[1,2]
high_FN <- high_CM[2,1]

low_TPR <- low_TP/(low_TP+low_FN)
high_TPR <- high_TP/(high_TP+high_FN)
low_TNR <- low_TN/(low_TN + low_FP)
high_TNR <- high_TN/(high_TN + high_FP)

low_FPR <- 1 - low_TNR
high_FPR <- 1 - high_TNR

low_accuracy <- (low_TP+low_TN)/(low_TP+low_TN+low_FP+ low_FN)
high_accuracy <- (high_TP+high_TN)/(high_TP+high_TN+high_FP+high_FN)

# Notes 
#0.25 Accuracy: 0.540576

#0.75 Accuracy: 0.614442

#The 0.75 cutoff model has a higher accuracy

#0.25 TPR: 0.96269

#0.75 TPR: 0.23784

#The 0.25 cutoff model has the highest TPR

#0.25 FPR: 0.87681

#0.75 FPR: 0.013175

#The 0.25 cutoff model has the highest FPR

# The 0.5 cutoff yields the best result because it has the highest accuracy 
#of all three cutoffs. It also has the lowest false positive rate which is 
#important because it would be not ideal if someone using this model to inform 
#buying decisions was told a car was a bad buy even though it would actual 
#be a smart purchase. 
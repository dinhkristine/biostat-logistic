
#### packages ---- 

library(tidyverse)


#### parameters ----

set.seed(1234)

random_prop <- 0.80


#### load data ---- 

data <- read.csv("breast_cancer_data.csv")  


#### create train and test set ---- 

train_ind <- caret::createDataPartition(y = data$diagnosis, p = random_prop, list=FALSE)

train <- data[train_ind, ] 

test <- data[-train_ind, ] 


#### linear regression FAIL ----

fit_lm <- lm(diagnosis ~ mean_radius, data = train)

ggplot(train, aes(x = mean_radius, y = diagnosis)) + 
  geom_point(alpha = 0.2, size = 2) + 
  geom_abline(intercept = fit_lm$coefficients[[1]], slope = fit_lm$coefficients[[2]], 
              color = "red", size = 1) + 
  scale_y_discrete(limits = c(0,1)) + 
  labs(y = "Diagnosis", 
       x = "Mean Radius", 
       title = "Linear Regression Fit for Diagnosis by Mean Radius") + 
  theme_minimal()


#### GLM ---- 
    
fit_glm <- glm(diagnosis ~ mean_radius, data = train, family = "binomial")
    
preds <- predict(fit_glm, newdata = test, type = "response")
    
test$preds_glm <- preds
    
roc <- pROC::roc(response = test$diagnosis, predictor = preds)

auc <- as.data.frame(pROC::auc(roc))

auc <- as.vector(auc$`pROC::auc(roc)`)


ggplot(test, aes(x = mean_radius)) + 
  geom_point(aes(y = diagnosis), size = 2, alpha = 0.2) + 
  geom_line(aes(y = preds_glm), color = "red", size = 1) + 
  scale_y_discrete(limits = c(0,1)) + 
  scale_y_discrete(limits = c(0,1)) + 
  labs(y = "Diagnosis", 
       x = "Mean Radius", 
       title = "Logistics Regression Fit for Diagnosis by Mean Radius") + 
  theme_minimal()



ggplot(train, aes(x = mean_radius, y = diagnosis)) + 
  geom_point(alpha = 0.2, size = 2) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") + 
  scale_y_discrete(limits = c(0,1)) + 
  labs(y = "Diagnosis", 
       x = "Mean Radius", 
       title = "Logistics Regression Fit for Diagnosis by Mean Radius") + 
  theme_minimal()

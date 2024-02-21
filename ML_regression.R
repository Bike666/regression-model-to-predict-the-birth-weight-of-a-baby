setwd("E:\\AAA Warsaw University\\The 2nd semester\\Machine Learning 1\\_assessment-20230512T103446Z-001\\_assessment\\data")
options(scipen=999)
Sys.setenv(LANG = "en")



if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(readr)){
  install.packages("readr")
  library(readr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(leaps)){
  install.packages("leaps")
  library(leaps)
}

if(!require(car)){
  install.packages("car")
  library(car)
}


if(!require(caret)){
  install.packages("caret")
  library(caret)
}


if(!require(corrplot)){
  install.packages("corrplot")
  library(corrplot)
}

if(!require(MLmetrics)){
  install.packages("MLmetrics")
  library(MLmetrics)
}

if(!require(foreach)){
  install.packages("foreach")
  library(foreach)
}

if(!require(doParallel)){
  install.packages("doParallel")
  library(doParallel)
}

if(!require(randomForest)){
  install.packages("randomForest")
  library(randomForest)
}

if(!require(adabag)){
  install.packages("adabag")
  library(adabag)
}
if(!require(ada)){
  install.packages("ada")
  library(ada)
}

if(!require(gbm)){
  install.packages("gbm")
  library(gbm)
}
# read the file and prepare the data
newborn_train<- read.csv("newborn_train.csv")
newborn_test<- read.csv("newborn_test.csv")

glimpse(newborn_train)
glimpse(newborn_test)

colSums(is.na(newborn_train)) %>% 
  sort()

colSums(is.na(newborn_test)) %>% 
  sort()
#There are some missing values
table(newborn_train$cigarettes_before_pregnancy)
# we can see that cigarettes_before_pregnancy most is 0 , so we will use 0 to replace the na
#truncmean(newborn_train$cigarettes_before_pregnancy, p = 0.1, type = "both")
newborn_train$cigarettes_before_pregnancy[is.na(newborn_train$cigarettes_before_pregnancy)] <- 0
any(is.na(newborn_train$cigarettes_before_pregnancy))

table(newborn_train$mother_delivery_weight)
hist(newborn_train$mother_delivery_weight)
median(newborn_train$mother_delivery_weight, na.rm = TRUE)
# the median is 181
newborn_train$mother_delivery_weight[is.na(newborn_train$mother_delivery_weight)] <- 181

table(newborn_train$number_prenatal_visits) %>% sort()          
hist(newborn_train$number_prenatal_visits)
median(newborn_train$number_prenatal_visits, na.rm = TRUE)
# number_prenatal_visits the most frequency number is 12
newborn_train$number_prenatal_visits[is.na(newborn_train$number_prenatal_visits)] <- 12

table(newborn_train$mother_weight_gain) %>% sort()   
hist(newborn_train$mother_weight_gain)
median(newborn_train$mother_weight_gain, na.rm = TRUE)
# mother_weight_gain the most frequency number is 30 , median is 29 , so quite similar ,so here I choose 29
newborn_train$mother_weight_gain[is.na(newborn_train$mother_weight_gain)] <- 29

table(newborn_train$mother_body_mass_index) %>% sort()   
hist(newborn_train$mother_body_mass_index)
median(newborn_train$mother_body_mass_index, na.rm = TRUE)
# mother_body_mass_index  median is 25.7 , so quite similar ,so here I choose 25.7
newborn_train$mother_body_mass_index[is.na(newborn_train$mother_body_mass_index)] <- 25.7

table(newborn_train$mother_height) %>% sort()   
hist(newborn_train$mother_height)
median(newborn_train$mother_height, na.rm = TRUE)
# mother_height  median is 64 , so quite similar ,so here I choose 64
newborn_train$mother_height[is.na(newborn_train$mother_height)] <- 64

table(newborn_train$father_age) %>% sort()   
hist(newborn_train$father_age)
median(newborn_train$father_age, na.rm = TRUE)
newborn_train$father_age[is.na(newborn_train$father_age)] <- 31

newborn_train$newborn_gender <- as.factor(newborn_train$newborn_gender)
newborn_train$previous_cesarean <- as.factor(newborn_train$previous_cesarean)


table(newborn_train$mother_marital_status) %>% sort() 


newborn_train <- newborn_train %>% 
  mutate( White_alone = ifelse(newborn_train$mother_race == 1, 1, 0),
          Black_alone = ifelse(newborn_train$mother_race == 2, 1, 0),
          AIAN_alone = ifelse(newborn_train$mother_race == 3, 1, 0),
          Asian_alone = ifelse(newborn_train$mother_race == 4, 1, 0),
          NHOPI_alone = ifelse(newborn_train$mother_race == 5, 1, 0),
          More_than_one_race = ifelse(newborn_train$mother_race == 6, 1, 0)) 

newborn_train$White_alone <- as.factor(newborn_train$White_alone)
newborn_train$Black_alone <- as.factor(newborn_train$Black_alone)  
newborn_train$AIAN_alone <- as.factor(newborn_train$AIAN_alone)  
newborn_train$Asian_alone <- as.factor(newborn_train$Asian_alone)  
newborn_train$NHOPI_alone <- as.factor(newborn_train$NHOPI_alone)  
newborn_train$More_than_one_race <- as.factor(newborn_train$More_than_one_race)  
newborn_train$mother_marital_status<- as.factor(newborn_train$mother_marital_status)


     

# delete mother_race coloum
newborn_train <- subset(newborn_train, select = -c(mother_race))

newborn_train$father_education <- factor(newborn_train$father_education 
,
                              # levels from lowest to highest
                              levels = c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8,
                                         9),
                              labels = c("8th_grade_or_less",
                                         "9-12th_grade_and_no_diploma",
                                         "High_school_graduate_or_GED_completed",
                                         "Some_college_credit_but_no_degree",
                                         "Associate_degree",
                                         "Bachelor’s_degree",
                                         "Master’s_degree",
                                         "Doctorate_or_Professional_degree",
                                         "Unknown"
                                
                          ),
                              ordered = TRUE)


newborn_train$prenatal_care_month <- factor(newborn_train$prenatal_care_month 
                                         ,
                                         # levels from lowest to highest
                                         levels = c(0,
                                                    1,
                                                    2,
                                                    3,
                                                    4,
                                                    5,
                                                    6,
                                                    7,
                                                    8,
                                                    9,
                                                    99),
                                         labels = c(0,
                                                    1,
                                                    2,
                                                    3,
                                                    4,
                                                    5,
                                                    6,
                                                    7,
                                                    8,
                                                    9,
                                                    "prenatal_care_month_unknow"
                                           
                                                    
                                         ),
                                         ordered = TRUE)
table(newborn_train$prenatal_care_month)

newborn_train$mother_marital_status <- factor(newborn_train$mother_marital_status,
                                              labels = c("married","no_married")
                                              ,
                                              ordered = FALSE) # ordinal

newborn_train <- newborn_train %>% 
  mutate( married = ifelse(newborn_train$mother_marital_status        == "married", 1, 0),
          no_married = ifelse(newborn_train$mother_marital_status        == "no_married", 1,0),
          unknow_married = ifelse(is.na(newborn_train$mother_marital_status ),  1,0) )
newborn_train$no_married[is.na(newborn_train$no_married)] <- 0
newborn_train$married[is.na(newborn_train$married)] <- 0
newborn_train$married <- factor(newborn_train$married) 
newborn_train$no_married <- factor(newborn_train$no_married)
newborn_train$unknow_married <- factor(newborn_train$unknow_married)
newborn_train <- subset(newborn_train, select = -c(mother_marital_status))


newborn_train <- newborn_train %>% 
  mutate( previous_cesarean_N = ifelse(newborn_train$previous_cesarean == "N", 1, 0),
          previous_cesarean_Y = ifelse(newborn_train$previous_cesarean == "Y", 1,0),
          previous_cesarean_other = ifelse(newborn_train$previous_cesarean == "U", 1,0), )
newborn_train$previous_cesarean_N <- factor(newborn_train$previous_cesarean_N)
newborn_train$previous_cesarean_Y <- factor(newborn_train$previous_cesarean_Y)
newborn_train$previous_cesarean_other <- factor(newborn_train$previous_cesarean_other)
newborn_train <- subset(newborn_train, select = -c(previous_cesarean))


newborn_train <- newborn_train %>% 
  mutate( newborn_gender_F = ifelse(newborn_train$newborn_gender == "F", 1, 0),
          
          newborn_gender_M = ifelse(newborn_train$newborn_gender == "M", 1, 0 ))
newborn_train$newborn_gender_M <- factor(newborn_train$newborn_gender_M)
newborn_train$newborn_gender_F <- factor(newborn_train$newborn_gender_F)
newborn_train <- subset(newborn_train, select = -c(newborn_gender))

table(newborn_train$prenatal_care_month)

newborn_train$prenatal_care_month[is.na(newborn_train$prenatal_care_month)] <- 4

#Checking if there any outlier

#table(newborn_train$mother_body_mass_index)
#newborn_train$mother_body_mass_index[newborn_train$mother_body_mass_index > 68] <- 68
#newborn_train$mother_body_mass_index[newborn_train$mother_body_mass_index >= 67 & newborn_train$mother_body_mass_index < 68] <- 67
#a <-mean(newborn_train$mother_body_mass_index)
#sd <- sd(newborn_train$mother_body_mass_index)
#threshold <- 3
#outliers <- newborn_train$mother_body_mass_index[abs(newborn_train$mother_body_mass_index - a) > threshold * sd]
#plot(outliers)
#length(outliers)
#unique(outliers) %>% sort()
#a <- mean(newborn_train$mother_body_mass_index)
#sd <- sd(newborn_train$mother_body_mass_index)
#threshold <- 3

# 将原数据中的异常值替换为平均数
#newborn_train$mother_body_mass_index[abs(newborn_train$mother_body_mass_index - a) > threshold * sd] <- a

# 打印替换后的数据
#print(newborn_train$mother_body_mass_index)




# name
column_names <- c('mother_body_mass_index', "mother_delivery_weight", " mother_height", "mother_weight_gain","father_age",'cigarettes_before_pregnancy','number_prenatal_visits'      )  # 将需要计算误差和改正的列名添加到列表中

# 获取数据表中所有列的名称
#column_names <- colnames(newborn_train)



for (col_name in column_names) {
  
  if (is.numeric(newborn_train[[col_name]])) {
    # mean
    mean_value <- mean(newborn_train[[col_name]])
    
    # sd
    sd_value <- sd(newborn_train[[col_name]])
    
   
    threshold <- 3
    
   
    outliers <- newborn_train[[col_name]][abs(newborn_train[[col_name]] - mean_value) > threshold * sd_value]
    
    # outlier change to mean
    newborn_train[[col_name]][abs(newborn_train[[col_name]] - mean_value) > threshold * sd_value] <- mean_value
  }
}

hist(newborn_train$mother_body_mass_index)
hist(newborn_train$mother_delivery_weight)




# There are some outlines 

save.image(".RData")





#Test one

colSums(is.na(newborn_train)) %>% 
  sort()

colSums(is.na(newborn_test)) %>% 
  sort()
#There are some missing values
table(newborn_test$cigarettes_before_pregnancy)
# we can see that cigarettes_before_pregnancy most is 0 , so we will use 0 to replace the na
#truncmean(newborn_train$cigarettes_before_pregnancy, p = 0.1, type = "both")
newborn_test$cigarettes_before_pregnancy[is.na(newborn_test$cigarettes_before_pregnancy)] <- 0
any(is.na(newborn_test$cigarettes_before_pregnancy))

table(newborn_test$mother_delivery_weight)
hist(newborn_test$mother_delivery_weight)
median(newborn_test$mother_delivery_weight, na.rm = TRUE)
# the median is 181
newborn_test$mother_delivery_weight[is.na(newborn_test$mother_delivery_weight)] <- 181

table(newborn_test$number_prenatal_visits) %>% sort()          
hist(newborn_test$number_prenatal_visits)
median(newborn_test$number_prenatal_visits, na.rm = TRUE)
# number_prenatal_visits the most frequency number is 12
newborn_test$number_prenatal_visits[is.na(newborn_test$number_prenatal_visits)] <- 12

table(newborn_test$mother_weight_gain) %>% sort()   
hist(newborn_test$mother_weight_gain)
median(newborn_test$mother_weight_gain, na.rm = TRUE)
# mother_weight_gain the most frequency number is 30 , median is 29 , so quite similar ,so here I choose 29
newborn_test$mother_weight_gain[is.na(newborn_test$mother_weight_gain)] <- 29

table(newborn_test$mother_body_mass_index) %>% sort()   
hist(newborn_test$mother_body_mass_index)
median(newborn_test$mother_body_mass_index, na.rm = TRUE)
# mother_body_mass_index  median is 25.7 , so quite similar ,so here I choose 25.7
newborn_test$mother_body_mass_index[is.na(newborn_test$mother_body_mass_index)] <- 25.7

table(newborn_test$mother_height) %>% sort()   
hist(newborn_test$mother_height)
median(newborn_test$mother_height, na.rm = TRUE)
# mother_height  median is 64 , so quite similar ,so here I choose 64
newborn_test$mother_height[is.na(newborn_test$mother_height)] <- 64

table(newborn_test$father_age) %>% sort()   
hist(newborn_test$father_age)
median(newborn_test$father_age, na.rm = TRUE)
newborn_test$father_age[is.na(newborn_test$father_age)] <- 31

newborn_test$newborn_gender <- as.factor(newborn_test$newborn_gender)
newborn_test$previous_cesarean <- as.factor(newborn_test$previous_cesarean)


table(newborn_test$mother_marital_status) %>% sort() 


newborn_test <- newborn_test %>% 
  mutate( White_alone = ifelse(newborn_test$mother_race == 1, 1, 0),
          Black_alone = ifelse(newborn_test$mother_race == 2, 1, 0),
          AIAN_alone = ifelse(newborn_test$mother_race == 3, 1, 0),
          Asian_alone = ifelse(newborn_test$mother_race == 4, 1, 0),
          NHOPI_alone = ifelse(newborn_test$mother_race == 5, 1, 0),
          More_than_one_race = ifelse(newborn_test$mother_race == 6, 1, 0)) 

newborn_test$White_alone <- as.factor(newborn_test$White_alone)
newborn_test$Black_alone <- as.factor(newborn_test$Black_alone)  
newborn_test$AIAN_alone <- as.factor(newborn_test$AIAN_alone)  
newborn_test$Asian_alone <- as.factor(newborn_test$Asian_alone)  
newborn_test$NHOPI_alone <- as.factor(newborn_test$NHOPI_alone)  
newborn_test$More_than_one_race <- as.factor(newborn_test$More_than_one_race)  
newborn_test$mother_marital_status<- as.factor(newborn_test$mother_marital_status)




# delete mother_race coloum
newborn_test <- subset(newborn_test, select = -c(mother_race))

newborn_test$father_education <- factor(newborn_test$father_education 
                                         ,
                                         # levels from lowest to highest
                                         levels = c(1,
                                                    2,
                                                    3,
                                                    4,
                                                    5,
                                                    6,
                                                    7,
                                                    8,
                                                    9),
                                         labels = c("8th_grade_or_less",
                                                    "9-12th_grade_and_no_diploma",
                                                    "High_school_graduate_or_GED_completed",
                                                    "Some_college_credit_but_no_degree",
                                                    "Associate_degree",
                                                    "Bachelor’s_degree",
                                                    "Master’s_degree",
                                                    "Doctorate_or_Professional_degree",
                                                    "Unknown"
                                                    
                                         ),
                                         ordered = TRUE)


newborn_test$prenatal_care_month <- factor(newborn_test$prenatal_care_month 
                                            ,
                                            # levels from lowest to highest
                                            levels = c(0,
                                                       1,
                                                       2,
                                                       3,
                                                       4,
                                                       5,
                                                       6,
                                                       7,
                                                       8,
                                                       9,
                                                       99),
                                            labels = c(0,
                                                       1,
                                                       2,
                                                       3,
                                                       4,
                                                       5,
                                                       6,
                                                       7,
                                                       8,
                                                       9,
                                                       "prenatal_care_month_unknow"
                                                       
                                                       
                                            ),
                                            ordered = TRUE)
table(newborn_test$prenatal_care_month)

newborn_test$mother_marital_status <- factor(newborn_test$mother_marital_status,
                                              labels = c("married","no_married")
                                              ,
                                              ordered = FALSE) # ordinal

newborn_test <- newborn_test %>% 
  mutate( married = ifelse(newborn_test$mother_marital_status        == "married", 1, 0),
          no_married = ifelse(newborn_test$mother_marital_status        == "no_married", 1,0),
          unknow_married = ifelse(is.na(newborn_test$mother_marital_status ),  1,0) )
newborn_test$no_married[is.na(newborn_test$no_married)] <- 0
newborn_test$married[is.na(newborn_test$married)] <- 0
newborn_test$married <- factor(newborn_test$married) 
newborn_test$no_married <- factor(newborn_test$no_married)
newborn_test$unknow_married <- factor(newborn_test$unknow_married)
newborn_test <- subset(newborn_test, select = -c(mother_marital_status))


newborn_test <- newborn_test %>% 
  mutate( previous_cesarean_N = ifelse(newborn_test$previous_cesarean == "N", 1, 0),
          previous_cesarean_Y = ifelse(newborn_test$previous_cesarean == "Y", 1,0),
          previous_cesarean_other = ifelse(newborn_test$previous_cesarean == "U", 1,0), )
newborn_test$previous_cesarean_N <- factor(newborn_test$previous_cesarean_N)
newborn_test$previous_cesarean_Y <- factor(newborn_test$previous_cesarean_Y)
newborn_test$previous_cesarean_other <- factor(newborn_test$previous_cesarean_other)
newborn_test <- subset(newborn_test, select = -c(previous_cesarean))


newborn_test <- newborn_test %>% 
  mutate( newborn_gender_F = ifelse(newborn_test$newborn_gender == "F", 1, 0),
          
          newborn_gender_M = ifelse(newborn_test$newborn_gender == "M", 1, 0 ))
newborn_test$newborn_gender_M <- factor(newborn_test$newborn_gender_M)
newborn_test$newborn_gender_F <- factor(newborn_test$newborn_gender_F)
newborn_test <- subset(newborn_test, select = -c(newborn_gender))

table(newborn_test$prenatal_care_month)

newborn_test$prenatal_care_month[is.na(newborn_test$prenatal_care_month)] <- 4

summary(newborn_test)

write.csv(newborn_test, file = "newborn_test1.csv", row.names = FALSE)



# feature selection 
set.seed(123)
percentage <- 0.1
sample_size_0_1 <- round(percentage * nrow(newborn_train)) 
newborn_train_0_1 <- newborn_train[sample(nrow(newborn_train), size = sample_size_0_1), ]


newborn_train_0_1_vars <- 
  # check if variable is numeric
  sapply(newborn_train_0_1, is.numeric) %>% 
  
  which() %>% 
  
  names()

newborn_train_0_1_vars

newborn_train_0_1_correlations <- 
  cor(newborn_train_0_1[, newborn_train_0_1_vars],
      use = "pairwise.complete.obs")

newborn_train_0_1_correlations
corrplot(newborn_train_0_1_correlations)
dev.off()
plot.new()
# we can see that father age has no big influence



newborn_train_0_1_catvars  <- 
  # check if variable is a factor
  sapply(newborn_train_0_1, is.factor) %>% 
  # select those which are
  which() %>% 
  # and keep just their names
  names()


newborn_train_0_1_catvars_anova <- function(categorical_var) {
  anova_ <- aov(newborn_train_0_1$newborn_weight~ 
                  newborn_train_0_1[[categorical_var]]) 
  
  return(summary(anova_)[[1]][1, 4])
}

sapply(newborn_train_0_1_catvars,
       newborn_train_0_1_catvars_anova) %>% 
  
  sort(decreasing = TRUE) -> newborn_train_0_1_catvars_all

# we can see that NHOPI_alone is small 

newborn_train_0_1_catvars_all
glimpse(newborn_train_0_1)

set.seed(123)
options(contrasts = c("contr.treatment",  
                      "contr.treatment"))
newborn_train_0_1_train <- createDataPartition(newborn_train_0_1$newborn_weight, 
                                          
                                          p = 0.7, 
                                          
                                          list = FALSE) 

newborn_train_1 <- newborn_train_0_1[newborn_train_0_1_train,]
newborn_test_1 <- newborn_train_0_1[-newborn_train_0_1_train,]
summary(newborn_train_1)
nrow(newborn_train_1)
summary(newborn_test_1)

if (file.exists("E:\\AAA Warsaw University\\The 2nd semester\\Machine Learning 1\\_assessment-20230512T103446Z-001\\_assessment\\data/.Rhistory")) {
  loadhistory("E:\\AAA Warsaw University\\The 2nd semester\\Machine Learning 1\\_assessment-20230512T103446Z-001\\_assessment\\data/.Rhistory")
}

load("E:\\AAA Warsaw University\\The 2nd semester\\Machine Learning 1\\_assessment-20230512T103446Z-001\\_assessment\\data/.RData")
save.image(".RData")
# liner model

ctrl_cv5 <- trainControl(method = "cv",
                          number = 5)
ctrl_cv10 <- trainControl(method = "cv",
                         number = 10)
set.seed(123456)

liner_model <-
  train(newborn_weight ~ ., 
        data = newborn_train_1 %>% 
         
          dplyr::select(-father_age),
        method = "lm",
        
        trControl = ctrl_cv5)
liner_model
summary(liner_model)

liner_model$resample
liner_model$finalModel
predictions <- predict(liner_model, newdata = newborn_test_1)
absolute_percentage_errors_liner <- abs((predictions - newborn_test_1$newborn_weight) / newborn_test_1$newborn_weight) * 100
mape <- mean(absolute_percentage_errors_liner)
# mape is 16.05896
count <- sum(newborn_test_1$NHOPI_alone == 1)


glimpse(newborn_test_1)


# Elastic Net

parameters_elastic <- expand.grid(alpha = seq(0, 1, 0.2), 
                                  lambda = seq(10, 1e4, 10))

parameters_elastic2 <- expand.grid(alpha = seq(0, 1, 0.1), 
                                   lambda = seq(0.01, 1, 0.01))

nrow(parameters_elastic2)

elastic_net <- train(newborn_weight ~ .,
                     data = newborn_train_1 %>% 
                                
                             dplyr::select(-father_age,-newborn_gender_M ,previous_cesarean_other  ,previous_cesarean_Y,previous_cesarean_N,unknow_married,More_than_one_race ,Asian_alone    ),
                        method = "glmnet", 
                        tuneGrid = parameters_elastic2,
                        trControl = ctrl_cv5)
class(elastic_net$Rsquared )


summary(elastic_net)
elastic_net$bestPerformance
elastic_net$bestTune
predictions1 <- predict(elastic_net, newdata = newborn_test_1)
absolute_percentage_errors <- abs((predictions1 - newborn_test_1$newborn_weight) / newborn_test_1$newborn_weight) * 100
mape <- mean(absolute_percentage_errors)

mape_value <- mape(newborn_test_1$ newborn_weight , predictions1,newdata = newborn_test_1)
# mape is 16.05874  # 01247738


MAPE(predictions1, newborn_test_1$ newborn_weight)



# ridge
lambdas <- exp(log(10)*seq(-2, 9, length.out = 200))

parameters_ridge <- expand.grid(alpha = 0, # ridge 
                                lambda = lambdas)





set.seed(12345)
ridge <- train(newborn_weight ~ .,
                        data = newborn_train_1 %>% 
                          dplyr::select(-father_age),
                      method = "glmnet", 
                      tuneGrid = parameters_ridge,
                      trControl = ctrl_cv5)


predictions1 <- predict(ridge, newdata = newborn_test_1)


MAPE(predictions1, newborn_test_1$ newborn_weight)
#0.1606134


#Random Forest Regression
percentage1 <- 0.01
sample_size_001 <- round(percentage1 * nrow(newborn_train)) 
newborn_train_001 <- newborn_train[sample(nrow(newborn_train), size = sample_size_001), ]
ctrl_cv5 <- trainControl(method = "cv",
                         number = 5)
set.seed(123)
options(contrasts = c("contr.treatment",  
                      "contr.treatment"))
newborn_train_001_train <- createDataPartition(newborn_train_001$newborn_weight, 
                                               
                                               p = 0.7, 
                                              
                                               list = FALSE) 

newborn_train_001_1 <- newborn_train_001[newborn_train_001_train,]
newborn_test_001_1 <- newborn_train_001[-newborn_train_001_train,]
summary(newborn_train_001_1)
nrow(newborn_train_001_1)
summary(newborn_train_001_1)

rf_model <- train(newborn_weight ~ .,
                  data = newborn_train_001_1 %>% dplyr::select(-father_age),
                  method = "rf",
                  trControl = ctrl_cv5)

rf_model
summary(rf_model)

rf_model$resample
rf_model$finalModel

predictions <- predict(rf_model, newdata = newborn_test_001_1)
absolute_percentage_errors_rf <- abs((predictions - newborn_test_001_1$newborn_weight) / newborn_test_001_1$newborn_weight) * 100
mape <- mean(absolute_percentage_errors_rf)
#15.78384
predictions_test <- predict(rf_model, newdata = newborn_test)
write.csv(predictions_test, file = "predictions_test.csv", row.names = FALSE)

#Gradient Boosting Random Forest Regressor
gbm_model <- train(newborn_weight ~ .,
                   data = newborn_train_001_1 %>% dplyr::select(-father_age),
                   method = "gbm",
                   trControl = ctrl_cv5,
                   #distribution = "gaussian",
                   verbose = FALSE)
                   #n.trees = 100,
                   #interaction.depth = 4,
                   #shrinkage = 0.01,
                   #bag.fraction = 0.5)


predictions <- predict(gbm_model, newdata = newborn_test_001_1)
absolute_percentage_errors_gbm <- abs((predictions - newborn_test_001_1$newborn_weight) / newborn_test_001_1$newborn_weight) * 100
mape <- mean(absolute_percentage_errors_gbm)
# 15.54973
Sys.time()
predictions_test <- predict(gbm_model, newdata = newborn_test)
write.csv(predictions_test, file = "predictions_gbm_mode.csv", row.names = FALSE)


#param_grid <- expand.grid(n.trees = c(100, 200, 300),
                         # interaction.depth = c(3, 4, 5),
                          #shrinkage = c(0.01, 0.1, 0.2),
                          #bag.fraction = c(0.5, 0.6, 0.7))

param_grid <- expand.grid(
  n.trees = c(100, 200, 300),
  interaction.depth = c(2, 4, 6),
  shrinkage = c(0.01, 0.05, 0.1),
  n.minobsinnode = c(5, 10, 15)
)


gbm_model1 <- train(newborn_weight ~ .,
                   data = newborn_train_001_1 %>% dplyr::select(-father_age),
                   method = "gbm",
                   trControl = ctrl_cv5,
                   tuneGrid = param_grid,
                   verbose = FALSE)

predictions <- predict(gbm_model1, newdata = newborn_test_001_1)
absolute_percentage_errors_gbm1 <- abs((predictions - newborn_test_001_1$newborn_weight) / newborn_test_001_1$newborn_weight) * 100
mape <- mean(absolute_percentage_errors_gbm1)
Sys.time()
write.csv(predictions, file = "predictions_regression.csv", row.names = FALSE)
gbm_model1$bestTune

predictions_test <- predict(gbm_model1, newdata = newborn_test)
write.csv(predictions_test, file = "predictions_regression.csv", row.names = FALSE)

#15.41693
best_model <- getBest(gbm_model1$results, metric = "RMSE")
gbm_model1$results

best_model <- selectBest(gbm_model1, metric = "RMSE")




#param_grid2 <- expand.grid(
#  n.trees = c(50,70,100,150, 170,200,250, 300,350,400),
#  interaction.depth = c(2, 4, 6,8),
#  shrinkage = c(0.01, 0.05, 0.1,0.15),
#  n.minobsinnode = c(5, 10, 15,20)
#)


#gbm_model2 <- train(newborn_weight ~ .,
#                    data = newborn_train_001_1 %>% dplyr::select(-father_age),
#                    method = "gbm",
##                    trControl = ctrl_cv5,
#                    tuneGrid = param_grid2,
#                    verbose = FALSE)

#predictions <- predict(gbm_model2, newdata = newborn_test_001_1)
#absolute_percentage_errors_gbm2 <- abs((predictions - newborn_test_001_1$newborn_weight) / newborn_test_001_1$newborn_weight) * 100
#mape <- mean(absolute_percentage_errors_gbm2)
#Sys.time()







# #KNN
# load("E:\\AAA Warsaw University\\The 2nd semester\\Machine Learning 1\\_assessment-20230512T103446Z-001\\hushuai\\ML_train.RData")
# 
# set.seed(123)
# percentage_1 <- 0.01
# sample_size_0_0_1 <- round(percentage_1 * nrow(newborn_train)) 
# newborn_train_0_0_1 <- newborn_train[sample(nrow(newborn_train), size = sample_size_0_1), ]
# 
# options(contrasts = c("contr.treatment",  
#                       "contr.treatment"))
# newborn_train_0_0_1_train <- createDataPartition(newborn_train_0_0_1$newborn_weight, # target variable
                                                # share of the training sample
                                                #p = 0.7, 
                                                # should result be a list?
                                                #list = FALSE) 
 
# newborn_train_2 <- newborn_train_0_0_1[newborn_train_0_0_1_train,]
# newborn_test_2 <- newborn_train_0_0_1[-newborn_train_0_0_1_train,]
# summary(newborn_train_2)
# summary(newborn_test_2)
# 
 #ctrl_cv3 <- trainControl(method = "cv",
                          #number = 3)
# 
 #different_k <- data.frame(k = seq(1, 100, 2))
# 
# 
# KNN_model <- 
   #train(newborn_weight ~ .,
         
         #data = newborn_train_1 %>% 
           #dplyr::select(-father_age), 
        # method ="knn",
        # trControl = ctrl_cv3,
         #tuneGrid = different_k,
         #preProcess = c("range")
  # )
 #predictions <- predict(KNN_model, newdata = newborn_test_1)
 #absolute_percentage_errors_rf <- abs((predictions - newborn_test_1$newborn_weight) / newborn_test_1$newborn_weight) * 100
 #mape <- mean(absolute_percentage_errors_rf)
# 
# 
# 
# KNN_model




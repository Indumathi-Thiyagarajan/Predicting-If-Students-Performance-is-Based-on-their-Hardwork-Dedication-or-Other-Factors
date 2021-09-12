
install.packages("readr")
library("readr")
install.packages("tidyverse")
library("tidyverse")
stu.perf<- read.csv("E:/MSITM/ISM 645/Final exam/student_performance.csv")
str(stu.perf)
glimpse(stu.perf)

set.seed(123)

install.packages("ggplot2")
library("ggplot2")
library("dplyr")

#Do grades of Mathematics and Portuguese differ by gender?

plotdf <-stu.perf %>% select(subject,total_grade,sex,higher,studytime, internet)%>% filter(subject == "Mathematics" || subject == "Portugese")
ggplot(plotdf, aes(subject, total_grade)) +
  geom_col()+
  facet_wrap(~sex)+
  labs(title = "Grades of students in subject mathematics and portuguese by gender", x = "Subject Name", y = "Grades", size = 3)
#Yes, I agree grades of Mathematics and Portuguese differ by gender

#Is a student's motivation for higher education in the future important for current performance of Mathematics and Portuguese?

ggplot(plotdf , aes(higher, total_grade)) +
  geom_col()+
  labs(title = "Student's motivation for higher education ", x = "Depends on Current performance on Maths and Portuguese?", y = "Grades", size = 3)
#So i predict from plot that Students motivation for higher education in the future is important for current performance of Mathematics and Portuguese

#Does study time really matter to get better grade?
ggplot(plotdf, aes(studytime, total_grade))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE)+
  labs(title = "Dependence of study time over grade", x = "Study time in hours", y = "Grades", size = 3)

#Yes, study time really matter to get better grade as the more hours student spend , their grades increases. 

#Does access to Internet influence students' performance?
ggplot(plotdf, aes(internet, total_grade))+
  geom_col()+
  labs(title = "Influence of Internet on students' performance", x = "Accessible to internet ?", y = "Grades", size = 3)



#Yes, Influence of Internet on students' performance is higher

#==================================================================



#====================================

#------------------ New Student's Information
#------------------ traveltime = 2
#------------------ studytime = 4
#------------------ freetime = 3
#------------------ goout = 2

#Split the data into two separate data frames for each subject: Mathematics and Portuguese. Then, remove a variable "subject".

install.packages("tidyr")
library(tidyr)

Mathematics_df <- stu.perf%>%
  filter(subject == "Mathematics")%>%
  select(., -subject)

head(Mathematics_df)

Portuguese_df <- stu.perf%>%
  filter(subject == "Portuguese")%>%
  select(., -subject)
head(Portuguese_df)

#Make linear regression models to predict total_grade of Mathematics and Portuguese, respectively, based on time spending patterns (travel time, study time, free time after school, and going out with friends)

Mathematics_lr <- lm(total_grade ~ traveltime+studytime+freetime+goout, data = Mathematics_df)
summary(Mathematics_lr)


Portuguese_lr <- lm(total_grade ~ traveltime+studytime+freetime+goout, data = Portuguese_df)
summary(Portuguese_lr)

#Based on your model, make prediction for a new student's final grades of Mathematics and Portuguese, based on the following information.


new_student_infor <- data.frame(traveltime = 2,studytime = 4,freetime = 3,goout = 2)

New_stu_mathematics <- predict(Mathematics_lr, newdata = new_student_infor)
New_stu_portuguese <- predict(Portuguese_lr, newdata = new_student_infor)

New_stu_mathematics
New_stu_portuguese

#New students grade in Mathematics = 35.872, Portuguese = 39.785

#===================================================================



#Create a new binary variable (factor type) "pass" defined as 1 if total_grade > 30 and 0 otherwise. Then, remove the variable "total_grade".
head(Mathematics_df)

library(dplyr)

pass_df <-Mathematics_df%>%  mutate(pass = ifelse(total_grade > 30, "1","0"))%>% mutate_if(is.character,as.factor)%>% select(.,-total_grade)

#Split data into 70% train and 30% test data.
install.packages("caTools")
library(caTools)
set.seed(123)

split = sample.split(pass_df, SplitRatio = 0.7)
train_lg <- subset(pass_df, split== TRUE)
test_lg <- subset(pass_df, split == FALSE)


#Based on train data, make a logistic regression model to predict "pass" using all available predictor variables.

pass_lg_train<- glm(pass ~ ., data = train_lg,  family = "binomial")


#Based on test data, make a ROC curve and calculate AUC for the model trained
install.packages("broom")
library(broom)
pass_lg_test<- glm(pass ~ ., data = train_lg,  family = "binomial")%>%
  augment(type.predict ="response", newdata = test_lg)

install.packages("pROC")
library("pROC")

Roc <-roc(test_lg $pass , pass_lg_test$.fitted)
plot(Roc)
auc(Roc)

#0.7303

#===================================================================


#Based on train data, build a random forest model to predict "pass" using all available predictor variables.
install.packages("randomForest")
library("randomForest")

random_forest <- randomForest(pass ~ ., data = train_lg, ntree=500, importance = TRUE)

importance(random_forest)
varImpPlot(random_forest)

#Based on test data, make a ROC curve and calculate AUC for the model trained.


library(dplyr)
predict_rf <- random_forest %>% predict(type = "prob", newdata = test_lg)

predict_rf

library(pROC)
ROC_RF <- roc(test_lg$pass, predict_rf[,2])
plot(Roc)
lines(ROC_RF, col = "green")
auc(Roc)
auc(ROC_RF)

#Auc value for Logistic regression model = 0.7303
#Auc value for random forest model = 0.7428
#Based on the AUC value for both models, we can predict Random forest is better model for predicting this data. 

#===================================================================


# One might argue that background of parents and family structure are important in predicting children's performance. 
#     Based on your analysis, do you agree or disagree with this argument?
#     Note that parents and family-related variables include Pstatus, Medu, Mjob, Fedu, Fjob, guardian, famsize, famrel, and famsup.


head(pass_df)
library(randomForest)

train_family <- train_lg %>%
  select(Pstatus, Medu, Mjob, Fedu, Fjob, guardian, famsize, famrel,famsup,pass)

rf_family<- randomForest(pass ~ ., data = train_family, importance=TRUE)

predict_rf_acid <- rf_family %>%
  predict(newdata = test_lg, type = "prob")

roc_rf_family <- roc(test_lg$pass, predict_rf_acid[, 2])

plot(ROC_RF, col = "red")
lines(roc_rf_family, col = "green")

auc(ROC_RF)
auc(roc_rf_family)

#AUC when including all variables is 0.755, Auc when creating model for parents and family related variable AUC is 0.5801
# AUC is higher with all variables included 
#Also the Mean decrease accurancy does not have parents and family related variables as most important factors.
#So I conclude that Parents and family related informations are not important in predicting the students performance. 

#===================================================================


#====================================

#Predictive application that can access the real time data, where teachers can update and adapt courses to students requirements. 
#These application that allows teachers to measure, monitor and respond to students needs by tracing students' performance, targeting actions to improve their 
#learning process thereby helping them to address their needs before the final exams. Each student has varied level of knowledge, 
#Predictive analytics application that aids teachers to help students choose courses based on their knowledge and skill thereby helping
#them with the materials curated especially for the students based on their knowledge. This will improve student's interest towards each subject. 
#This way teachers can highly engage with students and help with their performance and provide better education. 
# These data can be allowed to be accessed by government, Thereby government would know
#the performance of the school and also helps government to see if the syllabus are curated to industry needs and current trends. 
#This way the quality of education provided by school, Teachers ability to monitor and respond to student needs and 
#students ability to get updated knowledge be improved.


#===================================================================  
# Predicting If Students Performance is Based on their Hardwork Dedication or Other emographic, social and school related features
 prediction model to predict students' performance in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features that were collected by using school reports and questionnaires. 

# Through Data Visualization in GGPLOT, I conclude that.

  Grades of Mathematics and Portuguese differ by gender
  Student's motivation for higher education in the future is important for current performance of Mathematics and Portuguese
  Study time really matter to get better grade as the more hours student spend , their grades increases. 
  Influence of Internet on students' performance is higher

# We Build two model Logistic Regression Model and Random Forest Model.
  Auc value for Logistic regression model = 0.7303
  Auc value for random forest model = 0.7428
  Based on the AUC value for both models, we can predict Random forest is better model for predicting this data. 


# Build a Random Forest Model in determining if students background of parents and family structure are important in predicting children's performance. 
  AUC when including all variables is 0.755, Auc when creating model for parents and family related variable AUC is 0.5801
  AUC is higher with all variables included 
  Also the Mean decrease accurancy does not have parents and family related variables as most important factors.
  So I conclude that Parents and family related informations are not important in predicting the students performance. 

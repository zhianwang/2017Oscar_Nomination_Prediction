####Programming Final Project
##Predicting Best picture nomination for the year 2017

setwd("D:/0. MSBA_Courses/Github/OscarNominationPrediction-master/3 Logistic Regression code")

#Reading the training, validation & testing dataset
movie_train<-read.csv("movie_train.csv", header=TRUE)
movie_val<-read.csv("movie_val.csv",header=TRUE)
movie_test<-read.csv("movie_test.csv", header=TRUE)

#The movies being nominated should have a minimum duration of 40 minutes
#so checking if there are any movies which doesnt satisfy our condition
length(movie_train[movie_train$duration<=40,])
length(movie_val[movie_val$duration<=40,])
length(movie_test[movie_test$duration<=40,])
#we see that none of the movies have duration less than 40

#we see that budget value is null for many of the variables and 
#after analyzing the train data set we come to a conclusion that budget is around 50% of the gross value 

length(movie_train$budget[is.na(movie_train$budget)])
#there are no observations without the budget value

#if we have missing data for budget, we replace the missing budget values with 50% of the gross value 
movie_train$budget[is.na(movie_train$budget)] <- movie_train$gross[is.na(movie_train$budget)]*0.50

#now checking if there are any missing budget values in the validation data
length(movie_val$budget[is.na(movie_val$budget)])

#Similarly applying the strategy for the test dataset
length(movie_test$budget[is.na(movie_test$budget)])

#there are 6 observations without the budget value

#now replacing the missing budget values with 60% of the gross value in the test set
movie_test$budget[is.na(movie_test$budget)]<- movie_test$gross[is.na(movie_test$budget)]*0.6

# now looking at the data
movie_train
str(movie_train)

movie_val
str(movie_val)

movie_test
str(movie_test)

#we need to convert the nominated column to categorical
movie_train$nominated<- as.factor(movie_train$nominated)
str(movie_train)

## performing stepwise regression
library(MASS)
model<-glm(nominated~duration+gross+budget+facebook_likes+average_rating+sentiment_score,family=binomial(link='logit'),data=movie_train)
stepwise_reg<-stepAIC(model,direction="both")
stepwise_reg$anova

#now we need to build the logistic regression

model<- glm(nominated~duration+gross+budget+facebook_likes+average_rating,family=binomial(link='logit'),data=movie_train)
summary(model)


### performing SLR
model<- glm(nominated~duration,family=binomial(link='logit'),data=movie_train)
summary(model)

model<- glm(nominated~gross,family=binomial(link='logit'),data=movie_train)
summary(model)


model<- glm(nominated~budget,family=binomial(link='logit'),data=movie_train)
summary(model)


model<- glm(nominated~facebook_likes,family=binomial(link='logit'),data=movie_train)
summary(model)

model<- glm(nominated~average_rating,family=binomial(link='logit'),data=movie_train)
summary(model)


model<- glm(nominated~sentiment_score,family=binomial(link='logit'),data=movie_train)
summary(model)

#anova(model,test = "Chisq")

#checking the R2 value for the Logistic Regression
library(pscl)
pR2(model)

#now predicting the nominations for validation data and finding the missclassification rate

library(ROCR)
new_nominations<-predict(model,newdata = movie_val,type = "response")
#fixed_nominations<-ifelse(new_nominations>=0.5,1,0)
#table(fixed_nominations)
#if there are less than 8 movies with probability greater than 0.5 then we choose the top 8 movies with highest probability 
ordered_nomination<- order(-new_nominations)
fixed_nominations=ifelse(new_nominations>0,0,1)
for (i in ordered_nomination[1:8]){
fixed_nominations[i]=1  
} 
table(fixed_nominations)
movie_val$Predictions<-fixed_nominations

confusion_matrix<-table(movie_val$Predictions,movie_val$nominated)
confusion_matrix

misclass_rate<-mean(movie_val$Predictions!=movie_val$nominated)
misclass_rate


#now plotting the roc curve and finding the area under the curve (AUC)

library(ROCR)
pred<-prediction(fixed_nominations,movie_val$nominated)
roc<-performance(pred,measure="tpr",x.measure="fpr")
plot(roc,main="ROC Curve")

#Now finding the AUC value
auc<- performance(pred,measure='auc')
auc@y.values[[1]]

#now plotting the lift curve
lift_curve<-performance(pred,"lift","rpp")
plot(lift_curve,main="Lift Curve",colorize=TRUE)
#the output of lift curve means that the chance of the
#movie being nominated is 2 times higher if we selected a movie from top 20% of the predicted probabilities  


#finding the movies which are nominated for the best picture for the year 2017
new_nominations_test<-predict(model,newdata = movie_test,type = "response")

#ordering the nomination predictions in descending order
ordered_nomination_test<-order(-new_nominations_test)
movie_test$nomination_prob<- new_nominations_test

#Assigning 0(Not Nominated) to all the movies at first 
fixed_nominations_test<-ifelse(new_nominations_test>=0,0,1)

#Now assigning 1(Nominated) to the Top 8 movies with highest probability of being nominated
for (i in ordered_nomination_test[1:8]){
  fixed_nominations_test[i]=1
}
table(fixed_nominations_test)

#Storing the predicted nominations in our actual test dataset
movie_test$Predictions<-fixed_nominations_test
str(movie_test)

#now listing the movies which our model thinks are going to be nominated
nomination_predicted<-movie_test[movie_test$Predictions==1,]

#Printing the list of movies which are predicted to be nominated for the best picture award
nomination_predicted$movie_name[order(-nomination_predicted$nomination_prob)]


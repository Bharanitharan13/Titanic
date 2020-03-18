  #####Logistic Regression###
###set directory###
setwd('E:\\missed classes\\Titanic')
getwd()

###import####
train<-read.csv('train.csv', header = TRUE, sep =',' ,
                na.strings = c("NA","NAN",""))
test<-read.csv('test.csv', header = TRUE, sep = ',', 
               na.strings = c("NA","NAN",""))


###audit###
str(train)

###converting###
train$Survived<-as.factor(train$Survived)
train$Pclass<-as.factor(train$Pclass)

summary(train)

###imputation###
train$Age<-ifelse(is.na(train$Age),median(train$Age, na.rm=TRUE),train$Age)
sum(is.na(train$Age))

train$Embarked<-as.character(train$Embarked)
is.character((train$Embarked))
summary(train)
train$Embarked<-ifelse(is.na(train$Embarked),'S',train$Embarked)
sum(is.na(train$Embarked))
train$Embarked<-as.factor(train$Embarked)
is.factor(train$Embarked)
sum(is.na(train$Embarked))
summary(train)

##skewness###
library(e1071)
skewness(train$Age)
boxplot(train$Age)

##Transform##
train$ln_age<-log(train$Age)
train$ln_fare<-log(train$Fare)
skewness(train$ln_age)
skewness(train$ln_fare)

train$ln_fare<-ifelse(train$ln_fare==-Inf,0,train$ln_fare)
skewness(train$ln_fare)

####chi square test####
table1<-table(train$Sex,train$Survived)
chisq.test(table1)
table2<-table(train$Pclass,train$Survived)
chisq.test(table2)
gtable2
table3<-table(train$Embarked,train$Survived)
chisq.test(table3)
table3

####in cor we pass only two variables, but to pass more than 2 use correlation matric###

library(Hmisc)
cormat<-subset(train,select=c('Age','SibSp','Parch','Fare'))
correlationmatrix<-rcorr(as.matrix(cormat))
cormat<-as.data.frame(correlationmatrix$r)
cormat

names(train)
model<- glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,family = 'binomial',data = train)
summary(model)


model_ln<- glm(Survived~Pclass+Sex+Age+SibSp+Parch+ln_fare+Embarked,family = 'binomial',data = train)
summary(model_ln)

train$probs<-model_ln$fitted.values

train$preditsurvice<-ifelse(train$probs>0.50,1,0)

confusionmatrix<-table(train$Survived,train$preditsurvice)
print(confusionmatrix)


summary(train)
##test###
###imputation###

str(test)
test$Pclass<- as.factor(test$Pclass)
summary(test)


test$Age<-ifelse(is.na(test$Age),median(test$Age, na.rm=TRUE),test$Age)
sum(is.na(train$Age))
test$Fare<-ifelse(is.na(test$Fare),median(test$Fare),na.rm=TRUE,test$Fare)
sum(is.na(train$Fare))

test$ln_fare<- log(test$Fare)

test$ln_fare<- ifelse(test$ln_fare==Inf,0,test$ln_fare)

test$Survived_Probs <- predict(model_ln, test, type = 'response')

test$Survived <- ifelse(test$Survived_Probs>=0.50, 1, 0)

write.csv(test,'final.csv')
names(test)
submission <- c('PassengerId','Survived')

submit<-test[submission]  

write.csv(submit,'submission.csv',row.names = FALSE)

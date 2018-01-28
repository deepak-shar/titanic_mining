#Make sure that the parameter na.strings is equal to c("") so that each missing 
#value is coded as a NA. This will help us in the next steps.
training.data.raw = read.csv('train.csv',header=T,na.strings=c(""))
head(training.data.raw)
#Now we need to check for missing values
sapply(training.data.raw,function(x) sum(is.na(x)))
#look how many unique values there are for each variable
sapply(training.data.raw, function(x) length(unique(x)))
#A visual take on the missing values might be helpful: the Amelia package
#has a special plotting function missmap() that will plot your dataset and
#highlight missing values:
if (!require("Amelia")) install.packages("Amelia")
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")
#The variable cabin has too many missing values, we will not use it. 
#We will also drop PassengerId since it is only an index and Ticket
data = subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))
head(data)
#Taking care of the missing values
#There are different ways to do this, a typical approach is to replace 
#the missing values with the average, the median or the mode of the 
#existing one. I'll be using the average
data$Age[is.na(data$Age)] = mean(data$Age,na.rm=T)
#For a better understanding of how R is going to deal with the categorical
#variables, we can use the contrasts() function. 
#This function will show us how the variables have been dummyfied by R 
#and how to interpret them in a model.
contrasts(data$Sex)
contrasts(data$Embarked)
#for the missing values in Embarked, since there are only two, we 
#will discard those two rows
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL
#Model fitting
#split data
train = data[1:800,]
test = data[801:889,]
#fit the model on training data
model = glm(Survived ~.,family=binomial(link='logit'),data=train)
#By using function summary() we obtain the results of our model:
summary(model)
#The difference between the null deviance and the residual deviance shows 
#how our model is doing against the null model (a model with only the intercept).
#The wider this gap, the better
#Now we can run the anova() function on the model to analyze 
#the table of deviance
anova(model, test="Chisq")
# we can add or reduce feature with less p values and see the impact on AIC
#AIC=akaik information criterion and model with lower AIC would be the preferred one
#it can be used only with comparison to other model

#Assessing the predictive ability of the model
fitted.results = predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)
misClasificError = mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

#plot ROC curve
library(ROCR)
p = predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr= prediction(p, test$Survived)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc

#completed































rm(list=ls())
x = c("ggplot2", "corrgram", "DMwR", "caret", "unbalanced", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
lapply(x, require, character.only = TRUE)
rm(x)
setwd("C:/Users/hp/Desktop/DATA SCIENTISTS")
getwd()
#Read the test and train data
santander_train=read.csv("train.csv", header=TRUE)
is.na(santander_train)

#Outlier analysis
#BoxPlots - Distribution and Outlier Check
numeric_index = sapply(santander_train,is.numeric) #selecting only numeric
numeric_data = santander_train[,numeric_index]
cnames = colnames(numeric_data)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "target"), data = subset(santander_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="target")+
           ggtitle(paste("Box plot of targeted for",cnames[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)
# #Remove outliers using boxplot method
df = santander_train
santander_train = df
val = santander_train$previous[santander_train$previous %in% boxplot.stats(santander_train$previous)$out]
#loop to remove from all variables
for(i in cnames){
  print(i)
  val = santander_train[,i][santander_train[,i] %in% boxplot.stats(santander_train[,i])$out]
  print(length(val))
  santander_train = santander_train[which(!santander_train[,i] %in% val),]
}
#Replace all outliers with NA and impute
#create NA
for(i in cnames){
  val = santander_train[,i][santander_train[,i] %in% boxplot.stats(santander_train[,i])$out]
  print(length(val))
  santander_train[,i][santander_train[,i] %in% val] = NA
}
santander_train_norm = subset(santander_train, 
                              select = -c(ID_code,target))
cnames = santander_train_norm
santander_train[is.na(santander_train)] = mean(santander_train, na.rm = T)

#Missing Value analysis
missing_val = data.frame(apply(santander_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(santander_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)
ggplot(data = missing_val[1:5,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()
#Mean Method
santander_train= data.frame(santander_train)
santander_train[is.na(santander_train)] = mean(santander_train, na.rm = T)
#Median Method
santander_train$var_44[is.na(santander_train$var_44)] = median(santander_train$var_44, na.rm = T)
write.csv(santander_train, 'santander_train_missing.csv', row.names = F)

#Feature selection
for(i in 1:ncol(santander_train)){
  
  if(class(santander_train[,i]) == 'factor'){
    
    santander_train[,i] = factor(santander_train[,i], labels=(1:length(levels(factor(santander_train[,i])))))
  }
}
numeric_index = sapply(santander_train,is.numeric)
numeric_index
corrgram(santander_train[,numeric_index], order = F, upper.panel = panel.pie, 
         text.panel=panel.txt, main = "Correlation Plot")
## Chi-squared Test of Independence
factor_index = sapply(santander_train,is.factor)
factor_data = data.frame(santander_train$target)
table(factor_data$santander_train.target,factor_data$target)
print(colnames(factor_data))
print(chisq.test(table(factor_data$santander_train.target,factor_data)))
## Dimension Reduction
#Feature scaling
#Normality check
qqnorm(santander_train_norm$var_111)
hist(santander_train_norm$var_111)

#Normalisation
santander_train_norm = subset(santander_train, 
                         select = -c(ID_code,target))
cnames = santander_train_norm
for(i in cnames){
  print(i)
  santander_train_norm[,i] = (santander_train[,i] - min(santander_train_norm[,i]))/
    (max(santander_train_norm[,i] - min(santander_train_norm[,i])))
}
#Standardisation
 for(i in cnames){
   print(i)
   santander_train[,i] = (santander_train_norm[,i] - mean(santander_train_[,i]))/
                                  sd(santander_train_norm[,i])
 }


#Develop training and test data
train_index= sample(1:nrow(santander_train), 0.8* nrow(santander_train))
training=santander_train[train_index,]
test= santander_train[-train_index,]


#Logistic Regression
logit_model = glm(target ~ ., data = training, family = "binomial")
#summary of the model
summary(logit_model)
#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$target, logit_Predictions)
#False Negative rate
FNR = FN/FN+TP 
#Accuracy: 89.54
#FNR: 52.37
#Recall: 48.53
#Alkaline info criteria: 1245.7

##KNN Implementation
library(class)
#Predict test data
KNN_Predictions = knn(training[, 3:202], test[, 3:202], training$target, k = 7)
#Confusion matrix
Conf_matrix = table(KNN_Predictions, test$target)
#Accuracy
sum(diag(Conf_matrix))/nrow(test)
#False Negative rate
FNR = FN/FN+TP 
#Accuracy: 90.64
#FNR: 61.09
#Recall: 39.91


#naive Bayes
library(e1071)
#Develop model
NB_model = naiveBayes(target ~ ., data = training)
#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,3:202], type = 'class')
#Look at confusion matrix
Conf_matrix = table(observed = test[,2], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)
#Accuracy: 92.16
#FNR: 42.71
#Recall: 58.29

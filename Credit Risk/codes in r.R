library('xgboost') #XGBoost
library('readr') #fast and friendly way to read rectangular data 
library('stringr') # Character manipulation: these functions allow you to manipulate the individual characters inside the strings inside character vectors.
library('caret') #to streamline the model training process for complex regression and classification problems
library('car')# Applied regression
library('ggplot2') #Elegant data viz 
library('ggthemes') #Extra theme
library('scales') #Scale function 
library('dplyr') #Gramar data manip 
library('mice') #Multivariate Imputation by Chained Equation
library('randomForest') #Random forest
library('corrplot') # correlation plot 
library('knitr') #Dynamic report
library('Amelia') #missmap
library('ROCR') #Performance
library('psych') #Research 
library('magrittr') #Forward pipe
library('lattice') #Trellis Graphics
library('ROSE') #Random Over Sampling Examples
library('cvAUC') #CrossValidated Area
library('DiagrammeR') # Diagram
library('Ckmeans.1d.dp') #Fast clustering


rm(list = ls())
myData<-read.csv("C:/Users/sanjay/Desktop/4th sem/projects/Credit-Risk-Model-master/train.csv")
myTest<-read.csv("C:/Users/sanjay/Desktop/4th sem/projects/Credit-Risk-Model-master/test.csv")


head(myData)
head(myTest)



myData$X<-NULL
myTest$X<-NULL

dim(myData)
nrow(myData)
ncol(myData)
names(myData)
summary(myData)


sapply(myData,function(x) sum(is.na(x)))

missmap(myData, legend = TRUE, col = c("white","firebrick2"), main= "Missing values vs observed from Training set",yaxt='n')



sum(is.na(myData$NumberOfDependents))
summary(myData$NumberOfDependents)
head(myData$NumberOfDependents,50)
table(myData$NumberOfDependents)
myData$NumberOfDependents[is.na(myData$NumberOfDependents)] <- median(myData$NumberOfDependents,na.rm=T)
myData$NumberOfDependents<-ifelse(myData$NumberOfDependents>10,median(myData$NumberOfDependents),myData$NumberOfDependents)



par(lend="butt")
table(myData$SeriousDlqin2yrs)
rf <- sum(myData$SeriousDlqin2yrs == 0, na.rm = TRUE) #Person no experienced 90 days past due delinquency or worse 
rf
rt <- sum(myData$SeriousDlqin2yrs == 1, na.rm = TRUE) #Person experienced 90 days past due delinquency or worse
rt
n <- rf + rt

dst <- density(myData$NumberOfDependents, na.rm = TRUE)
dst
head(dst$y,10)
dstg <- density(myData$NumberOfDependents[myData$SeriousDlqin2yrs == 0], na.rm = TRUE)
dstg
dstf <- density(myData$NumberOfDependents[myData$SeriousDlqin2yrs == 1], na.rm = TRUE)
dstf

hist(myData$NumberOfDependents, col = grey(0.5), border = grey(0.2),
     main = paste("Number of dependents"),
     xlab = "Number",
     proba = TRUE, ylim = c(0, max(dst$y)+0.002))

lines(dstg$x, rf/n*dstg$y, lwd = 3, col = "blue")
lines(dstf$x, rf/n*dstf$y, lwd = 3, col = "red")
lines(dst$x, dst$y)
legend("topright", inset = 0.01, legend = c("Delinquency", "No delinquency","Total"),
       col = c("red","blue","black"),
       lty = c(1, 1,1), lwd = 2, pt.cex = 2)







########NumberRealEstateLoansOrLines cleaner
summary(myData$NumberRealEstateLoansOrLines)
sum(is.na(myData$NumberRealEstateLoansOrLines))
table(myData$NumberRealEstateLoansOrLines)
myData$NumberRealEstateLoansOrLines<-ifelse(myData$NumberRealEstateLoansOrLines==54,median(myData$NumberOfOpenCreditLinesAndLoans),myData$NumberRealEstateLoansOrLines)
par(lend="butt")
rf <- sum(myData$SeriousDlqin2yrs == 0, na.rm = TRUE)
rf#Person no experienced 90 days past due delinquency or worse 
rt <- sum(myData$SeriousDlqin2yrs == 1, na.rm = TRUE) 
rt#Person experienced 90 days past due delinquency or worse
n <- rf + rt
dst <- density(myData$NumberRealEstateLoansOrLines, na.rm = TRUE)
dst
hist(myData$NumberRealEstateLoansOrLines, col = grey(0.5), border = grey(0.9),
     main = paste("Number of real estate loans or lines"),
     xlab = "Number",
     proba = TRUE, ylim = c(0, max(dst$y)+0.002))
lines(dst$x, dst$y,col="blue")






#########NumberOfOpenCreditLinesAndLoans cleaner
sum(is.na(myData$NumberOfOpenCreditLinesAndLoans))
summary(myData$NumberOfOpenCreditLinesAndLoans)
#Replace by median (more than 15 is strange)
myData$NumberOfOpenCreditLinesAndLoans<-ifelse(myData$NumberOfOpenCreditLinesAndLoans>15,median(myData$NumberOfOpenCreditLinesAndLoans),myData$NumberOfOpenCreditLinesAndLoans)

boxplot(myData$NumberOfOpenCreditLinesAndLoans,col = "blue")





#########Monthly Income cleaner
sum(is.na(myData$MonthlyIncome))
summary(myData$MonthlyIncome)
#If salary > 100 000 (senseless)
myData$MonthlyIncome[is.na(myData$MonthlyIncome)]<-median(myData$MonthlyIncome,na.rm = TRUE)
myData$MonthlyIncome[(myData$MonthlyIncome)>100000]<-median(myData$MonthlyIncome,na.rm = TRUE)

par(lend="butt")
rf <- sum(myData$SeriousDlqin2yrs == 0, na.rm = TRUE) #Person no experienced 90 days past due delinquency or worse 
rt <- sum(myData$SeriousDlqin2yrs == 1, na.rm = TRUE) #Person experienced 90 days past due delinquency or worse
n <- rf + rt
dst <- density(myData$MonthlyIncome, na.rm = TRUE)
dstg <- density(myData$MonthlyIncome[myData$SeriousDlqin2yrs == 0], na.rm = TRUE)
dstf <- density(myData$MonthlyIncome[myData$SeriousDlqin2yrs == 1], na.rm = TRUE)
hist(myData$MonthlyIncome, col = grey(0.9), border = grey(0.8),
     main = paste("Monthly income"),
     xlab = "Income",
     proba = TRUE, ylim = c(0, max(dst$y)))
lines(dstg$x, rf/n*dstg$y, lwd = 3, col = "blue")
lines(dstf$x, rf/n*dstf$y, lwd = 3, col = "red")
lines(dst$x, dst$y)
legend("topright", inset = 0.01, legend = c("Delinquency", "No delinquency","Total"),
       col = c("red","blue","black"),
       lty = c(1, 1,1), lwd = 2, pt.cex = 2)




#########Dept Ratio cleaner
summary(myData$DebtRatio)
sum(is.na(myData$DebtRatio))
range(myData$DebtRatio)
#Replace by median
myData$DebtRatio<-ifelse(myData$DebtRatio<0,median(myData$DebtRatio),myData$DebtRatio)
myData$DebtRatio<-ifelse(myData$DebtRatio>1,median(myData$DebtRatio),myData$DebtRatio)
boxplot(myData$DebtRatio, notch=TRUE, 
        col="gold",
        main="Dept ratio")



########Age cleaner
sum(is.na(myData$age))
summary(myData$age)
#Histograme of age
par(lend="butt")
rf <- sum(myData$SeriousDlqin2yrs == 0, na.rm = TRUE) #Person no experienced 90 days past due delinquency or worse 
rt <- sum(myData$SeriousDlqin2yrs == 1, na.rm = TRUE) #Person experienced 90 days past due delinquency or worse
n <- rf + rt
dst <- density(myData$age, na.rm = TRUE)
dstg <- density(myData$age[myData$SeriousDlqin2yrs == 0], na.rm = TRUE)
dstf <- density(myData$age[myData$SeriousDlqin2yrs == 1], na.rm = TRUE)
hist(myData$age, col = grey(0.9), border = grey(0.8),
     main = paste("Age of ", nrow(myData), " clients"),
     xlab = "Age [years]",
     proba = TRUE, ylim = c(0, max(dst$y)+0.002))
lines(dstg$x, rf/n*dstg$y, lwd = 3, col = "blue")
lines(dstf$x, rf/n*dstf$y, lwd = 3, col = "red")
lines(dst$x, dst$y)
legend("topright", inset = 0.01, legend = c("Delinquency", "No delinquency","Total"),
       col = c("red","blue","black"),
       lty = c(1, 1,1), lwd = 2, pt.cex = 2)


#######revolvingUtilizationOfUnsecuredLines cleaner
names(myData)

summary(myData$RevolvingUtilizationOfUnsecuredLines)
# It should be between 0 and 1. (%)
sum(is.na(myData$RevolvingUtilizationOfUnsecuredLines))
sum(myData$RevolvingUtilizationOfUnsecuredLines>1)
# Median (?)
myData$RevolvingUtilizationOfUnsecuredLines[myData$RevolvingUtilizationOfUnsecuredLines>1]=median(myData$RevolvingUtilizationOfUnsecuredLines)
#If <0 -> 0 | If >1 -> 1 
myData$RevolvingUtilizationOfUnsecuredLines<-ifelse(myData$RevolvingUtilizationOfUnsecuredLines<0,0,myData$RevolvingUtilizationOfUnsecuredLines)
myData$RevolvingUtilizationOfUnsecuredLines<-ifelse(myData$RevolvingUtilizationOfUnsecuredLines>1,1,myData$RevolvingUtilizationOfUnsecuredLines)

boxplot(myData$RevolvingUtilizationOfUnsecuredLines,col="green")





######NumberOfTime30.59DaysPastDueNotWorse cleaner
sum(is.na(myData$NumberOfTime30.59DaysPastDueNotWorse))
table(myData$NumberOfTime30.59DaysPastDueNotWorse)
#Absurd data (>50 for example)
myData$NumberOfTime30.59DaysPastDueNotWorse[myData$NumberOfTime30.59DaysPastDueNotWorse>=50]<-0
hist(myData$NumberOfTime30.59DaysPastDueNotWorse,col="blue")


########NumberOfTime60.89DaysPastDueNotWorse cleaner
sum(is.na(myData$NumberOfTime60.89DaysPastDueNotWorse))
table(myData$NumberOfTime30.59DaysPastDueNotWorse)
#Absurd data (>50 for example)
myData$NumberOfTime60.89DaysPastDueNotWorse[myData$NumberOfTime60.89DaysPastDueNotWorse>50]<-0
#Histogram
hist(myData$NumberOfTime60.89DaysPastDueNotWorse,col="grey")


########NumberOfTimes90DaysLate cleaner
sum(is.na(myData$NumberOfTimes90DaysLate))
summary(myData$NumberOfTimes90DaysLate)
table(myData$NumberOfTimes90DaysLate)
#More than 20 is non sense 
myData$NumberOfTimes90DaysLate<-ifelse(myData$NumberOfTimes90DaysLate>50,0,myData$NumberOfTimes90DaysLate)
#Histogram
hist(myData$NumberOfTimes90DaysLate,col="green")



















###    Information after data distributions & imputation #####

describeBy(myData)

#correlation
names(myData) <- c("Result", "DefaultLine","Age","DelayBetween3059","DebtRatio","MonthlyIncome","NbLoanCredit","DelaySup90","NbRealEstateLoansLines","DelayBetween6089","NumberOfDependents")
head(myData)
cor(myData)
M <- cor(myData)
corrplot(M, method = "square")

names(myData) <-c("SeriousDlqin2yrs","RevolvingUtilizationOfUnsecuredLines","age",
                  "NumberOfTime30.59DaysPastDueNotWorse","DebtRatio","MonthlyIncome",
                  "NumberOfOpenCreditLinesAndLoans","NumberOfTimes90DaysLate",
                  "NumberRealEstateLoansOrLines","NumberOfTime60.89DaysPastDueNotWorse")




########building models  ########

#Histogram of Delinquency 2 years
h <- hist(myData$SeriousDlqin2yrs,breaks=2, col = c("blue", "red"), border = grey(0.8),main='Y/N of delincincy',xlab = 'Person experienced 90 days past due delinquency or worse', las = 1, xlim = c(0, 1),ylim=c(0,150000))#,probability = T)
text(h$mids,h$counts,labels=paste((h$counts/150000)*100, "%"), adj=c(0.5, -0.5))
legend("topright", inset = 0.01, legend = c("No Delinquency", "Delinquency"), col = c("blue","red"), lty = c(1, 1), lwd = 2, pt.cex = 2)





#80-20 split
newMyData<-myData[myData$SeriousDlqin2yrs==1,]
DownsampleData<-myData[myData$SeriousDlqin2yrs==0,]
downsam<-sample(1:139974,11000)


nDat<-rbind(newMyData,DownsampleData[downsam,])
head(nDat)

nDat<-nDat[sample(nrow(nDat)),]
rownames(nDat)<-NULL

set.seed(36)
trainIndex <- createDataPartition(nDat$SeriousDlqin2yrs, p = .8,list = FALSE, times = 1)
head(trainIndex)
ntrain<-nDat[trainIndex,]
ntest<-nDat[-trainIndex,]

ntrain.gbm<-ntrain


#Panel pair
panel.cor <- function(x, y, digits=2, prefix="", cex.cor,col) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- cor(x, y) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 10 
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  
  text(0.5, 0.5, txt, cex = cex * 0.2,
       col = if(r>0 & r<0.333) 'lightblue'
       else if(r>0.333 & r<0.666) 'skyblue3'
       else if(r>0.666 & r <1) 'dodgerblue'
       else if(r<0 & r>-0.333) 'salmon'
       else if(r>=-0.666 & r<0.333) 'tomato'
       else if(r>=-1 & r <-0.666) 'red') 
}

pairs(myData[1:100,], upper.panel=panel.cor)


#Conditional Distribution
# melting the data frame
feature.names<-names(nDat)[-1]

vizDat<- melt(nDat,id.vars = 'SeriousDlqin2yrs'
              ,measure.vars = feature.names, variable.name = "Feature"
              ,value.name = "Value")

# conditional box plots for each feature on the response variable
p <- ggplot(data = vizDat, aes(x=Feature, y=Value)) + 
  geom_boxplot(aes(fill=SeriousDlqin2yrs))
p <- p + facet_wrap( ~ Feature, scales="free")
p + ggtitle("Conditional Distributions of each variable")

nDat.s<- scale(nDat[,-1],center=TRUE, scale=TRUE)
Dat.pc<-prcomp(nDat.s)
summary(Dat.pc)

# pr1 and pr2 only explain 36% of the variance but still 
# lets look at the distribution of for fun

plot(Dat.pc$x[,1:2],col=as.factor(nDat[,1]))


#Eigen
x_scaled = scale(myData)
cov_matrix = cov(x_scaled)
eigen(cov_matrix)

cor_matrix = cor(x_scaled)

eigen(cov_matrix)$value 
eigen(cor_matrix)$value 
eigen(cov(myData))$value
eigen(cor(myData))$value 

iev = eigen(cov(x_scaled))$values/sum(eigen(cov(x_scaled))$values)
cev = cumsum(iev)
plot(iev,type="b")

#Biplot
A = eigen(cov_matrix)$vector[,c(1,2)]
Y= as.matrix(x_scaled) %*% A

plot(Y,xlab="PC1",ylab="PC2",col = c("red","blue")[(myData$SeriousDlqin2yrs==0)||(myData$SeriousDlqin2yrs==1)])

pca <- princomp(x_scaled)

biplot(pca)





########Model Logistic regression########
logistic.model<-glm(SeriousDlqin2yrs~.,data = myData,family = binomial)
summary(logistic.model)
set.seed(123)
# ROC and AUROC
pred.logi.model<-predict(logistic.model,myData,type='response')
pr <- prediction(pred.logi.model, myData$SeriousDlqin2yrs)

prf0 <- performance(pr, measure = "tpr", x.measure = "fpr")
par(mfrow = c(2,2)) 
plot(logistic.model)


auclr <- performance(pr, measure = "auc")
auclr <- auclr@y.values[[1]]
auclr


##### Random forest   #####
names(ntrain)<-c('SeriousDlqin2yrs','F1','F2','F3','F4','F5'
                 ,'F6','F7','F8','F9','F10')
names(ntest)<-c('SeriousDlqin2yrs','F1','F2','F3','F4','F5'
                ,'F6','F7','F8','F9','F10')
set.seed(123)
ntrain$SeriousDlqin2yrs = as.factor(ntrain$SeriousDlqin2yrs)
ntest$SeriousDlqin2yrs = as.factor(ntest$SeriousDlqin2yrs)
RF <- randomForest(ntrain[,-c(1,6,10,11)],ntrain$SeriousDlqin2yrs
                   ,sampsize=c(10000),do.trace=TRUE,importance=TRUE,ntree=500,forest=TRUE)
varImpPlot(RF)

pred <- data.frame(predict(RF,ntest[,-c(1,6,10,11)]))
plot(RF)

#RF$err.rate[,3][500]
pred.forest<-predict(RF,newdata = ntest[,-1],'prob')
plot(pred.forest)

output<-pred.forest[,2]
pr <- prediction(output, ntest$SeriousDlqin2yrs)
prf1 <- performance(pr, measure = "tpr", x.measure = "fpr")
aucrf <- performance(pr, measure = "auc")
aucrf <- aucrf@y.values[[1]]
aucrf


### better random forest XG Boost ####
ntrain.gbm<-ntrain
dtrain <- xgb.DMatrix(data = as.matrix(ntrain[,-1])
                      , label = as.numeric(ntrain$SeriousDlqin2yrs)-1)

dtest<- xgb.DMatrix(data = as.matrix(ntest[,-1])
                    , label = as.numeric(ntest$SeriousDlqin2yrs)-1)

watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max.depth=3
                 , eta=0.01, nthread = 2, nround=2000
                 , watchlist=watchlist, eval.metric = "error"
                 , eval.metric = "logloss"
                 , objective = "binary:logistic")

print(xgb.importance(model = bst))

xgb.plot.importance(importance_matrix = xgb.importance(model = bst))

pred.xg<-predict(bst,dtest)
pr <- prediction(pred.xg, ntest$SeriousDlqin2yrs)
prf5 <- performance(pr, measure = "tpr", x.measure = "fpr")

auc <- performance(pr, measure = "auc")
auc
auc <- auc@y.values[[1]]
auc

########Conclusion#######
auclr #0.8481
aucrf #0.8472
auc   #0.8582
plot(prf0, col = "blue") #black logistic model 
plot(prf1, add = T, col = "red" ) #random forest
plot(prf5, add = T, col = "green") #xg boost 



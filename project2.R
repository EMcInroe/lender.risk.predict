library(readr)
lending_club_loan_two <- read_csv("Statistics/STAT 508/Project2/lending_club_loan_two.csv/lending_club_loan_two.csv")
summary(lending_club_loan_two)

## Create index column to keep track of observations
lending_club_loan_two$index <- 1:nrow(lending_club_loan_two)
lending_club_loan_two<-na.omit(lending_club_loan_two)
summary(lending_club_loan_two$loan_status)

#### Get a smaller sample size for computations
set.seed(508)
samp<-sample(335868, size=10000)
lend<-data.frame(lending_club_loan_two[samp,])
summary(lend)

#set index column as row names
rownames(lend) <- lend$index

#remove original Index and address columns from data frame
lend$index<- NULL
lend$address<-NULL
summary(lend)
View(lend)


## Remove emp_title because it is to variable
lend$emp_title<-NULL

##############
####  EDA ####
##############
library(psych)
attach(lend)

sum.lend<-describe(lend[,c(1,3,4,9,15,17:21, 24,25)], IQR=TRUE)
print(sum.lend, digits=2)
write.csv(sum.lend, "Statistics/STAT 508/Project2/sum.lend.csv")

margin.table(prop.table(table(grade)),1)
margin.table(prop.table(table(sub_grade)),1)
margin.table(prop.table(table(emp_length)),1)
margin.table(prop.table(table(home_ownership)),1)
margin.table(prop.table(table(verification_status)),1)
margin.table(prop.table(table(loan_status)),1)
margin.table(prop.table(table(purpose)),1)
margin.table(prop.table(table(earliest_cr_line)),1)
margin.table(prop.table(table(initial_list_status)),1)
margin.table(prop.table(table(application_type)),1)

## applicatin type removed due to no variation
lend$application_type<-NULL

## delete subgrade, too many levels
lend$sub_grade<-NULL

##histograms
par(mfrow=c(2,3))
hist(loan_amnt, main="")
hist(int_rate, main="")
hist(installment, main="")
hist(annual_inc,main="")
hist(dti, main="")
hist(open_acc, main="")
hist(pub_rec, main="")
hist(revol_bal,main="")
hist(revol_util, main="")
hist(total_acc, main="")
hist(mort_acc, main="")
hist(pub_rec_bankruptcies, main="")

## Boxplots
par(mfrow=c(1,3))
boxplot(loan_amnt, xlab="Loan Amount")
boxplot(int_rate, xlab="Int. Rate")
boxplot(installment, xlab="installment")
boxplot(annual_inc, xlab="Annual Inc.")
boxplot(dti, xlab="dti")
boxplot(open_acc, xlab="Open Acc.")
boxplot(pub_rec, xlab="Pub Rec")
boxplot(revol_bal, xlab="revol bal")
boxplot(revol_util, xlab="revol util")
boxplot(total_acc, xlab="tot acc")
boxplot(mort_acc, xlab="mort acc")
boxplot(pub_rec_bankruptcies, xlab="bankruptcies")
par(mfrow=c(1,1))

summary(lend$loan_amnt[lend$loan_status=="Fully Paid"])
summary(lend$loan_amnt[lend$loan_status=="Charged Off"])
with(lend, plot(loan_status, loan_amnt, col = c("white", "grey60"), ylab = "Loan Amount",
                 xlab="", xaxt = "n"))
legend("top", inset = 0.02, legend = c("Charged Off", "Fully Paid"), fill = c("white",
                                                                  "grey60"), bg = "white")

with(lend, plot(loan_status, dti, col = c("white", "grey60"), ylab = "dti",
                xlab="", xaxt = "n"))
legend("top", inset = 0.02, legend = c("Charged Off", "Fully Paid"), fill = c("white",
                                                                              "grey60"), bg = "white")
with(lend, plot(loan_status, int_rate, col = c("white", "grey60"), ylab = "Interest Rate",
                xlab="", xaxt = "n"))
legend("top", inset = 0.02, legend = c("Charged Off", "Fully Paid"), fill = c("white",
                                                                              "grey60"), bg = "white")


## Correlation
library(ggplot2)
library(GGally)
ggcorr(lend[,c("loan_amnt", "int_rate","installment","annual_inc","dti","open_acc",
            "pub_rec","revol_bal","revol_util","total_acc","mort_acc","pub_rec_bankruptcies")])

## Contingency Tables
library(gmodels)
CrossTable(loan_status, term, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(loan_status, grade, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(loan_status, emp_length, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(loan_status, home_ownership, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(loan_status, verification_status, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(loan_status, purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(loan_status, title, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(loan_status, initial_list_status, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

detach(lend)

## remove initial_list_status (No correlation with loan status)
lend$initial_list_status<-NULL

## remove obsevations with 'none' for home_ownership (2 obs removed)
lend<-subset(lend, home_ownership!="NONE")

## remove title variable, redundant and full or errors
lend$title<-NULL


## combine categories in 'purpose'

lend$Purpose_alt <- ifelse(lend$purpose ==  "car", "major_purchase",lend$purpose)
lend$Purpose_alt <- ifelse(lend$purpose == "house", "major_purchase", lend$Purpose_alt)
lend$Purpose_alt <- ifelse(lend$purpose == "medical", "personal",lend$Purpose_alt)
lend$Purpose_alt <- ifelse(lend$purpose =="wedding", "personal", lend$Purpose_alt)
lend$Purpose_alt <- ifelse(lend$purpose =="vacation", "personal", lend$Purpose_alt)
lend$Purpose_alt <- ifelse(lend$purpose=="renewable_energy", "other", lend$Purpose_alt)
lend$Purpose_alt <- ifelse(lend$purpose=="moving", "personal", lend$Purpose_alt)

margin.table(prop.table(table(lend$Purpose_alt)),1)
CrossTable(lend$loan_status, lend$Purpose_alt, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

## Change dates
library(stringr)

lend$earliest_dat<-sub("(\\w+)(\\W+)(\\w+)", "\\3\\2\\1-01", lend$earliest_cr_line)
lend$earliest_Date<-as.Date(lend$earliest_dat, "%Y-%b-%d")
lend$issue_dat<-sub("(\\w+)(\\W+)(\\w+)", "\\3\\2\\1-01", lend$issue_d)
lend$issue_Date<-as.Date(lend$issue_dat, "%Y-%b-%d")
lend$issue_d<-NULL
lend$earliest_cr_line<-NULL
lend$earliest_dat<-NULL
lend$issue_dat<-NULL
lend$purpose<-NULL

## Create lend history variable
end_date<-as.Date("2022-01-01")
library(lubridate)
lend$amt_lend_hist <- round(time_length(difftime(end_date, lend$earliest_Date), "years"))  
lend$loan_age<-round(time_length(difftime(end_date, lend$issue_Date), "years"))

## Create data frame with dummy variables
lend.num<-data.frame(lend)
View(lend.num)

#### dummy variables
# term
lend.num$term_36<-ifelse(lend.num$term == "36 months",1,0)
margin.table(prop.table(table(lend.num$term_36)),1)
lend.num$term<-NULL

## home_ownership
lend.num$home_ownership_own<-ifelse(lend.num$home_ownership=="OWN", 1,0)
lend.num$home_ownership_rent<-ifelse(lend.num$home_ownership=="RENT",1,0)
lend.num$home_ownership<-NULL

## Verification Status
lend.num$verification_status_not<-ifelse(lend.num$verification_status=="Not Verified",1,0)
lend.num$verification_status_ver<-ifelse(lend.num$verification_status=="Verified",1,0)
lend.num$verification_status<-NULL

## ordinal variables
lend.num$earliest_Date<-NULL
lend.num$issue_Date<-NULL

lend.num$grade_num<-as.factor(lend.num$grade)
levels(lend.num$grade_num)
levels(lend.num$grade_num)<-c(1,2,3,4,5,6,7)
lend.num$grade_num<-as.numeric(lend.num$grade_num)
class(lend.num$grade_num)
lend.num$grade<-NULL

lend.num$loan_status<-ifelse(lend.num$loan_status=="Fully Paid",1,0)
lend.num$loan_status<-as.factor(lend.num$loan_status)
summary(lend.num$loan_status)
lend.num$emp_length<-NULL
lend.num$Purpose_alt<-NULL

lend$earliest_Date<-NULL
lend$issue_Date<-NULL
summary(lend)
lend$term<-as.factor(lend$term)
lend$grade<-as.factor(lend$grade)
lend$emp_length<-as.factor(lend$emp_length)
lend$home_ownership<-as.factor(lend$home_ownership)
lend$verification_status<-as.factor(lend$verification_status)
lend$loan_status<-as.factor(lend$loan_status)
lend$Purpose_alt<-as.factor(lend$Purpose_alt)

###########################
### Logistic Regression ###
###########################
## separate test and training data
set.seed(208)
train<-sample(1:nrow(lend.num), 4999)
lend.num.train<-lend.num[train,]
lend.num.test<-lend.num[-train,]
lend.test<-lend[-train,]
lend.train<-lend[train,]

glm.fit<-glm(loan_status~.,data=lend.num.train, family=binomial)
summary(glm.fit)

glm.prob<-predict(glm.fit, newdata = lend.num.test)
glm.pred<-rep("Charged Off",4999)
glm.pred[glm.prob>.5]<-"Fully Paid"
table(glm.pred, lend.test$loan_status)
mean(glm.pred==lend.test$loan_status)

## best subset selection
library(leaps)
reg.fit.full<-regsubsets(loan_status~.,lend.num, nvmax=15)
reg.summary<-summary(reg.fit.full)
plot(reg.summary$rsq, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted Rsq", type="l")
which.max(reg.summary$adjr2)
points(15,reg.summary$adjr2[15], col="red", cex=2, pch=20)

plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(reg.summary$cp)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min(reg.summary$bic)
points(6,reg.summary$bic[6], col="red", cex=2, pch=20)

coef(reg.fit.full,6)

## Logistic regression with reduced dimension
glm.fit<-glm(loan_status~dti+total_acc+term_36+home_ownership_rent+
               verification_status_not+grade_num,data=lend.num.train, family=binomial)
summary(glm.fit)

glm.prob<-predict(glm.fit, newdata = lend.num.test)
glm.pred<-rep("Charged Off",4999)
glm.pred[glm.prob>.5]<-"Fully Paid"
table(glm.pred, lend.test$loan_status)
mean(glm.pred==lend.test$loan_status)

############
###  LDA ###
############
library(MASS)
lda.fit<-lda(loan_status~., data=lend.num.train)
lda.fit

plot(lda.fit)

lda.pred<-predict(lda.fit,lend.num.test)
table(lda.pred$class, lend.num.test$loan_status)
mean(lda.pred$class==lend.num.test$loan_status)

library(ROCR)
CrossTable(lend.num.test$loan_status, lda.pred$class, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
pred<-prediction(as.numeric(lda.pred$class), as.numeric(lend.num.test$loan_status))
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="blue", main="ROC Curve")
abline(0,1, col="red", lty="dashed")

#############
###  QDA  ###
#############
qda.fit<-qda(loan_status~., lend.num.train)
qda.pred<-predict(qda.fit, lend.num.test)
table(qda.pred$class, lend.num.test$loan_status)
mean(qda.pred$class == lend.num.test$loan_status)


########################
## tree-based methods ##
########################
###decision tree
library(tree)
tree.loan<-tree(loan_status~., data=lend.train)
summary(tree.loan)
pred.tree<-predict(tree.loan, lend.test, type="class")
table(pred.tree, lend.test$loan_status)
mean(pred.tree==lend.test$loan_status)

### Random Forest
library(randomForest)
set.seed(2)
rf.loan<-randomForest(loan_status~., data=lend.train, mtry = 11, importance=TRUE)
pred.rf<-predict(rf.loan,newdata=lend.test)
table(pred.rf, lend.test$loan_status)
mean(pred.rf==lend.test$loan_status)
importance(rf.loan)

set.seed(2)
rf.loan<-randomForest(loan_status~., data=lend.num.train, mtry = 11, importance=TRUE)
summary(rf.loan)
pred.rf<-predict(rf.loan,newdata=lend.num.test)
table(pred.rf, lend.num.test$loan_status)
mean(pred.rf==lend.num.test$loan_status)
varImpPlot(rf.loan,  main="", cex=0.8)
plot(rf.loan)

CrossTable(lend.num.test$loan_status, pred.rf, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
pred<-prediction(as.numeric(pred.rf), as.numeric(lend.num.test$loan_status))
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="blue", main="ROC Curve")
abline(0,1, col="red", lty="dashed")
###########
## scale ##
###########
lend.scale<-lend.num
lend.scale$dti<-lend.scale$dti+0.0001
lend.scale$revol_bal<-lend.scale$revol_bal+0.0001
lend.scale$revol_util<-lend.scale$revol_util+0.0001
lend.scale[,c(1,2,3,4,6,7,9,10,11,14)]<-log(lend.scale[,c(1,2,3,4,6,7,9,10,11,14)],10)
lend.scale.train<-lend.scale[train,]
lend.scale.test<-lend.scale[-train,]

## lda
lda.fit2<-lda(loan_status~., data=lend.scale.train)
lda.fit2

plot(lda.fit2)

lda.pred2<-predict(lda.fit2,lend.scale.test)
table(lda.pred2$class, lend.scale.test$loan_status)
mean(lda.pred2$class==lend.scale.test$loan_status)

## logistic regression
glm.fit2<-glm(loan_status~.,data=lend.scale.train, family=binomial)
summary(glm.fit2)

glm.prob2<-predict(glm.fit2, newdata = lend.scale.test)
glm.pred2<-rep("Charged Off",4999)
glm.pred2[glm.prob2>.5]<-"Fully Paid"
table(glm.pred2, lend.test$loan_status)
mean(glm.pred2==lend.test$loan_status)

### k-nearest neighbor
library(class)
train.x<-matrix(lend.scale[train,-5])
test.x<-matrix(lend.scale[-train,-5])
train.status<-lend.scale[train,5]

set.seed(1)
knn.pred<-knn(lend.scale.train[,-5], lend.scale.test[,-5], lend.scale.train$loan_status, k=15)
table(knn.pred, lend.scale.test$loan_status)
mean(knn.pred==lend.scale.test$loan_status)


CrossTable(lend.scale.test$loan_status, knn.pred, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
pred<-prediction(as.numeric(knn.pred), as.numeric(lend.scale.test$loan_status))
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="blue", main="ROC Curve")
abline(0,1, col="red", lty="dashed")

## Logistic regression with select variables and scaling
glm.fit2<-glm(loan_status~dti+total_acc+term_36+home_ownership_rent+
               verification_status_not+grade_num,data=lend.scale.train, family=binomial)
summary(glm.fit2)

glm.prob2<-predict(glm.fit2, newdata = lend.scale.test)
glm.pred2<-rep("Charged Off",4999)
glm.pred2[glm.prob2>.5]<-"Fully Paid"
table(glm.pred2, lend.test$loan_status)
mean(glm.pred2==lend.test$loan_status)

CrossTable(lend.scale.test$loan_status, glm.pred2, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
glm.pred3<-rep("0",4999)
glm.pred3[glm.prob2>.5]<-"1"
pred<-prediction(as.numeric(glm.pred3), as.numeric(lend.scale.test$loan_status))
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="blue", main="ROC Curve")
abline(0,1, col="red", lty="dashed")
# Setting the working directory
setwd("C:\Users\Administrator\Documents\R studio\R")
# Importing the data
aids <- read.csv("Aust Aids Data.csv")
head(aids, n=10)
# Rename the column
colnames(aids)<- c("age", "los", "status", "state", "sex")
head(aids)
# Data Preprocessing and obtaining summary Statistics and plot of the Initial Data
meanx = apply(aids[,1:2], 2, mean)
sdx = apply(aids[,1:2], 2, sd)
table(aids$status)
statex<-table(aids$state)
sexx<-table(aids$sex)

aids1<-subset(aids, status==1)
aids0<-subset(aids, status==0)

meanx1 = apply(aids1[,1:2], 2, mean)
sdx1 = apply(aids1[,1:2], 2, sd)
table(aids1$status)
statex1<-table(aids1$state)
sexx1<-table(aids1$sex)


meanx0 = apply(aids0[,1:2], 2, mean)
sdx0 = apply(aids0[,1:2], 2, sd)
table(aids0$status)
statex0<-table(aids0$state)
sexx0<-table(aids0$sex)


stats_overall<-list(overallmean=meanx,overallsd=sdx,overallcount1=statex,overallcount2=sexx,table(aids$status))
stats_0<-list(mean=meanx0,sd=sdx0,state0=statex0,sex0=sexx0,table(aids0$status))
stats_1<-list(mean=meanx1,sd=sdx1,state0=statex1,sex0=sexx1,table(aids1$status))

boxplot(aids$age~aids$status, col=c("red","green"), ylab = "Age in Years",xlab = "Status")
boxplot(aids$los~aids$status, col=c("red","green"), ylab = "Lenght of Stay",xlab = "Status")

table1<-table(aids$state,aids$status)
table2<-table(aids$sex,aids$status)
barplot(table1, beside = T, col = c("red","green"), legend.text = T, xlab="Status",ylab="Location")
barplot(table2, beside = T, col = c("red","green"), legend.text = T, ylab = "Gender")

# outliers was detected in the data, we adjust for the outliers by removing the outliers
# remove outliers
out1<-boxplot(aids$age~aids$status, plot=FALSE)$out
aids_out1<- aids[-which(aids$age %in% out1),]
boxplot(aids_out1$age~aids_out1$status)
boxplot(aids_out1$los~aids_out1$status)
dim(aids_out1)
out2<-boxplot(aids_out1$los~aids_out1$status, plot=FALSE)$out
aids_out2<- aids_out1[-which(aids_out1$los %in% out2),]
boxplot(aids_out2$age~aids_out2$status)
boxplot(aids_out2$los~aids_out2$status)

# After Removal: Computing of the Summary statistics and Plot 
meanx_out2 = apply(aids_out2[,1:2], 2, mean)
sdx_out2 = apply(aids_out2[,1:2], 2, sd)
table(aids_out2$status)
statex_out2<-table(aids_out2$state)
sexx_out2<-table(aids_out2$sex)

aids1_out2<-subset(aids_out2, status==1)
aids0_out2<-subset(aids_out2, status==0)

meanx1_out2 = apply(aids1_out2[,1:2], 2, mean)
sdx1_out2 = apply(aids1_out2[,1:2], 2, sd)
table(aids1_out2$status)
statex1_out2<-table(aids1_out2$state)
sexx1_out2<-table(aids1_out2$sex)


meanx0_out2 = apply(aids0_out2[,1:2], 2, mean)
sdx0_out2 = apply(aids0_out2[,1:2], 2, sd)
table(aids0_out2$status)
statex0_out2<-table(aids0_out2$state)
sexx0_out2<-table(aids0_out2$sex)


stats_overall_out2<-list(overallmean=meanx_out2,overallsd=sdx_out2,overallcount1=statex_out2,overallcount2=sexx_out2,table(aids_out2$status))
stats_0_out2<-list(mean=meanx0_out2,sd=sdx0_out2,state0=statex0_out2,sex0=sexx0_out2,table(aids0_out2$status))
stats_1_out2<-list(mean=meanx1_out2,sd=sdx1_out2,state0=statex1_out2,sex0=sexx1_out2,table(aids1_out2$status))

boxplot(aids_out2$age~aids_out2$status, col=c("red","green"), ylab = "Age in Years",xlab = "Status")
boxplot(aids_out2$los~aids_out2$status, col=c("red","green"), ylab = "Lenght of Stay",xlab = "Status")

table1_out2<-table(aids_out2$state,aids_out2$status)
table2_out2<-table(aids_out2$sex,aids_out2$status)
barplot(table1_out2, beside = T, col = c("red","green"), legend.text = T, xlab="Status",ylab="Location")
barplot(table2_out2, beside = T, col = c("red","green"), legend.text = T, ylab = "Gender")

# Preliminary Analysis to uncover salient information
# Chi-Squared Test
table1_out2
chisq.test(table1_out2)$expected
chisq.test(table1_out2)
table2_out2
chisq.test(table2_out2)$expected
chisq.test(table2_out2)
# T-Test
t.test(aids_out2$age~aids_out2$status)
t.test(aids_out2$los~aids_out2$status)

# Fitting the Logistic Model
model <- glm(status ~.,family=binomial(link='logit'), data=aids_out2)
summary(model)
confint(model)
exp(cbind(OR = coef(model), confint(model)))
anova(model, test="Chisq")
library(pscl)
pR2(model)
# Dispersion Test
deviance(model)/df.residual(model)
model.qb <- glm(status ~.,family=quasibinomial(link='logit'), data=aids_out2)
pchisq(summary(model.qb)$dispersion * model$df.residual,model$df.residual, lower = F)
# Model Performance
library(ROCR)
# Accuracy
fitted.results <- predict(model,newdata=aids_out2,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != aids_out2$status)
print(paste('Accuracy',1-misClasificError))
# AUC ROC
p <- predict(model, newdata=aids_out2, type="response")
pr <- prediction(p, aids_out2$status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
plot(prf)
plot(prf, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)


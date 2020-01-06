rm(list=ls())
library('ggplot2')
library('leaps')
library('vminfulence')
library('visreg')
library('car')
library('dplyr')

plot(Train) #talk about relationships between brozek and covariates

scatterplotMatrix(Train[,-1]) #talk about skewness, include mean>median

#Boxplots: talk about skew more in depth plus potential outliers:
boxplot(Train$neck, Train$knee, Train$ankle, Train$biceps, Train$forearm, Train$wrist,
        main="Boxplot for each bodypart", 
        ylab="Centimeters",
        names=c("Neck", "Knee", "Ankle", "Biceps", "Forearm", "Wrist"))

boxplot(Train$thigh, Train$chest, Train$abdom, Train$hip,
        main="Boxplot for each bodypart", 
        ylab="Centimeters",
        names=c("Thigh", "Chest", "Abdomen", "Hip"))

# talker about dependance - high dependance can = potentialy remove one of the variables:
#chest v abdom,  hip vs thigh:
plot(Train)
qplot(Train$brozek, Train$neck)+ggtitle("Circumference of Neck")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$chest)+ggtitle("Circumference of Chest")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$abdom)+ggtitle("Circumference of Adomen")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$hip)+ggtitle("Circumference of hip")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$thigh)+ggtitle("Circumference of thigh")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$knee)+ggtitle("Circumference of knee")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$ankle)+ggtitle("Circumference of ankle")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$biceps)+ggtitle("Circumference of biceps")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$forearm)+ggtitle("Circumference of Forearm")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")
qplot(Train$brozek, Train$wrist)+ggtitle("Circumference of Wrist")+labs(x="Circumference of neck (cm)", y="Body Fat")+stat_smooth(colour="red", method = "loess")


ggplot(data=Train, aes(sample=neck))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Neck")
ggplot(data=Train, aes(sample=chest))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Chest")
ggplot(data=Train, aes(sample=abdom))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Abdomen")
ggplot(data=Train, aes(sample=hip))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Hip")
ggplot(data=Train, aes(sample=tigh))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Thigh")
ggplot(data=Train, aes(sample=knee))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Knee")
ggplot(data=Train, aes(sample=ankle))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Ankle")
ggplot(data=Train, aes(sample=biceps))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Biceps")
ggplot(data=Train, aes(sample=forearm))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Forearm")
ggplot(data=Train, aes(sample=wrist))+stat_qq(color="black")+stat_qq_line(color="red")+ggtitle("QQ-Plot for Wrist")


# Model Selection
fit1 <- lm(brozek~., data=Train)
fit0N <- lm(brozek~1, data=Train)
coefficients(fit1)  # Brokzek = coef*x_i

#First test
BSR <- regsubsets(brozek~., data=Train, nbest=1, nvmax=10)
summary.out <- summary(BSR)
summary.out
summary.out$cp #parameters, choose 6.15 since > 3.32.  include covs wirh star

plot(BSR, scale='')

plot(BSR, scale='Cp')
plot(BSR, scale='bic')
plot(BSR, scale='aic')
fit2 <- lm(brozek~neck+abdom+hip+wrist, data=Train)

#second test:
step(fit1) #takes out cov with smallest AIC

fit3 <- lm(brozek ~ neck+abdom+hip+forearm+wrist, data = Train) #use for model valuation

#third test:
step(fit0N, scope = brozek~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist)
#model mathces fit3

# F-test comapring the two models:
anova(fit2,fit3)
#evidence suggesting that fit3 is better model

#HLP:
influenceIndexPlot(fit3)
influencePlot(fit3)
# HLP if studentRes>2,<-2  or Hat,CookD.0.5
# there are 3 HLP


# Model Selection 2 - removing 1 HLP
fit4 <- update(fit3,
               subset = rownames(Train) != "33")
compareCoefs(fit3,fit4)

Train1 <- Train[-69,]

fit5 <- lm(brozek~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, data=Train1)
fit4N <- lm(brozek~1, data=Train1)
coefficients(fit4)  # Brokzek = coef*x_i

#First test
BSR <- regsubsets(brozek~., data=Train1, nbest=1, nvmax=10)
summary.out <- summary(BSR)
summary.out
summary.out$cp #parameters.  # Choose row 3

fit6 <- lm(brozek~neck+abdom+hip+wrist, data=Train1)

#second test:
step(fit5) #takes out cov with smallest AIC

fit7 <- lm(brozek ~ neck+abdom+hip+wrist+forearm, data = Train1) #use for model valuation

#third test:$
step(fit4N, scope = brozek~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist)
#model mathces fit7

anova(fit6,fit7)
#Select fit7

#compare deviances:
deviance(fit3)
deviance(fit7)
#Result of taking out row 33 means the deviance has decreased significantly from model 3 to 7, suggestng fit7 is better

# Model Selection 3 - take out 3 HLP
Train2 <- Train[-c(33,69,72),]

fit9 <- lm(brozek~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, data=Train2)
fit8N <- lm(brozek~1, data=Train2)
coefficients(fit9)

#First test
BSR <- regsubsets(brozek~., data=Train2, nbest=1, nvmax=10)
summary.out <- summary(BSR)
summary.out
summary.out$cp

fit10 <- lm(brozek~neck+abdom+hip+ankle+wrist, data=Train2)

#second test:
step(fit9) #takes out cov with smallest AIC

fit11 <- lm(brozek~ neck+abdom+hip+ankle+forearm+wrist, data = Train2)

#third test:
step(fit8N, scope = brozek~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist)
#model mathces fit11

anova(fit10,fit11)
#Choose fit11

deviance(fit11)
deviance(fit7)
#Deviance smaller again so choose fit11

#HLP:
influenceIndexPlot(fit11)
influencePlot(fit11)
# HLP if studentRes>2,<-2  or Hat,CookD.0.5
# there are 2 HLP: 68,177


# Model Selection 4 - take out 2 HLP
Train3 <- Train1[-c(68,177),]

fit13 <- lm(brozek~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, data=Train3)
fit12N <- lm(brozek~1, data=Train3)
coefficients(fit13)

#First test
BSR <- regsubsets(brozek~., data=Train3, nbest=1, nvmax=10)
summary.out <- summary(BSR)
summary.out
summary.out$cp

fit14 <- lm(brozek~neck+abdom+hip+ankle+wrist, data=Train3)

#second test:
step(fit13) #takes out cov with smallest AIC

fit15 <- lm(brozek~ neck+abdom+hip+ankle+forearm+wrist, data = Train3)

#third test:
step(fit12N, scope = brozek~neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist)
#agrees with fit15

anova(fit14,fit15)
#Choose 15
deviance(fit3)
deviance(fit7)
deviance(fit11)
deviance(fit15)

# Model validation
qqPlot(fit3) # Follows nomral distrn w 2 outliers

hist(rstudent(fit3)) #slight positive skew

crPlots(fit3)
residualPlots(fit3, tests=FALSE)
#solid line plots trend
#dashed line is predicted, fairly consistent

fit20 <- lm(sqrt(brozek) ~ neck+abdom+hip+forearm+wrist, data = Train)
qqPlot(fit20)
hist(rstudent(fit20)) 




fit2 <- lm(brozek ~ I(neck^3)+abdom+I(hip^3)+forearm+wrist, data = Train)
residualPlots(fit21, tests=FALSE)
crPlots(fit21)

plot(fit21, which = 2) # residuals bounce randomly around 0 line => linear model is good
crPlots(fit3)

#b)
fit1b <- lm(brozek~., data = Train)
fit2b <- lm(brozek ~ I(neck^3)+abdom+I(hip^3)+forearm+wrist, data = Train)

TestResponses=select(Test, brozek)$brozek

predictions1 <- predict(fit1, newdata=select(Test, -brozek))
predictions2 <- predict(fit2, newdata=select(Test, -brozek))

MSE1 <- mean((predictions1 - TestResponses)^2)
MSE2 <- mean((predictions2 - TestResponses)^2)


MSE1
MSE2
MSE3

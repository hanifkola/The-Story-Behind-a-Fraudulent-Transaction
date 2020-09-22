---
title: "final project"
author: "Sherveer Dhillon - Hanif Kolahdoozan "
date: "March 27, 2020"
output:
  word_document: default
  
---
```{r, echo=FALSE}
library(tidyverse)
library(knitr)
library(ggplot2)
library(ggpubr)
library(car)
library(broom)
library(ROSE)
library(caret)
library(ROCR)
library(regclass)
```

## Data Description


##Preperaing Data


```{r, echo = FALSE}

creditcardmain <- read.csv("creditcardcsvpresent.csv")
#creditcarddata$isFradulent <- as.factor(creditcarddata$isFradulent)
#creditcard <- creditcard[c(1,3:12)]
creditcard <- select(creditcardmain,-Transaction.date )
#creditcard <- ROSE(isFradulent ~ ., data =creditcarddata)$data
#str(creditcard)
#creditcarddata <- ovun.sample(isFradulent~. , data = creditcard, method = "under" )$data
creditcarddata <- ROSE(isFradulent~. , data = creditcard)$data
#bankdata$response <- as.factor(bankdata$response) 
#bal_BD <- ROSE(response~., data = bankdata)$data

```
 


##planning

looking at data to find which attributes helps to make precise classification
factor ones

```{r}
A <- ggplot(creditcard, aes( y = isFradulent, x =  X6.month_chbk_freq, color = isFradulent,alpha = 0.5)) + geom_point(shape = 1) +geom_jitter(width=0.2, height=0.2)
B <- ggplot(creditcard, aes( y = isFradulent, x =  Is.declined, color = isFradulent,alpha = 0.5)) + geom_point(shape = 1) +geom_jitter(width=0.2, height=0.2)
C <- ggplot(creditcard, aes( y = isFradulent, x =  isHighRiskCountry, color = isFradulent,alpha = 0.5)) + geom_point(shape = 1) +geom_jitter(width=0.2, height=0.2)
D<- ggplot(creditcard, aes( y = isFradulent, x =  isForeignTransaction, color = isFradulent,alpha = 0.5)) + geom_point(shape = 1) +geom_jitter(width=0.2, height=0.2)
ggarrange(A,B,C,D)
```

and for numerical variables
```{r, echo = FALSE}
E <- ggplot(creditcard, aes( y = isFradulent, x =  Average.Amount.transaction.day, color = isFradulent )) + geom_point(alpha = .01) + geom_jitter(width = 0, height = .1, alpha = .3)   
F <- ggplot(creditcard, aes( y = isFradulent, x =  Transaction_amount, color = isFradulent,alpha = 0.5)) + geom_point(alpha = .01) + geom_jitter(width = 0, height = .1, alpha = .3) 
G <- ggplot(creditcard, aes( y = isFradulent, x =  Total.Number.of.declines.day, color = isFradulent,alpha = 0.5)) +  geom_point(alpha = .01) + geom_jitter(width = 0, height = .1, alpha = .3) 
H <- ggplot(creditcard, aes( y = isFradulent, x =  Daily_chargeback_avg_amt, color = isFradulent,alpha = 0.5)) +  geom_point(alpha = .01) + geom_jitter(width = 0, height = .1, alpha = .3) 
I <- ggplot(creditcard, aes( y = isFradulent, x =  X6_month_avg_chbk_amt, color = isFradulent,alpha = 0.5)) +  geom_point(alpha = .01) + geom_jitter(width = 0, height = .1, alpha = .3)
ggarrange(E,F,G,H,I, ncol = 2, nrow= 3)

```

```{r, echo = FALSE}
#list <- colnames(creditcard)
#listreg <- list[c(4:6,8:9,11)]
data(creditcarddata)

Train <- createDataPartition(creditcarddata$isFradulent, p = 0.75 , list = FALSE)
training <- creditcarddata[ Train, ]
testing <- creditcarddata[ -Train, ]
#contrasts(training$isHighRiskCountry) = contr.treatment(2)
creditcardmodel <- glm (formula =isFradulent ~ Transaction_amount+ Total.Number.of.declines.day + isHighRiskCountry +  X6.month_chbk_freq + isForeignTransaction + X6_month_avg_chbk_amt  +Average.Amount.transaction.day, data = training, family = binomial() )
summary(creditcardmodel) 
```



```{r}
exp(confint(creditcardmodel))

```


```{r}


```


##Analysis

testing the accuracy of the model
```{r}
pred = predict(creditcardmodel,type = "response", newdata = testing)
accuracy <- table (testing$isFradulent, pred >= 0.5)
#(1049+1108) / (1049+1108+48+102)
sum(diag(accuracy))/sum(accuracy)

```

Drawing ROCR plot
A Reciever Operator Characteristic curve illuatrate diagonistic ability of our model
```{r}
pred = predict(creditcardmodel,type = "response", newdata = testing)
ROCRpred = prediction(pred,testing$isFradulent)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE)

```
#AUC
AUc is simply area under the curve, and is strong measurment to analyse the model accuracy. 
```{r}
perf <- performance(ROCRpred, measure = "auc")
#perf
print(  perf@y.values)
```

The plot above illustrate performance of the model in classifiying test data. according to this plot, True positive rates are significantly high which shows power of the model. 



Test of multicollinearity
```{r}

car::vif(creditcardmodel)
```
```{r}
training$cooks <- cooks.distance(creditcardmodel)
plot(sort(training$cooks, decreasing=TRUE))
```

```{r}
durbinWatsonTest(creditcardmodel)

```
```{r}
exp(creditcardmodel$coefficients)

```

```{r}
modelresiduals <- creditcardmodel$residuals
modelresiduals2 <- subset(modelresiduals,modelresiduals < 1.05 & modelresiduals > -1.05 )
plot(abs(modelresiduals2))
#plot(exp(wine.color.model$residuals))
```
We can check performance of our classification model with confusion Matrix below which shows acceptable level of accuracy.  
```{r}

confusion_matrix(creditcardmodel)
```


```{r}
probabilities <- predict(creditcardmodel,type = "response") 
predictors <- names(creditcardmodel$coefficients)
predictors <- predictors[2:length(predictors)]
#predictors[3] <- "isHighRiskCountry"
#predictors[5] <- "isForeignTransaction"
 predictors <- predictors[c(1,2,4,6,7)]
mydata <- subset (training,select=predictors)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam") + 
  theme( axis.text.y=element_blank()) + 
  facet_wrap(~predictors,scales = "free_y") 
```

incomplete information
In this data set as mentioned in previous section, there is no lost data,so this assumption is not violated. 
Testing complete separation
The logistic model fails if there is complete overlap. Since there is no certain overlap in data this assumption is not violated


##conlcusion



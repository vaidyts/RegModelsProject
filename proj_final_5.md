---
title: "Mileage of automatic vs manual transmission cars"
output: pdf_document
---

### Executive Summary

As part of the course project for Regression Models course, we are asked, based on the mtcars dataset, to answer whether automatic or manual transmission is better for miles per gallon (mpg), and to substantiate the answer using statistical tools and methods. We go about by looking at the data, testing for normality, plotting exploratory graphs, and applying inferential and regression techniques. It is seen that there is indeed a statistically significant difference in mpg between automatic and manual cars. We explore further by developing 3 different regression models based on different assumptins and comparing them for predictor power, residuals, etc. It is seen that Model0 is very simplistic and has the least predictive power, while Models 1 and 2 are more robust and are better and comparable in term of predictive power and residuals.

### Solution

### Exploratory analysis, inference, and Regression Model #0

First we load the data and create two dataframes for auto and manual cars. We do some exploratory plots and then we check for normality of the data using Shapiro Wilk test. The resulting p-values are high, so we can say that the data are normally distributed. It is seen that the means are different, but are they statistically different? For this, a two-sample t-test is done. The test returns a p value of 0.001374, which implies that the results are statistically significant. Hence it can be said with atleast 95% confidence that the manual transmission cars give 7.245 miles per gallon higher than automatic transmission cars. The same result can be obtained by building a regression model ```model0``` between ```mpg``` and ```am```. The slope coefficient of this is found to be 7.245. The slope coefficient is defined as the change in the outcome for a unit change in the predictor. In this case, a unit change in the predictor corresponds to a change from automatic to manual,hence the regression coefficient corresponds to the mpg difference between automatic and manual transmission cars.

### Development of Regression Model #1

However, when we do a summary of ```model0``` and look at the R-squared, we seen that the variable ```am``` is able to explain only a third of the residual variation, hence something is clearly missing, and we need to include other variables in the model. We use our intuition and general knowledge. Intuition says that weight and horse power(```wt```, ```hp```) should strongly correlate with ```mpg```. So let us add ```wt``` and ```hp``` and try again and call this model ```try1```. We see that in ```try1```, the R squared has gone up to 83% just with the inclusion of these two variables, which is pretty good. This model is simple, intuitive and has good predictive power. However we see that ```am``` is not a very strong predictor of ```mpg```, which is sort of counter intuitive. This makes us wonder what might be the strongest predictors of ```mpg``` from a mathematical viewpoint. Which brings us to make another attempt.

### Development of Regression Model #2

For this, we start a model with all variables and look at the summary. This model is named ```try2```. We construct leverage Plots to find out the most important variables. We find that ```am```, ```wt```, ```hp```, ```qsec``` and ```disp``` are the most important ones. So we select these variables and fit another model named ```model3```. To detect any evidence of multi-collinearity of variables, we estimate the variation inflation factor. All the values are less than 10, which tells that there are no strong co-dependent variables, hence we proceed further. We refine ```try3``` further by removing ```disp``` based on high value of Pr(>|t|) and call this ```try4```. We further refine ```try4``` by removing ```hp``` based on high value of Pr(>|t|) and refine further. This is ```try5```. A look at ```try5``` tells us that all variables have low p-values, and a R-squared of 85%, so we can stop here. This is the final model 2. Finally, we look at the fit quality and residuals of the models individually as well as in comparison with each other. Both models are very similar in predictie power with no systematic variation (heteroskedasticity). 

### Code - selected portions only

```{r eval=FALSE}
#Some exploratory plots
plot(mtcars$mpg,col=as.factor(mtcars$am),pch=19,main="Exploratory plot")
abline(h=mean(automatic$mpg),col="black"); abline(h=mean(manual$mpg),col="red")
plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Exploratory plot")
abline(h=mean(automatic$mpg),col="black"); abline(h=mean(manual$mpg),col="red")
boxplot(mtcars$mpg~mtcars$am,main="BOX plot comparison")
# Test for normality using Shapiro-Wilk test
shapiro.test(automatic$mpg); shapiro.test(manual$mpg) 
# Perform t-test to compare means statistically. 
t.test(automatic$mpg,manual$mpg) 
#Model0 and Plots 
model0 <- lm(mpg~am,data=mtcars); summary(try0) 
plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Model0")
p0 <- predict(lm(mtcars$mpg~mtcars$am),data.frame(x=mtcars$am),interval="confidence")
lines(sort(p0[,1]),col="blue",lwd=2);lines(sort(p0[,2]),col="blue");lines(sort(p0[,3]),col="blue")
plot(predict(model0),resid(model0),col=as.factor(mtcars$am),pch=19,main="Model0 Residuals")
#Model1 and Plots
try1 <- lm(mpg ~ am + wt + hp, data=mtcars); summary(try1)
model1 <- try1 #This is our final Model#1
plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Model1")
p1 <- predict(lm(mtcars$mpg~mtcars$am+mtcars$wt+mtcars$hp),data.frame(x=mtcars$am),interval="confidence")
lines(sort(p1[,1]),col="blue",lwd=2);lines(sort(p1[,2]),col="blue");lines(sort(p1[,3]),col="blue")
plot(predict(model1),resid(model1),col=as.factor(mtcars$am),pch=19,main="Model1 Residuals")
# Model2 and Plots
try2 <- lm(mpg ~ ., data=mtcars); summary(try2); leveragePlots(try2); 
try3 <- lm(mpg ~ am + wt + hp + qsec + disp, data=mtcars); summary(try3); vif(try3) 
try4 <- lm(mpg ~ am + wt + hp + qsec, data=mtcars); summary(try4) 
try5 <- lm(mpg ~ am + wt + qsec, data=mtcars); summary(try5)
model2 <- try5 This is our final Model#2
plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Model2")
legend("topleft", pch = 19, col = c("black", "red","blue"), legend = c("automatic", "manual","Model2"))
p2 <- predict(lm(mtcars$mpg~mtcars$am+mtcars$wt+mtcars$qsec),data.frame(x=mtcars$am),interval="confidence")
lines(sort(p2[,1]),col="blue",lwd=2);lines(sort(p2[,2]),col="blue");lines(sort(p2[,3]),col="blue")
plot(predict(model2),resid(model2),col=as.factor(mtcars$am),pch=19,main="Model2 Residuals")
# Overlay the Models 0, 1 and 2, and residuals
plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Data, Model0, 1, 2")
lines(sort(predict(model0)),col="pink",lwd=2)
lines(sort(predict(model1)),col="blue",lwd=2)
lines(sort(predict(model2)),col="green",lwd=2)
plot(sort(resid(model0)),col="pink",main="Residuals: Model0, 1, 2")
lines(sort(resid(model1)),col="blue",lwd=2)
lines(sort(resid(model2)),col="green",lwd=2)
# Generate standard residual plots
plot(model1,main="Model1")
plot(model2,main="Model2")
```

```{r,include=FALSE}
library(car)
data(mtcars)
automatic <- mtcars[mtcars$am==0,]
manual <- mtcars[mtcars$am==1,]
model0 <- lm(mpg ~ am, data=mtcars)
model1 <- lm(mpg ~ am + wt + hp, data=mtcars)
model2 <- lm(mpg ~ am + wt + qsec, data=mtcars)
```

```{r,echo=FALSE,fig.height=10}
par(mfrow=c(2,2)); 
plot(mtcars$mpg,col=as.factor(mtcars$am),pch=19,main="Exploratory plot")
abline(h=mean(automatic$mpg),col="black"); abline(h=mean(manual$mpg),col="red")
legend("topleft", pch = 19, col = c("black", "red"), legend = c("automatic", "manual"))
plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Exploratory plot")
abline(h=mean(automatic$mpg),col="black"); abline(h=mean(manual$mpg),col="red")
legend("topleft", pch = 19, col = c("black", "red"), legend = c("automatic", "manual"))
boxplot(mtcars$mpg~mtcars$am,main="BOX plot comparison")
```

```{r,echo=FALSE,fig.height=10}
par(mfrow=c(4,2)); 

plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Model0:mpg~am")
legend("topleft", pch = 19, col = c("black", "red","blue"), legend = c("automatic", "manual","Model0"))
p0 <- predict(lm(mtcars$mpg~mtcars$am),data.frame(x=mtcars),interval="confidence")
lines(sort(p0[,1]),col="blue",lwd=2);lines(sort(p0[,2]),col="blue");lines(sort(p0[,3]),col="blue")

plot(predict(model0),resid(model0),col=as.factor(mtcars$am),pch=19,main="Model0 Residuals")

plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Model1:mpg~am+wt+hp")
legend("topleft", pch = 19, col = c("black", "red","blue"), legend = c("automatic", "manual","Model1"))
p1 <- predict(lm(mtcars$mpg~mtcars$am+mtcars$wt+mtcars$hp),data.frame(x=mtcars),interval="confidence")
lines(sort(p1[,1]),col="blue",lwd=2);lines(sort(p1[,2]),col="blue");lines(sort(p1[,3]),col="blue")

plot(predict(model1),resid(model1),col=as.factor(mtcars$am),pch=19,main="Model1 Residuals")

plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Model2:mpg~am+wt+qsec")
legend("topleft", pch = 19, col = c("black", "red","blue"), legend = c("automatic", "manual","Model2"))
p2 <- predict(lm(mtcars$mpg~mtcars$am+mtcars$wt+mtcars$qsec),data.frame(x=mtcars),interval="confidence")
lines(sort(p2[,1]),col="blue",lwd=2);lines(sort(p2[,2]),col="blue");lines(sort(p2[,3]),col="blue")

plot(predict(model2),resid(model2),col=as.factor(mtcars$am),pch=19,main="Model2 Residuals")

plot(sort(mtcars$mpg),col=as.factor(mtcars$am),pch=19,main="Data, Model0, 1, 2")
lines(sort(predict(model0)),col="pink",lwd=2)
lines(sort(predict(model1)),col="blue",lwd=2)
lines(sort(predict(model2)),col="green",lwd=2)
legend("topleft", col = c("pink","blue","green"), legend = c("model0","model1","model2"),lty=c(1,1))

plot(sort(resid(model0)),col="pink",main="Residuals: Model0, 1, 2")
lines(sort(resid(model1)),col="blue",lwd=2)
lines(sort(resid(model2)),col="green",lwd=2)
legend("topleft", col = c("pink","blue","green"), legend = c("model0","model1","model2"),lty=c(1,1))
```

```{r,echo=FALSE,fig.height=10}
par(mfrow=c(4,2))
plot(model1,main="Model1")
plot(model2,main="Model2")
```

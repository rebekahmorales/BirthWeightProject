## Final Project Write Up for R Code
## Rebekah Morales
## INFO 523
## Birth weight Data

## Pulling in my original Data
require(lattice)
library(readr)
brthwght.data <- read_csv("birthwt.csv")

## Looking at my orignal Data
summary(brthwght.data)
glimpse(brthwght.data)

## Going to look at initial plots individually to see if I can see anything significant.
require( gridExtra )
pt1 <- dotplot(ftv ~ bwt , main="Birth Weight Vs First Trimester Visits", data=brthwght.data )
pt2 <- dotplot(race ~ bwt,  main="Dot Plot of Birth Weight and Race", data= brthwght.data  )
pt3 <- bwplot( ~ bwt, main="Birth Weight Box Plot", data= brthwght.data  )
pt4 <- bwplot(~ age, main="Boxplot of Mother's Age", data= brthwght.data  )
pt5 <- bwplot( ~lwt, main="Boxplot of Mother's Weight", data= brthwght.data  )
pt6 <- dotplot( ptl ~ bwt,  main="Premature Labors vs Birth Weight", data= brthwght.data  )
grid.arrange( pt1, pt2, pt3, pt4, pt5, pt6, ncol= 2 , nrow= 3 )

## Checking Correlations to see what is going on in my variables
brthwt_matrix <- cor(brthwght.data)
round(brthwt_matrix, 2)
## As I do not care about the Col1 the correlations there do not matter to me. Removing it from the dataset, and rerunning.
colnames(brthwght.data)
brthwght.data <- subset(brthwght.data, select = -c(...1))
brthwt_matrix <- cor(brthwght.data)
round(brthwt_matrix, 2)

## Looking at a Scatterplot matrix. As there are so many variables it is a bit messy.
pairs(brthwght.data)

## Looking at a boxplot for the categorical variables
library(plyr)    
library(ggplot2) 
#Race vs Birthweight
ggplot(brthwght.data, aes(x=as.factor(race), y=bwt)) + geom_boxplot() + xlab("Race: 1=White, 2=African American, 3=Other")+ ylab("Birth Weight")

#Race vs Birthweight
ggplot(brthwght.data, aes(x=as.factor(smoke), y=bwt)) + geom_boxplot() + xlab("Smoking: 0=No, 1=Yes")+ ylab("Birth Weight")

#Hypertension vs Birthweight
ggplot(brthwght.data, aes(x=as.factor(ht), y=bwt)) + geom_boxplot() + xlab("Hypertension: 0=No, 1=Yes")+ ylab("Birth Weight")

#Uterine Irritability vs Birthweight
ggplot(brthwght.data, aes(x=as.factor(ui), y=bwt)) + geom_boxplot() + xlab("Uterine Irritability : 0=No, 1=Yes")+ ylab("Birth Weight")


## Looking at my original full model, that includes all my possible explanitory variables.(low has been removed due to correlation)
brthwght.fit <- lm(bwt ~ race + smoke + ptl + ht + ui + age + lwt + ftv, data=brthwght.data)
summary(brthwght.fit)

## Running assumptions check on original full model
e.wght <- residuals(brthwght.fit)
plot( e.wght ~ bwt, data=brthwght.data)
yhat <- fitted( brthwght.fit )
plot( e.wght ~ yhat )
qqmath( ~ e.wght, distribution = "qnorm" )
## The data appears reasonably normal.  

## I will also look at each of my variables individually with my Response Variable for residuals diagnostics. 

## Residuals plot for Low
## As this is a categorical variable I have two vertical lines. To meet assumptions they should still add to 0 and the lines should be symetric or near identical
## As the line on the left is a lot lower than the line on the right this is not met. 
## I expected to see this though as the indicator low automatically indicates a low birth weight and is more of a double check than a variable. I will associate it with my response variable. 
lowfit <- lm(bwt ~ low , data=brthwght.data)
e.low <- residuals(lowfit)
yhat.low <- fitted( lowfit )
plot( e.low ~ yhat.low )

## Residuals plot for Age
## I would say my assumptions are met here
agefit <- lm(bwt ~ age , data=brthwght.data)
e.age <- residuals(agefit)
yhat.age <- fitted( agefit )
plot( e.age ~ yhat.age )

## Residuals plot for lwt
## Assumpetions are all met here too
lwtfit <- lm(bwt ~ lwt , data=brthwght.data)
e.lwt <- residuals(lwtfit)
yhat.lwt <- fitted( lwtfit )
plot( e.lwt ~ yhat.lwt )

## Residuals plot for race
## for this one the line on the far right has a wider deviation than the others. 
## The line on the far left has more lower numbers and potientially there are outliers. 
racefit <- lm(bwt ~ race , data=brthwght.data)
e.race <- residuals(racefit)
yhat.race <- fitted( racefit )
plot( e.race ~ yhat.race )

## Residuals plot for smoke
## The line on the right had more consistent data in the middle of the line meaning residuals were spread evenly for this line. 
## the left line shows more towards the bottom that may be outliers.
smokefit <- lm(bwt ~ smoke , data=brthwght.data)
e.smoke <- residuals(smokefit)
yhat.smoke <- fitted( smokefit )
plot( e.smoke ~ yhat.smoke )

## Residuals plot for ptl
## for preterm labor, we did not have enough points on the right side and so these appear to look like outliers even when they are not. 
## for the other two main lines they are all centered around 0, and appear to be spread equally.
ptlfit <- lm(bwt ~ ptl , data=brthwght.data)
e.ptl <- residuals(ptlfit)
yhat.ptl <- fitted( ptlfit )
plot( e.ptl ~ yhat.ptl )

## Residuals plot for ht
## While there are less residuals for the left line they both appear to meet my assumptions.
htfit <- lm(bwt ~ ht , data=brthwght.data)
e.ht <- residuals(htfit)
yhat.ht <- fitted( htfit )
plot( e.ht ~ yhat.ht )

## Residuals plot for ui
## While there are less residuals for the left line they both appear to meet my assumptions.
## they both are centered around 0 and seem to be spread the same distance up as down.
uifit <- lm(bwt ~ ui , data=brthwght.data)
e.ui <- residuals(uifit)
yhat.ui <- fitted( uifit )
plot( e.ui ~ yhat.ui )

## Residuals plot for ftv
## there is not enough data on some of the lines for this. However, I would say this does not meet my assumptions. 
## It is spread differently and they jdo ot center around 0. The far left dips more on the lower end and the 2nd to the left is closer to the top. 
ftvfit <- lm(bwt ~ ftv , data=brthwght.data)
e.ftv <- residuals(ftvfit)
yhat.ftv <- fitted( ftvfit )
plot( e.ftv ~ yhat.ftv )

summary(brthwght.fit)


## Checking the Cp and Rap for my new model with low and ftv removed. 
require( leaps )
X.leaps <- cbind(brthwght.data$"age", brthwght.data$"lwt", brthwght.data$"race", brthwght.data$"smoke", brthwght.data$"ptl", brthwght.data$"ht", brthwght.data$"ui", brthwght.data$"ftv")
Cp.temp <- leaps(x=X.leaps, y=brthwght.data$"bwt", method="Cp")
Cp.info=cbind(Cp.temp$which,Cp.temp$size,Cp.temp$Cp)
colnames(Cp.info)=c("age","lwt","race","smoke", "ptl", "ht", "ui", "ftv","p","Cp")
Cp.info

## My model with the lowest Cp(3.3698) included the variables lwt, race, smoke, ht, and ui. 
## My model with the Cp closest to p (p=5, Cp=5.649050) included all variables above minus lwt. 

## Running Rap now
Rap.temp <- leaps(x=X.leaps, y=brthwght.data$"bwt", method="adjr2")
Rap.info=cbind(Rap.temp$which,Rap.temp$size,Rap.temp$adjr2)
colnames(Rap.info)=c("age","lwt","race","smoke", "ptl", "ht", "ui", "ftv", "p","adjR2")
Rap.info

## My model with the highest Rap (0.200083798) included the variables lwt, race, smoke, ht, and ui.
## My model with the second highest Rap(0.196814267) included all the variables above and ptl.

## As both suggested the same three models I will now begin to look at the AIC and AIC Corrected to determine which is the best model.

## Three models to test against each other
test.fit1 <- lm(bwt ~ lwt + race + smoke + ht + ui, data=brthwght.data)
test.fit2 <- lm(bwt ~ lwt + race + smoke + ht + ui + ptl, data=brthwght.data)
test.fit3 <- lm(bwt ~ lwt + race + smoke + ht + ui + ftv, data=brthwght.data)

## Checking AIC and AIC corrected
storeAIC <- AIC(test.fit1, test.fit2, test.fit3, k=3 )
AICc <- storeAIC$AIC + ( 2*(storeAIC$df)^2 + 2*storeAIC$df ) / ( 189- storeAIC$df -1)
cbind( storeAIC, AICc)

## Based off the AIC and AIC corrected my best model under both is the model with fewer variables. 
## These variables include lwt, race, smoke, ht, and ui. 
## As I have already checked the residuals for each of these varaibles I will now check the residuals for this model all togehter. 
nbest.fit <- lm(bwt ~ lwt + race + smoke + ht + ui, data=brthwght.data)
e.wght <- residuals(nbest.fit)
plot( e.wght ~ bwt, data=brthwght.data)
yhat <- fitted( nbest.fit )
plot( e.wght ~ yhat )
qqmath( ~ e.wght, distribution = "qnorm" )

## With this new model all my assumptions appear perfect. There is no pattern in the residuals, it appears to add to 0, and my variance is constant. 
## For my QQ plot it deviates away at the top right but then starts to come back to the line, so it still meets my assumption of normality.

## Running the summary for my new best model
summary(nbest.fit)

#Removing lwt so I can get confidence intervals for everything outside of weight
best.model.new  <- lm(bwt ~ race + smoke + ht + ui, data=brthwght.data)

## Running Confidence intervals for my data
## Races with No smoking no HT and no UI
xnew <- data.frame(race=1, smoke=0, ht=0, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=0, ht=0, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=0, ht=0, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)

## Races with No smoking yes HT and no UI
xnew <- data.frame(race=1, smoke=0, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=0, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=0, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)

## Races with No smoking no HT and yes UI
xnew <- data.frame(race=1, smoke=0, ht=0, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=0, ht=0, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=0, ht=0, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)

## Races with yes smoking no HT and no UI
xnew <- data.frame(race=1, smoke=1, ht=0, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=1, ht=0, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=1, ht=0, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)


## Races with yes smoking yes HT and no UI
xnew <- data.frame(race=1, smoke=1, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=1, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=1, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)

## Races with yes smoking yes HT and no UI
xnew <- data.frame(race=1, smoke=1, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=1, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=1, ht=1, ui=0)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)

## Races with yes smoking no HT and yes UI
xnew <- data.frame(race=1, smoke=1, ht=0, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=1, ht=0, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=1, ht=0, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)

## Races with no smoking yes HT and yes UI
xnew <- data.frame(race=1, smoke=0, ht=1, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=0, ht=1, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=0, ht=1, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)

## Races with yes smoking yes HT and yes UI
xnew <- data.frame(race=1, smoke=1, ht=1, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=2, smoke=1, ht=1, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)
xnew <- data.frame(race=3, smoke=1, ht=1, ui=1)
predict(best.model.new, newdata=xnew, interval="confidence", level=.95)

## The End!

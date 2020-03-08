---
  title: "Food Bank Analysis - Predict Demand in Mobile Pantry - RandomForest, Bootstraping, Mixed Model, Interaction"
subtitle: "Advanced Methods for Statistical Inference"
author: <small>Doris Kuo</small>
  date: <small>12/14/2019</small>
  output:   
  html_document:
  toc: true
toc_float: true
theme: darkly

---
  
   {r setup, include=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE)
 



**Goal**\

- Confirm whether the four factors(Population, County, Month, RuralArea) we choose really affect the demand in a food bank mobile pantry?
  - Use Generalize Linear Model to double check the prediction
- Use bootstraping resampling to check performance

- If we consider interaction or use mixed model, will the performance be better?
  - Population X County
- Month X Temperature
- Check under this sample size and the result how confident we are by Power Analysis

# 0. DataSet
 {r, message=FALSE}
library(tidyverse)
library(ggplot2)
df_raw <- read.csv("Food_Pantry_Cleaned_weather_population.csv")
 



 {r, message=FALSE}
head(df_raw)
summary(df_raw)
str(df_raw)
 


> 1. Original dataset is really small
2. Finding other outsourcing data: Popularity
3. Web scraping for Temperature, Humidity, and Weather Condition....


# 1. Explore the Data
 {r, message=FALSE}
library(GGally)
df_raw%>%
  keep(is.integer)%>%
  ggcorr(palette = "RdBu", label = TRUE)
 

# 2. Modeling and Compare the result
Hypothesis: Temperature,County, Population, Month can be good variables to predict Individuals\
Model: Random Forest and Generalize linear model

</br>
  
  ## Random Forest
   {r,message=FALSE,warning=FALSE}
#split data
library(caTools)
library(rpart)
library(caret)

df<-df_raw%>%
  select(County,Rural.Area,Population,Month,Individuals, Temperature)%>%
  na.omit()
set.seed(1234)
df_set <- df%>%
  pull(Individuals)%>%
  sample.split(SplitRatio = 0.75)
df_train <- subset(df, df_set==TRUE)
df_test <- subset(df, df_set==FALSE)

#Decision Tree
library(rpart)
tree_mod <-
  rpart(
    Individuals~.,
    data = df_train,
    control = rpart.control(cp = 0.001)
  )


library(rpart.plot)
tree_mod


#RandomForest
grid <- expand.grid(.mtry = c(3, 6, 9, 12, 15))
ctrl <-
  trainControl(method = "cv",
               number = 5,
               selectionFunction = "best")
set.seed(1234)
?train
rf.mod <-
  train(
    Individuals ~ ., data = df_train,
    method = "rf",
    trControl = ctrl,
    tuneGrid = grid
  )
#Random Forest Performance

rf_pred <- predict(rf.mod, df_test, type = "raw")
pred<-rf_pred
test <-df_test$Individuals
rmse <- RMSE(test,pred)
rmse
 


## Generalize linear model
 
lmTest = glm(Individuals~., data=df_train, family = gaussian)
summary(lmTest)
 
> From the p-value and the difference between the Residual deviance and Null deviance, the four variables: County, Rural.Area, Population, Month can be good predictor for numbers of individuals.

 
#Predict
lm.pred <- predict(lmTest, df_test,type="response")
#Performance
test <-df_test$Individuals
rmse <- RMSE(test,lm.pred,na.rm=TRUE)
rmse
 
> The RMSE(result performance) is quite similar as the RMSE of random forest

# 3. Resample: support T-value

## Bootstrapping 1000 times
 
teamPerCVars <- dplyr::select(df_train,County,Rural.Area,Population,Month,Individuals)
bootstrapping <- function(df) { 
  df <- df 
  sampledRows <- sample(1:nrow(df), nrow(df), replace = TRUE) 
  df <- df[sampledRows, ] 
  bsMod <- glm(formula = Individuals ~ ., family = gaussian, data = df) 
  results <- broom::tidy(bsMod) 
  return(results)
}
bootstrapping(df_train)
bsteam <- replicate(1000, bootstrapping(df_train), simplify = FALSE) 
bsCombined <- do.call("rbind", bsteam)
 



## Month T-value
 

#forceful t-value
meanEffect <- mean(bsCombined$statistic[bsCombined$term == "Month"]) 
ciUpper <- quantile(bsCombined$statistic[bsCombined$term == "Month"], .975) 
ciLower <- quantile(bsCombined$statistic[bsCombined$term == "Month"], .025) 
hist(bsCombined$statistic[bsCombined$term == "Month"], col = "slategray1") 
abline(v = summary(lmTest)$coefficients["Month","t value"], col = "goldenrod4", lwd = 2) 
abline(v = ciUpper, col = "sienna3", lwd = 2) 
abline(v = ciLower, col = "sienna3", lwd = 2) 
abline(v = meanEffect, col = "sienna3", lwd = 2)
 

## Rural.AreaYes T-value
 
meanEffect <- mean(bsCombined$statistic[bsCombined$term == "Rural.AreaYes"]) 
ciUpper <- quantile(bsCombined$statistic[bsCombined$term == "Rural.AreaYes"], .975) 
ciLower <- quantile(bsCombined$statistic[bsCombined$term == "Rural.AreaYes"], .025) 
hist(bsCombined$statistic[bsCombined$term == "Rural.AreaYes"], col = "slategray1") 
abline(v = summary(lmTest)$coefficients["Rural.AreaYes","t value"], col = "goldenrod4", lwd = 2) 
abline(v = ciUpper, col = "sienna3", lwd = 2) 
abline(v = ciLower, col = "sienna3", lwd = 2) 
abline(v = meanEffect, col = "sienna3", lwd = 2)
 


## Population T-value
 {r, echo=FALSE}
meanEffect <- mean(bsCombined$statistic[bsCombined$term == "Population"]) 
ciUpper <- quantile(bsCombined$statistic[bsCombined$term == "Population"], .975) 
ciLower <- quantile(bsCombined$statistic[bsCombined$term == "Population"], .025) 
hist(bsCombined$statistic[bsCombined$term == "Population"], col = "slategray1") 
abline(v = summary(lmTest)$coefficients["Population","t value"], col = "goldenrod4", lwd = 2) 
abline(v = ciUpper, col = "sienna3", lwd = 2) 
abline(v = ciLower, col = "sienna3", lwd = 2) 
abline(v = meanEffect, col = "sienna3", lwd = 2)
 

## County T-value
 {r, echo=FALSE}
meanEffect <- mean(bsCombined$statistic[bsCombined$term == "CountyKosciusko"]) 
ciUpper <- quantile(bsCombined$statistic[bsCombined$term == "CountyKosciusko"], .975) 
ciLower <- quantile(bsCombined$statistic[bsCombined$term == "CountyKosciusko"], .025) 
hist(bsCombined$statistic[bsCombined$term == "CountyKosciusko"], col = "slategray1") 
abline(v = summary(lmTest)$coefficients["CountyKosciusko","t value"], col = "goldenrod4", lwd = 2) 
abline(v = ciUpper, col = "sienna3", lwd = 2) 
abline(v = ciLower, col = "sienna3", lwd = 2) 
abline(v = meanEffect, col = "sienna3", lwd = 2)
 
 {r, echo=FALSE}
meanEffect <- mean(bsCombined$statistic[bsCombined$term == "CountyMarshall"]) 
ciUpper <- quantile(bsCombined$statistic[bsCombined$term == "CountyMarshall"], .975) 
ciLower <- quantile(bsCombined$statistic[bsCombined$term == "CountyMarshall"], .025) 
hist(bsCombined$statistic[bsCombined$term == "CountyMarshall"], col = "slategray1") 
abline(v = summary(lmTest)$coefficients["CountyMarshall","t value"], col = "goldenrod4", lwd = 2) 
abline(v = ciUpper, col = "sienna3", lwd = 2) 
abline(v = ciLower, col = "sienna3", lwd = 2) 
abline(v = meanEffect, col = "sienna3", lwd = 2)
 
 {r, echo=FALSE}
meanEffect <- mean(bsCombined$statistic[bsCombined$term == "CountySt Joseph"]) 
ciUpper <- quantile(bsCombined$statistic[bsCombined$term == "CountySt Joseph"], .975) 
ciLower <- quantile(bsCombined$statistic[bsCombined$term == "CountySt Joseph"], .025) 
hist(bsCombined$statistic[bsCombined$term == "CountySt Joseph"], col = "slategray1") 
abline(v = summary(lmTest)$coefficients["CountySt Joseph","t value"], col = "goldenrod4", lwd = 2) 
abline(v = ciUpper, col = "sienna3", lwd = 2) 
abline(v = ciLower, col = "sienna3", lwd = 2) 
abline(v = meanEffect, col = "sienna3", lwd = 2)
 
 {r, echo=FALSE}
meanEffect <- mean(bsCombined$statistic[bsCombined$term == "CountyLaPorte"]) 
ciUpper <- quantile(bsCombined$statistic[bsCombined$term == "CountyLaPorte"], .975) 
ciLower <- quantile(bsCombined$statistic[bsCombined$term == "CountyLaPorte"], .025) 
hist(bsCombined$statistic[bsCombined$term == "CountyLaPorte"], col = "slategray1") 
abline(v = summary(lmTest)$coefficients["CountyLaPorte","t value"], col = "goldenrod4", lwd = 2) 
abline(v = ciUpper, col = "sienna3", lwd = 2) 
abline(v = ciLower, col = "sienna3", lwd = 2) 
abline(v = meanEffect, col = "sienna3", lwd = 2)
 
 {r, echo=FALSE}
meanEffect <- mean(bsCombined$statistic[bsCombined$term == "CountyStarke"]) 
ciUpper <- quantile(bsCombined$statistic[bsCombined$term == "CountyStarke"], .975) 
ciLower <- quantile(bsCombined$statistic[bsCombined$term == "CountyStarke"], .025) 
hist(bsCombined$statistic[bsCombined$term == "CountyStarke"], col = "slategray1") 
abline(v = summary(lmTest)$coefficients["CountyStarke","t value"], col = "goldenrod4", lwd = 2) 
abline(v = ciUpper, col = "sienna3", lwd = 2) 
abline(v = ciLower, col = "sienna3", lwd = 2) 
abline(v = meanEffect, col = "sienna3", lwd = 2)
 

> Population and County's T-values have some issues..

# 4. Explore Relationship by graph
## Population: Randomly Shift samples
 {r, message=FALSE}
df%>%
  ggplot(aes(Population, Individuals))+
  geom_point(color="#00AFBB",alpha=0.4)+
  theme_minimal()+
  geom_smooth(method='lm',color="#108F98",alpha=0.3)+
  ggtitle("Population X Individuals")

#devtools::install_github("saberry/inferviz",force = TRUE)
library(inferviz)
simViz(df, Population, Individuals,  distractors = 5, answer = FALSE)
 

> Though the graph is not perfect, but is still ok. It means when we use Population to predict Individual in the Generalized Linear Model, the outcome didn't come out by chance. 

## County: Violin plot
 
df%>%
  ggplot(aes(County,  Individuals)) +
  geom_violin(aes(x = reorder(County, Individuals, FUN = median)),fill='#00AFBB',alpha=0.3)+
  coord_flip()+
  theme_minimal()

 

> Different counties do have different distribution of individuals, though they the majority of the number of individuals are between 0-750.

# 5. Check Interaction
## Population X County
 
lmTest = glm(Individuals~., data=df_train, family = gaussian)
summary(lmTest)
lmTest_inter = glm(Individuals~ (County*Population) + Rural.Area + Month + Temperature, data=df_train, family = gaussian)
summary(lmTest_inter)

library(interactions)
interact_plot(lmTest_inter, pred = Population, modx=County )

#Predict
lm.pred <- predict(lmTest_inter, df_test,type="response")
#Performance
test <-df_test$Individuals
rmse <- RMSE(test,lm.pred,na.rm=TRUE)
rmse
 

>Should take the interaction relationship between county and population into consideration.

# 6. Mixed Model: Month X Temperature
 {r, message=FALSE}
df%>%
  ggplot(aes(Temperature,Individuals,group = Month, color = as.factor(Month)))+
  geom_smooth(method='lm',se = FALSE)+
  theme_minimal()

library(lme4)
df$Month
lmTest = glm(Individuals~., data=df_train, family = gaussian)
summary(lmTest)
lmTest_slope = lmer(Individuals~  County + Population + Rural.Area + (Temperature|Month) , data=df_train)
summary(lmTest_slope)

#Predict
lm.pred <- predict(lmTest_slope, df_test,type="response")
#Performance
test <-df_test$Individuals
rmse <- RMSE(test,lm.pred,na.rm=TRUE)
rmse



randomSlopesPred <- predict(lmTest_slope )
linPred <- predict(lmTest)


allPred <- cbind(actual = df_train$Individuals,
                 linear = linPred,
                 slopemodel = randomSlopesPred
)
plot(allPred[, "actual"], allPred[, "linear"])
plot(allPred[, "actual"], allPred[, "slopemodel"])
 

#> Though on the graph, the slope of temperature are different, when we consider the different slope in the mixed model, the result doesn't change a lot

# 7. Power Analysis: How confident we are


library(pwr)
lmTest = lm(Individuals~., data=df_train)
summary(lmTest)
m_r_adjusted = summary(lmTest)$adj.r.squared
m_r_adjusted
m_f2 = m_r_adjusted /(1-m_r_adjusted)
m_f2
# u = k-1 =9 = the 1st DF /v= 244 = the second DF/ f2= m_f2
pwr.f2.test(u=9, v=244, f2=m_f2)

> Though there have some issues in the model, the power(confidence) under this sample to predict the current performance we have is 1.


 
library(pwr)
lmTest = lmer(Individuals~  County + Population + Rural.Area + (Temperature|Month) , data=df_train)
summary(lmTest)
m_r_adjusted = summary(lmTest)$adj.r.squared
m_r_adjusted
m_f2 = m_r_adjusted /(1-m_r_adjusted)
m_f2
# u = k-1 =9 = the 1st DF /v= 244 = the second DF/ f2= m_f2
pwr.f2.test(u=9, v=244, f2=m_f2)
 


# Conclusion

> 1. Linear model did actually just similar as Random Forest probably because our dataset is pretty small. Based on Occam's razor, we should use the linear model.
2. To improve our model, besides finding more features (ex: weekdays), we should consider the reaction between population and county

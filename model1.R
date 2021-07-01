##REFERENCES
#https://www.youtube.com/watch?v=2Sb1Gvo5si8 
#https://www.analyticsvidhya.com/blog/2020/12/predicting-using-linear-regression-in-r/ 
# http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/

##Loading data into r scipt and summarizing
Data<- read.csv("data.csv")
summary(Data)
str(Data)
##Storing value to be predicted
p<-as.data.frame(9.25)
colnames(p)<-"Hours"

##Splitting data into training and test data set
library(caTools)
split<- sample.split(Data, SplitRatio = 0.5)
train<- subset(Data,split="TRUE")
test<- subset(Data,split="FALSE")

##Visuvalizing data
plot(Data)
ggplot(Data,aes(x = Hours, y = Scores)) + geom_point() +geom_smooth(method = "lm")

##Modeling a ML model
Model<-lm(Scores~Hours,Data)
summary(Model)

##> summary(Model)

##Call:
##  lm(formula = Scores ~ Hours, data = Data)

##Residuals:
##  Min      1Q  Median      3Q     Max 
##-10.578  -5.340   1.839   4.593   7.265 

##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)   2.4837     2.5317   0.981    0.337    
##Hours         9.7758     0.4529  21.583   <2e-16 ***
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##Residual standard error: 5.603 on 23 degrees of freedom
##Multiple R-squared:  0.9529,	Adjusted R-squared:  0.9509 
##F-statistic: 465.8 on 1 and 23 DF,  p-value: < 2.2e-16

##Predict model
pred<-predict(Model, test)
pred

##Comparing predicted vs actual
plot(test$Scores,type= "l",lty=1.8, col="red")
lines(pred,type = "l",lty=1.8,col="blue")
plot(pred,type= "l",lty=1.8, col="blue")
##Plotting model
ggplot(model,aes(x= Hours,y= Scores))+geom_point()+geom_smooth(method = "lm")

##Prediction
predict(model,newdata = p)

##Accuracy
rmse<- sqrt(mean(pred-Data$Scores)^2)

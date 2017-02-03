library(ggplot2)
data(diamonds)
#uses lm() - https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html

#get a random collection of 500 samples of diamond data
diamSmall <- diamonds[sample(1:dim(diamonds)[1],500),]

################################################1
#Equation: Price = theta_1 _ theta_2*carat + esilon
M1 = lm(price~carat, diamSmall);
#price is the target and carat is the single X we are using in this model

theta<-coef(M1)
#this will have 2 values: theta1 is the intercept and theta2 will be the 'carat' weight

#graph the data and draw a line using the coefficeints we just got
ggplot(diamSmall,aes(carat,price)) + geom_point() + geom_abline(intercept=theta[1],slope=theta[2],size=2,color=I("red"))

#predict 3 values of carat  - carat = 1, carat=2, carat=3
#note that since there is simply 1 X column this works
predict(M1, data.frame(carat=c(3,4,5)))

#show entire summary
summary(M1)

#get a specific stat from summary
summary(M1)$r.squared
summary(M1)$residuals

#you can also use residuals()
residuals(M1)

#to get the residuals, get the mean of the squared residuals
#this gets the squars of the errors; squaring it makes sure positive errors do not cancel negative errors and vice versa
#note that technically RSS uses sum() and not mean
#also note that mean is the same as the sum, yet divided by the number of residuals
mean(residuals(M1)^2)


################################################2
#this adjusts for non-linearity
M2<-lm(price~carat+I(carat^2)+I(carat^3), diamSmall)
theta<-coef(M2)
#make 500 points, a floats between 0-3
X<-seq(0,3,length=500)

#make predictions y
Y<-theta[1]+theta[2]*x+theta[3]*x^2+theta[4]*x^3

#visualize the regression
D<-data.frame(x=X,y=Y)
ggplot(D,aes(x=X,y=Y))+geom_line(size=2,color=I("red")) + geom_point(data=diamSmall, aes(x=carat,y=price))

#find the R^2 (r squared)
summary(M2)$r.squared

mean(residuals(M2)^2)

################################################3
#two variables
M3<-lm(price~carat+color,diamSmall)
summary(M3)$r.squared
mean(residuals(M3)^2)


################################################4
#log of the varaibles
M4<-lm(log(price)~log(carat),diamSmall)
theta<-coef(M4)

#plot the scatterplot of log(price) vs log(caret)
ggplot(diamSmall,aes(log(carat),log(price))) + geom_point() + geom_abline(intercept=theta[1], slope=theta[2],size=2,color=I("red"))
summary(M4)$r.squared
mean(residuals(M4)^2)

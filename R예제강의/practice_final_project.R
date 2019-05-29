rm(list=ls())

setwd("C:/Users/1pc/Desktop/4.28파이썬및 빅데이터/R예제강의")

a<-read.csv("house_population.csv")
str(a)
attach(a)

par(mfcol=c(1,2))
# histogram graph
hist(price,breaks=5,probability = T,main="KOREA : Histogram of House Price")
lines(density(price),col="red")
hist(price_j,breaks=5,probability = T,main="JAPAN : Histogram of House Price")
lines(density(price_j),col="blue")

# time series graph
par(mfcol=c(1,2))
plot(year,price,main="KOREA : House Price (%, 2015=100)",type="o", lwd=2, col="red")
plot(year_j,price_j,main="JAPAN : House Price (%, 2000=100)",type="o", lwd=2, col="blue")
plot(year,population,main="KOREA : Production Population (%)",type="o", lwd=2, col="red")
plot(year_j,population_j,main="JAPAN : Production Populaton (%)",type="o", lwd=2, col="blue")


# scatter graph
par(mfcol=c(1,2))
plot(population,price,main="KOREA : Population vs House Price ", cex=2.5, col="red")
plot(population_j,price_j,main="JAPAN : Population vs House Price ", cex=2.5, col="blue")


# histogram graph
hist(price,breaks=5,probability = T,main="KOREA : Histogram of House Price")
lines(density(price),col="red")
hist(price_j,breaks=5,probability = T,main="JAPAN : Histogram of House Price")
lines(density(price_j),col="blue")

# Box plot
install.packages("car")
library(psych)
par(mfcol=c(1,1))
boxplot(price,price_j, data=a,col=c("red","blue"), xlab=c("Korea                   Japan"),main="Boxplot of House Price(%)")


# Parameter Processing
price2<-price/population
p<-c(price2,(price_j/population_j))

korea<-rep("K",27)
g<-c(korea,rep("J",27))
n<-cbind(p,g)
n<-data.frame(n)
n$p<-as.numeric(n$p)
str(n)

# box plot & two saple t-test
par(mfcol=c(1,1))
levels(n$g)<-c("J","K")
n$g=factor(n$g,levels=c("K","J"))

boxplot(p~g, data=n,col=c("red","blue"), ylab="price/population",levels=c("K","J"))

var.test(p~g,data=n)
t.test(p~g,data=n,var.equal=TRUE)



install.packages("car")
library(car)
library(MASS)
# Parameter Corelation EDA
b <- subset(a, select = -c(year,year_j,price_j,population_j))
str(b)
plot(b)
cor(b)

# Multi-Regression Model, Multicollinearity(vif)
c<-lm(price~.,data=b)
summary(c)
vif(c)

# Multi-Regression Model Tunning
d <-subset(b,select=-c(apt))
c2<-lm(price~.,data=d)
summary(c2)
c3<-lm(price~population+rent, data=d)
summary(c3)
plot(c3)

# predicton scatter plot
par(mfcol=c(1,1))
pred<-predict(c3,newdata=d)		#예측치 생성
plot(price,pred, pch=19, cex=2, col="grey")				#예측치 산점도 그리기
points(price, pred,cex=2)

k<-lm(pred~price,data=d)
abline(k,col="red",lwd=3)	

# prediction data loadiing
a2<-read.csv("population_future.csv")
plot(a2$year,a2$population,main="Predicted Population (%)",type="o", lwd=2, col="red")

# predicton modeling #2
par(mfcol=c(1,2))
c4<-lm(price~population, data=d)
summary(c4)
pred2<-predict(c4,newdata=d)	
plot(price,pred2, pch=19, cex=2, col="pink")				#예측치 산점도 그리기
points(price, pred2,cex=2)

j<-lm(pred2~price,data=d)
abline(j,col="red",lwd=3)	

a2<-a2[-1,]
pred3<-predict(c4,newdata=a2)	
px<-c(a$year,a2$year)
py<-c(a$price,pred3)
plot(px,py,main="Predicted Price (%)",type="o", lwd=2, col="red",xlim=c(1990,2020),ylim=c(40,120))

########################################################


library(manipulate)
library(UsingR)
data(galton)
myHist <- function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu,mu),c(0,150),col="red",lwd=5)
  mse <-mean((galton$child-mu)^2)
  text(63,150,paste("mu =",mu))
  text(63,140,paste("mse =",round(mse,2)))
}

manipulate(myHist(beta),beta=slider(0.6,1.2,step=.02))

fit <-lm(I(child-mean(child)) ~ I(parent-mean(parent)) - 1,data=galton)

cov(x,y)=(1/n-1)*sum((x-mean(x))(y-mean(y))) 
cov(x,y)=(1/n-1)*sum(x*y-n*mean(x)*mean(y))
beta=cor(Y,X)*sd(Y)/sd(X)
mean(Y) = beta0 +beta*mean(X)
beta=sum(X*Y)/sum(X^2)


###q1q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
xw=x*w
mu=weighted.mean(x,w)
mu
#q1q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x - 1)
 #q1q3
data(mtcars)
lm(mtcars$mpg ~ mtcars$wt,data=mtcars)

#q1q4
#q1q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
mean(x)
sd(x)
(x-mean(x))/sd(x)





###################quiz2################
#q2q1

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~x)
sumCoef <-summary(fit)$coefficients
confIntervalBetea0 =  sumCoef[1,1] +c(-1,1)*qt(.975,df=fit$df)*sumCoef[1,2]
confIntervalBetea0
confIntervalBetea1 =  sumCoef[2,1] +c(-1,1)*qt(.975,df=fit$df)*sumCoef[2,2]
confIntervalBetea1

#q2q2
sigma <-sqrt(sum(resid(fit)^2)/(length(x)-2)) # ou lie output de summary. sigma 0 summary(fit)$sigma est residual standard deviation.

#q2q3
library(UsingR)
data(mtcars)
fitmcars <- lm(mpg ~ wt,data=mtcars)
fitmcars
coef<-summary(fitmcars)$coefficients
#q2q4
averageWt <- mean(mtcars$wt)
newd <-data.frame(wt=c(averageWt))
newd2 <-data.frame(wt=c(3.0))


predConf<-predict(fitmcars,newd,interv = ("confidence"), se.fit = FALSE,level = 0.95)
predInt<-predict(fitmcars,newd2,interv = ("prediction"))



 
#q2q6
fit=fitmcars
sumCoef <-summary(fit)$coefficients
confIntervalBetea0 =  sumCoef[1,1] +c(-1,1)*qt(.975,df=fit$df)*sumCoef[1,2]
confIntervalBetea0
confIntervalBetea1 =  sumCoef[2,1] +c(-1,1)*qt(.975,df=fit$df)*sumCoef[2,2]
confIntervalBetea1
#q2q9
pred<-predict(fitmcars)
yo = mtcars$mpg -coef(fitmcars)[1]
r=(sum((pred-mtcars$mpg)^2))/sum(yo^2)
r
fit0 <-lm(mpg~fact,data=mtcars)
summary(fit0)



#q3q1

car <- mtcars
car$fact = relevel(factor(car$cyl),ref="4")
fit2 <- lm(mpg ~ wt+fact,data=car)
summary(fit2)

#q3q2
fit3<-lm(mpg ~fact,data=car)
summary(fit3)

fit4 <-lm(mpg ~fact*wt,data=car)
summary(fit4)

anova(fit0,fit2)
anova(fit2,fit4)


#q3q4
fitxx<-lm(mpg ~ I(wt*.5) + factor(cyl), data = mtcars)
summary(fitxx)
yy=mtcars$mpg
xx=mtcars$wt/2
plot(xx,yy)
abline(fitxx)
plot(fitxx)

#q3q5

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit6 <-lm(y ~x)
influence.measures(fit6)
hatvalues(fit6)
round(dfbetas(fit6)[c(1:5),2],3)
plot(x,y)
abline(fit6)
###########proj##########
library(UsingR)
require(stats)
require(graphics)
library(ggplot2)
data(mtcars)
fit <-lm(mpg ~.,data=mtcars)
corMatrix=rcorr(as.matrix(mtcars),type="pearson")
cor=corMatrix$r
corMpg=cor[1,]
corAm=cor[9,]
summary(fit)

# fitnf = lm(mpg ~wt+ cyl*am,data=mtcars)
# summary(fitnf)

 
ds <-split(mtcars,list(mtcars$cyl,mtcars$am),drop=TRUE)
v<-sapply(ds,function(x) mean(x[,1]))
# To display different lines in different facets, you need to
# create a data frame.
 
hline.data <- data.frame(mean = unname(v), cyl = c(4,4,6,6,8,8), am = c(0,1,0,1,0,1))
 



#hdf<-data.frame(mean=unname(v), factCyl=c(4,6,8,4,6,8),factAm=c(6,6,6,1,1,1))
#dummy2 <- data.frame(X = c("A", "B"), Z = c(1, 0))
plot3<- ggplot(data=mtcars, aes(x = wt, y=mpg))+geom_point(aes(color = cyl))


#datf <-expand.grid(fc=factor(mtcars$cyl),fa=factor(mtcars$am))
plot3=plot3+geom_smooth(method = "lm")+ facet_grid(cyl ~ am)+labs(title="mpg depending on weight for fixed cyl and am")  

plot3 =plot3+geom_hline(aes(yintercept= mean,color=am+3),hline.data,size=0.9,show_guide = TRUE)
print(plot3)
                
               
                 


plot3<- qplot(mpg ,wt, facets= cyl ~.,data=car0,geom=c("point","smooth"), method= "lm" ,color=cyl) 
abline(h=mean(car0$mpg,na.rm=T),lwd=2,col="red")
print(plot3)



plot3<- plot(plot3Data$Sub_metering_Data ~  plot3Data$Date_Time,ylab="Energy sub metering", xlab = "", col= plot3Data$Sub_metering ,type = "l") 
print(plot3)





# pairs(mtcars,panel=panel.smooth, rows = 2,col=3+(mtcars$am==1)+(mtcars$vs==1))
library(UsingR)
require(stats)
require(graphics)
library(ggplot2)
data(mtcars)
pairs(mtcars, main = "mtcars data")
coplot(mpg ~ wt|as.factor(cyl) * as.factor(am), data = mtcars,
       panel = panel.smooth, rows = 1)





summary(fit)
fitcyl <-lm(mpg ~ factor(cyl),data=mtcars)
fitam <-lm(mpg ~ am,data=mtcars)
summary(fitam)
fitwc <-lm(mpg ~factor(cyl)+wt,data=mtcars)
fitcwa<-lm(mpg ~factor(cyl)+wt+factor(am),data=mtcars)
fitint<-lm(mpg ~factor(cyl)+wt+factor(am)+factor(cyl):factor(am),data=mtcars)

plot(fitint2)


plot(mtcars$wt,mtcars$mpg,col=3+(mtcars$am==1))
abline(fitcwa)
abline(fitwc)
abline(fitcyl)
abline(fitint)

summary(fitint)

anova(fitcyl,fitwc)
anova(fitwc,fitcwa)

fitam <- lm(mpg ~factor(am),data=mtcars)
summary(fitam)
cor(mtcars$mpg,mtcars$am)



p <- qplot(mpg, wt, data=mtcars, facets = cyl ~ am)
hline.data <- data.frame(z = c(22.90000 ,19.12500 ,15.05000 ,28.07500 ,20.56667 ,15.40000), cyl = c(4,4,6,6,8,8), am = c(0,1,0,1,0,1))
p + geom_hline(aes(yintercept = z), hline.data) 


p <- qplot(mpg, wt, data=mtcars, facets = cyl ~ am)
hline.data <- data.frame(z = mean, cyl = c(4,4,6,6,8,8), am = c(0,1,0,1,0,1))
p + geom_hline(aes(yintercept = z), hline.data) 






##################

data(swiss)
require(stats)
require(graphics)
pairs(swiss,panel=panel.smooth,col=3+(swiss$catholic >50))

fswiss <-lm(Fertility ~. ,data=swiss)
summary(fswiss)


########quizz 4##################
#q4q1
library(MASS)
data(shuttle)

sh <-shuttle
rm(shuttle)
sh$use1 <- as.numeric(sh$use) 
sh$wind1 = as.numeric(sh$wind)

for(i in 1:nrow(sh)){
if(sh$wind1[i]==2){
  sh$wind1[i]=0
}
}
for(i in 1:nrow(sh)){
  if(sh$use1[i]==1){
    sh$use1[i] = 0
  }else{
    sh$use1[i] = 1
  }

}
sh$wind2=relevel(sh$wind,ref="tail")
fit <-glm(use1~wind2,data=sh,family = "binomial")
summary(fit)
round(exp(fit$coef),3)

#q4q2
fit2 <-glm(use1 ~wind + magn,data=sh,family = "binomial")
summary(fit2)
round(exp(fit2$coef),3)

#q4q3
sh$use2=1-sh$use1
fit <-glm(use2~wind,data=sh,family = "binomial")
summary(fit)
round(exp(fit$coef),3)

#q4q4
sh$use = relevel(sh$use,ref="1")
fit <-glm(use~wind,data=sh,family = "binomial")
summary(fit)
round(exp(fit$coef),3)



if(as.numeric(sh$use)==2){
  sh$usen = 0
}else{
  sh$usen = 1
}

data(InsectSprays)
ins <-InsectSprays
ins$spray =  relevel( factor(ins$spray),ref="B")
ins$spray1 <- as.numeric(ins$spray)
fit4 <-glm(count ~ spray,data=ins)
summary(fit4)
round(exp(fit4$coef),3)



library(plyr)
biggest_worries <- arrange(fatalities,desc(x))

title = paste("Fatalities from Events")



library(ggplot2)
ggplot(data = biggest_worries, aes(Group.1, x)) + geom_bar(stat = "identity") + 
  xlab("Event") + ylab("Fatalities") + ggtitle(title) + 
  coord_flip() + theme(legend.position = "none")



library(UsingR)
data(father.son)
x <- father.son$fheight
mean(x)
sd(x)
mean(x) + c(-1,1)*2*qnorm(0.975)*sd(x)/sqrt(length(x))


mean(x) + qnorm(c(0.025, 0.975)) * sd(x) / sqrt(length(x)) 
poisson.test(600,1)
##qiz2
st = 6/(qt(0.95,8))



n1=10
n2=10
sd1=sqrt(0.6)
x1=3
x2=5
sd2=sqrt(0.68)
conf=0.05
 
# n1=9
# n2=9
# x1=-3
# x2=1
# sd1=1.5
# sd2=1.8

# example pdf page 14
# n1=8
# n2=21
# x1=132.86
# x2=127.44
# sd1=15.34
# sd2=18.23

 

interval <-function(n1,n2,x1,x2,sd1,sd2,conf) {

sp <- sqrt( ((n1-1) * sd1^2 + (n2-1) * sd2^2) / (n1 + n2 - 2))
int = x1-x2 + c(-1, 1) * qt(1-conf/2, (n1+n2-2)) * sp * (1 / n1 + 1 / n2)^.5
print("SP:")
print(sp)
print("interv:")
int
}

######################
 #Quiz q6

intervalUnequal <- function(n1,n2,x1,x2,sd1,sd2,conf,tt=TRUE){
  sdn1 = (sd1^2)/n1 
  sdn2=(sd2^2)/n2
  num = (sdn1 +sdn2)^2
  df= num/ ((sdn1^2)/(n1-1)  +(sdn2^2)/(n2-1))

  if(tt){
    tdf= qt(1-conf/2,df) 
  }else{
    tdf=qnorm(1-conf/2,df)
  }

  x1-x2 +c(-1,1)*tdf*sqrt(sdn1+sdn2)
}


###q2
2/3*qt(.95,8) 
###q1
1100 +c(-1,1)*qt(0.975,8)*30/3


###quiz4

 
base = c(    140 	,
          	138 ,	
          	150 ,	
          	148 ,	
           	135 )
follow =c(  132,
            135,
            151,
            146,
            130)
t.test(follow-base)


#q4q2
ts = (X-110)/(30/sqrt(9)) 
abs(ts) >= qt(1-0.05/2,8) = 2.306004
abs(x-1100) >= 10*2.306004 = 23.06004
1100-23 <=X <= 1100+23
1077 <= X <= 1123
 #q4q3
 pbinom(2, prob = .5, size = 4, lower.tail = FALSE)

The answer to 1 is 0.1841
The answer to 2 is 0
OK

#q4q4
lambda0 = 1/100
n=10
rate= lambda0*1787
ppois(10,rate,lower.tail = TRUE)



#q4q5
power.t.test(n=18,delta= , sd=sigma, type="one.sample",alt="one.sided")$power





#q4q7
conf=0.1
z= qnorm(1-conf/2)
n=100
sigma=0.04
mua=0.01
mu0=0
pnorm(mu0+z*sigma/sqrt(n),mean=mua,sd=sigma/sqrt(n),lower.tail=FALSE)

#q4q8
s=.04
(qnorm(.95)+qnorm(.9))^2*s^2/.01^2

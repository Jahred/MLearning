####project 1 
rexp(n, lambda) 

# dexp(x, rate = 1, log = FALSE)
# pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
# qexp(p, rate = 1, lower.tail = TRUE, log.p = FALSE)
# rexp(n, rate = 1)

lambda = 0.2 
n=40
numSim =1000

hist(pop <-rexp(n, lambda) ,col = "cyan",main = "Exponential Distribution Lambda= 0.2")
abline(v=mean(pop) ,col="yellow" ,lwd =2)
legend("topright", c("Mean"), fill=c("yellow"))
dev.copy(png,file="Plot1.png",width=480,height=480)
dev.off()

means = NULL
for (i in 1 : numSim) { means = c(means, mean(rexp(n, lambda)))
}
h<-hist(means,col="gray",main = "Distribution of 40 means of Exponential Lambda= 0.2",cex.axis=0.8)
abline(v = mean(means), col = "green", lwd = 2)
abline(v = 1/lambda, col = "red", lwd = 2)
legend("topright",cex=0.6, c("mean of Exponential (1/lambda)", "Mean of Normal(1/lambad,...)"), fill=c("green", "red"))
xfit<-seq(min(means),max(means),length=40) 
yfit<-dnorm(xfit,mean=mean(means),sd=sd(means)) 
yfit <- yfit*diff(h$mids[1:2])*length(means) 
lines(xfit, yfit, col="black", lwd=2)

var40 = var(means)
varExponential = 1/lambda^2
theoVar = varExponential/n






#  lines(density(rnorm(1/lambda,1/lambda*sqrt(n)), adjust=2), lty="dotted", col="darkgreen", lwd=10) 





dev.copy(png,file="Plot2.png",width=480,height=480)
dev.off()



library(datasets)
data(ToothGrowth )
 summary(ToothGrowth)
library(ggplot2)
#hist(mean(ToothGrowth$len,ToothGrowth$dose,data= ToothGrowth,color=supp,geom=c("line"),facets= supp ~. )

 g1 = subset(ToothGrowth,as.character(ToothGrowth$supp)=="VC") 
 g1=g1[,c(1,3)]
 g2 = subset(ToothGrowth,as.character(ToothGrowth$supp)=="OJ") 
 g2=g2[,c(1,3)]
 
 
with(ToothGrowth,plot(dose,len,type = "l",col="black",xlab="",ylab="Energy sub metering"))
t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf


setwd("~/git/statisticalInference")
library(knitr)
knit2pdf( "statisticalInf_PA1_template.Rmd",  "inference.pdf")




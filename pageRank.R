pagerank <-function( matrx = matrix(byrow = TRUE,nrow = 3,ncol = 3,c(0,0,0,
                                                                     0.5,0,0,
                                                                     0.5,1,1)), beta =0.7, epsilon=0.0001,maxiter=50000,k=1){
  library(Matrix)
#   maxiter= 10000
#   beta=0.7
# epsilon=0.0001
#   matrx2 = matrix(byrow = TRUE,nrow = 3,ncol = 3,c(0,0,1,0.5,0,0,0.5,1,0))
 n <-nrow(matrx)
  #library(sna)

  x0 <-1/n
 
  r0 = matrix(ncol = 1,nrow = n,rep(x0,n))
  rend = matrix(ncol = 1,nrow = n)
iter=0
  repeat {
   
      rend <- beta * ( matrx %*% r0 )
      #la forme general est s = sum(rend[,1]) mais dans la forme de google il faut  s= (1-beta)/n
     #s <- sum(rend[,1]) 
  #s= (1-beta)/n
       rend <- rend +  k*((1-beta) / n)
  absl <- abs(r0-rend)
  #print(absl)
 # print(sum(absl[,1]))
    if(iter >= maxiter || sum(absl[,1]) < epsilon ){
     
        break
      }
      r0 <-rend
  iter=iter+1
  }
  rend
}



 

0.5*0.7
require(matrix)

D = matrix(byrow = TRUE,nrow = 6,ncol = 6,c(2,0,0,0,0,0,
                                             0,3,0,0,0,0,
                                             0,0,2,0,0,0,
                                             0,0,0,3,0,0,
                                             0,0,0,0,2,0,
                                             0,0,0,0,0,2
                                             ))
A=matrix(byrow = TRUE,nrow = 6,ncol = 6,c(0,1,1,0,0,0,
                                          1,0,0,1,0,1,
                                          1,0,0,1,0,0,
                                          0,1,1,0,1,0,
                                          0,0,0,1,0,1,
                                          0,1,0,0,1,0
))

L= D-A

Eing =eigen(L,symmetric = TRUE)
val = Eing$values
val
Eing
sort(val)

v2= Eing$vectors[,5]
 




h <- function(x,a,b,mod=11){
 num <- (a*x+b) %% mod
 print(num)
 print(intToBits(num)[4:1])
}
#sequ <- c(3,1,4,1,5,9,2,6,5)
sequ = c(1,2,3,4,5,6,7,8,9,10)
exo <- function(a,b){for(i in sequ){
  print("==========")
  print(i)
  h(i,a,b)
 }
}

stream1 <- runif(3, min = 1, max = 75)
t1 = 9
t2 = 49
t3 = 54
36*9+8*16+4*1
stream = c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5)

ams <-function(a,st){
  counts = c(0,0,0,0,0,0,0,0,0,0)
  q2 = c(0,0,0,0,0,0,0,0,0,0)
  k = floor(a/10)
  start = a %% 10
  for(i in a :75){
     
      j=(i %% 10)
       if(j==0){
         j=10
       }
    
    counts[j]=counts[j]+1
    
   # print(j)
  }

 for(h in 1:10){
   q2[h] = h*counts[h]*counts[h] 
   
 }
#print(sum(q2))
counts
}
trip <- function( a,b,c,st){
  
  qa = c(0,0,0,0,0,0,0,0,0,0)
  qb = c(0,0,0,0,0,0,0,0,0,0)
  qc = c(0,0,0,0,0,0,0,0,0,0)
  sums= c(0,0,0)
  counta = ams(a,st)
  countb = ams(b,st)
  countc = ams(c,st)
  
  ha=a %%10
  if(ha == 0){
    ha=10
  }
  hb=b %%10
  if(hb == 0){
    hb=10
  }
  hc=c %%10
  if(hc == 0){
    hc=10
  }
    qa[ha] = (75-a)*(2*counta[ha] -1)
  
    qb[hb] = (75-b)*(2*countb[hb] -1)
    qc[hc] = (75-c)*(2*countc[hc] -1)
    
  
  sums[1] = qa[ha]
  sums[2] =   qb[hb]
  
  sums[3] =  qc[hc] 
  median(sums)
}

D = matrix(byrow = TRUE,nrow = 3,ncol = 3,c(1,2,3,1,2,4))



mat <- matrix(byrow = T,nrow = 20,ncol= 3,c(28,145,0,
                                            25,125,1,
                                            65,140,0,
                                            50,130,0,
                                            38,115,0,
                                            55,118,0,
                                            44,105,2,
                                            29,97,3,
                                            50,90,0,
                                            63,88,0,
                                            43,83,0,
                                            35,63,4,
                                           
                                            42,57,9,
                                            50,60,0,
                                            55,63,10,
                                            
                                            23,40,5,
                                            50,30,0,
                                            64,37,6,
                                            33,22,7,
                                            55,20,8))

centroides <- matrix(byrow = T,nrow = 10,ncol = 4, c(25,125,1,0,
                                                     44,105,2,0,
                                                     29,97,3,0,
                                                     35,63,4,0,
                                                     23,40,5,0,
                                                     64,37,6,0,
                                                     33,22,7,0,
                                                     55,20,8,0,
                                                     42,57,9,0,
                                                     55,63,10,0))

distance <- function( v1,v2){
  d=sqrt((v1[1]-v2[1])*(v1[1]-v2[1]) + (v1[2]-v2[2])*(v1[2]-v2[2]))
  d
}
calculateCentroides <- function(k,mat,centroides){
  row = nrow(mat) 
  for(c in 1: k){
    centroides[c,1] =0;
    centroides[c,2]  =0
      for(i in 1:row ){
        if( mat[i,3] == c){
            centroides[c,1] = (centroides[c,1]+mat[i,1])
            centroides[c,2] = (centroides[c,2]+mat[i,2])
          }
      }
      centroides[c,1] = centroides[c,1] /(centroides[c,4])
      centroides[c,2] = centroides[c,2] /(centroides[c,4])
  }
  centroides
}
kmean1 <-function(k,mat, centroides){
  col=ncol(mat) 
  newc=FALSE
  row = nrow(mat) 
      for(i in 1:row ){
        d0 = 10000000000
        c0 = 0
        for(c in 1:k){
          d = distance(as.vector(mat[i,]),as.vector(centroides[c,] )) 
          
          if(d < d0){
            d0 = d
            c0= c
            newc = TRUE
            
          }
        }
        if(newc){
          print(i)
          print(c0)
          print(centroides[c0,4])
          centroides[c0,4] = centroides[c0,4]+1
          print("==============")
       mat[i,3]=c0
       newc=FALSE
        }
      }
 centroides = calculateCentroides(k,mat,centroides)
 #print(centroides)
  list(mat=mat,centroides=centroides)
  } 




A  = c(.10 ,	.015 	,.010 ,	.005 ,	1 ,0, 1)
B = c( .09 	,.016 	,.012 ,	.006, 	2 ,0 ,1) 
C = c( .08 	,.017 	,.014 ,	.007 ,	3, 0 ,1)
D = c( .07 ,	.018 	,.015 ,	.008 ,	4 ,0 ,1)
E = c( .06 ,	.019 	,.016 ,	.010 ,	5, 0 ,1)


getAds  <- function(){
  
  ads<-  runif(n = 101,min = 0,max = 3)
  
  for(i in 1:101){
    
    if(ads[i]<= 1){
        ads[i] =1
      }else
        if( ads[i]<=2){
          ads[i]  =2
      }else if(ads[i]<= 3){
          ads[i] = 3
      }
  }
  ads
}

getAds2 <-function(){
  j=1
  for(i in 1:101){
    ads[i] = j
    j=j+1
    if(j==4){
      j=1
    }
  }
  ads
}
updateStates <-function(l,A,B,C,round){
  if(l==1){
    A[6] = A[6]+1
    A[5] = A[5]-0.1
    A[7] = 0
    if(A[5] <= 0){
      A[1]=0
    }
  }
  if(l==2){
    B[6] = B[6]+1
    B[5] = B[5]-0.1
    B[7] = 0
    if(B[5] <= 0){
      B[1]=0
    }
  }
  if(l==3){
    C[6] = C[6]+1
    C[5] = C[5]-0.1
    C[7] = 0
    if(C[5] <= 0){
      C[1]=0
    }
  }
  if(round == 3){
    A[7] = 1
    B[7] = 1
    C[7] = 1
  }
states =  list(A=A,B=B,C=C)
return(states)
}

 balance <-function(){
 #ads <-  getAds()
 ads <-  getAds2()
 round=1
 #states= list()
 for(i in 1:101){
  
      esp = c(0,0,0)
      esp[1] = A[1] * A[ads[i]] * A[7]
      esp[2] = B[1] * B[ads[i]] * B[7] 
      esp[3] = C[1] * C[ads[i]] * C[7] 
      maxesp = max(esp)
      print(maxesp)
      if(maxesp > 0){
            for(l in 1:3){
                if(esp[l] == maxesp ){
                maxBider = l
                break
              }
            }
           states1 =updateStates(maxBider,A,B,C,round)
           print(i)
           
        }else{
        #end
          break
        }
    round= round+1;
    if(round == 4){
      round = 1
    }
 
    A = states1$A
    B = states1$B
    C = states1$C
  if(A[5]*B[5]*C[5]==0){
    break
  }
  
 }
states1
 }
prodscalaire <-function(a,b) {
  d=0
  for(i in 1:length(a)){
    d=d+a[i]*b[i]
  }
  d
}
distance <- function( v1,v2){
  d=0
  for(i in 1:length(v1)){
  d = d+(v1[i]-v2[i])^2  
  }
  sqrt(d)
}


a=c(1,1)
b=c(2,2)
c=c(3,4)


l1_norm_distance <- function(v1,v2){
  d=0
  for(i in 1:length(v1)){
    d = d+(v1[i]-v2[i])  
  }
  d
}

# library(MASS)
# 
# a <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
#               0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), 9, 4)
# 
# a.svd <- svd(a)
# a.svd$d



n1 <- n2 <- 9
x1 <- -3  ##treated
x2 <- 1  ##placebo
s1 <- 1.5  ##treated
s2 <- 1.8  ##placebo
s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))
(x1 - x2) + c(-1, 1) * qt(0.975, n1 + n2 - 2) * s * sqrt(1/n1 + 1/n2)

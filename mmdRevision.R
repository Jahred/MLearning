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


distance <- function( v1,v2){
  d=0
  for(i in 1:length(v1)){
    d = d+(v1[i]-v2[i])^2  
  }
  sqrt(d)
}
prodscalaire <-function(a,b) {
  d=0
  for(i in 1:length(a)){
    d=d+a[i]*b[i]
  }
  d
}

l1_norm_distance <- function(v1,v2){
  d=0
  for(i in 1:length(v1)){
    d = d+(v1[i]-v2[i])  
  }
  d
}

cosine_distance <- function(v1,v2){
  dot= prodscalaire(v1,v2) 
  norm1=distance(v1,c(rep(0,length(v1))))
  norm2=distance(v1,c(rep(0,length(v2))))
  cosi= dot/(norm1*norm2)
  print("============")
  print(cosi)
  print("============")
  
#   cosi =180 *  acos(cosi ) /pi
#   print("Degrees")
  cosi
}
distanceNorm <- function( v1){
  d=0
  for(i in 1:length(v1)){
    d = d+(v1[i])^2  
  }
  sqrt(d)
}
 
cosine_distanceVect <- function(v1,v2){
  dot= prodscalaire(v1,v2) 
  norm1=distanceNorm(v1)
  norm2=distanceNorm(v2)
  cosi= dot/(norm1*norm2)
  print("============")
  print(cosi)
  print("============")
  
  #   cosi =180 *  acos(cosi ) /pi
  #   print("Degrees")
  cosi
}
v1= c(2,1,1)
v2=c(10,-7,1)


minhashf <-function( A ){
  nrows = nrow(A)
  ncols = ncol(A)
  hashes = list(h1=function(x) x %% 5,h2=function(x) 2*x %% 5)
  numHash = length(hashes)
  print(numHash)
  hashValuesOfRowNumbers= c()
  minhash = matrix(byrow = TRUE, nrow = numHash,ncol = ncols,c(rep(1000,numHash*ncols)))
  for(row in 1:nrows){
    for(hashFunctionNr in 1:numHash){
      hashValuesOfRowNumbers[hashFunctionNr] = hashes[[hashFunctionNr]](row)
    }
    for(col in 1 : ncols){
      if(A[row,col] == 1){
        for( h in 1: numHash){
          if(hashValuesOfRowNumbers[h] < minhash[h,col]){
            minhash[h,col] = hashValuesOfRowNumbers[h] 
            
          }
        }
      }
    }
  }
  minhash
}

matrx = matrix(byrow = TRUE,nrow = 5,ncol = 2,c(1,0,
                                                0,1,1,1,1,0,0,1))

V  = matrix(byrow = TRUE,nrow = 2,ncol = 5,c(-.57,-.11,-.57,-.11,-.57,-.09,.7,-.09,.7,-09))
            q=c(5,0,0,0,0)
            qu = q*t(V)

q3=c(0,0,0,0,4)
qu3=q3*t(V)

q2=c(0,5,0,0,0)
 
qu2=q2*t(V)



fu = function(x,m){
  x*(1-3.17^(-100+m))
}
fu(1,20)
fu(2,40)
fu(3,60)
fu(4,80)

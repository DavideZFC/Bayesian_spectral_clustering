##################
# COMPUTES ALL THE ELEMENTS NEEDED TO RUN OUR BAYESIAN MODELS
##################


# returns affinity matrix (knn method)
make.affinity <- function(X, n.neighboors) {
  S=as.matrix(dist(X))
  N <- length(S[,1])
  
  A <- matrix(rep(0,N^2), ncol=N)
  for(i in 1:N) { # for each line
    # only connect to those points with larger similarity 
    
    or=order(S[i,],decreasing = F)
    for(j in or[1:n.neighboors]){
      A[i,j]=1
      A[j,i]=1
    }
  }
  return(A)  
}


enlargment<-function(lambda){
  n=length(lambda)
  ret=numeric(n)
  for(i in 1:n){
    if(i>(n-2)){
      ret[i]=1
    }
    else{
      ret[i]=1+(lambda[i]-lambda[n-1])/(lambda[n-1])
    }
  }
  return(ret)
}




##################
# the X variable comes from the file "AGitHub_Benchmarks"
# X<-

neig=8
M=10

n=dim(X)[1]
I = diag(M)

A <- make.affinity(X, neig)

D <- diag(apply(A, 1, sum))
U <- D - A
L=U

for(i in 1:n){
  L[i,]=(U[i,])/D[i,i]
}

eigenk = eigen(L)
l = eigenk$vectors[,(n-M+1):n]
l_norm = scale(l) 

covg=enlargment(eigenk$values[(n-M+1):n])
I0=diag(covg^(2))
I0

mu = matrix(rnorm(M^2), nrow = M)
alpha=rep(0.01,M)

###
# Final result, ready for a Jags model
data = list(y = l_norm, mu_p = mu, I = I, M = M, n = n, alpha=alpha, I0=I0)

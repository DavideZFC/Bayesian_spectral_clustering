


###########################
# PERFORMS BAYESIAN SPECTRAL CLUSTERING
###########################


# the X variable comes from the file "AGitHub_Benchmarks"
# X<-


M = 2 #number of clusters

I = diag(M)


s <- function(x1, x2, alpha=1) {
  exp(- alpha * norm(as.matrix(x1-x2), type="F"))
}

make.similarity <- function(X, similarity) {
  N <- nrow(X)
  S <- matrix(rep(NA,N^2), ncol=N)
  for(i in 1:N) {
    for(j in 1:N) {
      S[i,j] <- similarity(X[i,], X[j,])
    }
  }
  S
}

S <- make.similarity(X, s)

make.affinity <- function(S, n.neighboors) {
  N <- length(S[,1])
  
  if (n.neighboors >= N) {  # fully connected
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  A  
}

A <- make.affinity(S, 8)
D <- diag(apply(A, 1, sum))
U <- D - A
L <- solve(D)%*%U




eigenk_2 = eigen(L)
l = eigenk_2$vectors[,(n+1-M):n]
eigvalues2 = scale(eigenk_2$values[(n+1-M):n])
l_norm = scale(l) 
pairs(l_norm)

#data for jags
mu = matrix(rnorm(M^2), nrow = M)


data = list(y = l_norm, mu_p = mu, I = I, M = M, n = n)

inits = function() {list(omega = rep(1/M,M)) }
require(rjags)
model_Cluster=jags.model("Model2.txt",data=data,inits = inits, n.adapt=2000,n.chains=1)
variable.names=c("z")

output_CLuster=coda.samples(model=model_Cluster,variable.names=variable.names,n.iter=2000,thin=10)



data.out=as.matrix(output_CLuster) 
data.out=data.frame(data.out)     

n.chain=dim(data.out)[1] 
summary(data.out)


z.out = as.numeric(data.out[n.chain,])


pairs(l_norm, col = z.out, pch = 19, labels = c('L1','L2','L3'), font.labels = 3)
plot(data0, col = z.out, pch = 19, xlab = "X", ylab = "Y", asp = 1, lwd = 2.5)

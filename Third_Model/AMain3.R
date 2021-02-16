

###########################
# PERFORMS BAYESIAN SPECTRAL CLUSTERING
###########################


# the X variable comes from the file "AGitHub_Benchmarks"
# X<-

# from the file "AData_preprocessing.R" we take data
# data<- 


inits = function() {list(omega = rep(1/M,M)) }
require(rjags)

model_Cluster=jags.model("Model3.txt",data=data,inits = inits, n.adapt=2000,n.chains=1)
variable.names=c("z")

output_CLuster=coda.samples(model=model_Cluster,variable.names=variable.names,n.iter=1000,thin=10)

data.out=as.matrix(output_CLuster) 
data.out=data.frame(data.out)     
n.chain=dim(data.out)[1] 



z.out = as.numeric(data.out[n.chain,])
# we take as output cluster the last value of the z in the markov chain


plot(X, col = z.out, pch = 19, xlab = "X", ylab = "Y",  lwd = 2.5)

plot(l_norm[,1],l_norm[,2],col=z.out)

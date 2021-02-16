
##############################
# Upload the benchmark datasets
# and try a trivial clusterization with k-means
##############################

# Hp1
# X<-read.csv('ensemble0.csv')

# Hp2
# X<-read.csv('ensemble1.csv')

# Hp3 (noisy)
X<-read.csv('ensemble1.csv')
X=X+rnorm(dim(X)[1],0,0.05)

# Hp4 (three clusters)
# X<-read.table('tri_cluster.txt')

# Hp 5 (four clusters)
# X<-read.table('quadri_cluster.txt')

# Hp6
# X<-read.table('quadri_cluster2.txt')

# Hp7
# X<-read.table('quadri_noise.txt')


library(MASS)
q<-kmeans(X,2)
co=2*(q$cluster)-1

plot(X[,1],X[,2],xlab='X',ylab='Y',pch=18,col=co+1)

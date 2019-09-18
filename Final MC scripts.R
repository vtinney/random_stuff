# Created 9-17-2019
# Create sample data from the central tendancy estimates provided
# in the Probabilistic worksheet

# N0 with a log-normal distribution
m <- 33
s <- 221
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))
print(paste("location:", location))
print(paste("shape:", shape))
n0 <- rlnorm(n=100, location, shape)
summary(n0)

# r with a log-normal distribution
m <- -0.0467
s <- 0.005
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))
print(paste("location:", location))
print(paste("shape:", shape))
r <- rlnorm(n=100, location, shape)
summary(r)

# p0, f.surv, p.med, p.cons, all with normal distributions
p0 <-rnorm(n=100,mean=0.001,sd=0.0005)
f.surv <- rnorm(n=100,mean=0.27,sd=0.05)
p.med <- rnorm(n=100,mean=0.342,sd=0.11)
p.cons <- rnorm(n=100,mean=0.0141,sd=0.0065)


X = matrix(ncol = 12,nrow = 100000)
for(i in 1:100000){ 
  #First randomly sample the input parameters
  X[i,1] = sample(n0,1)
  X[i,2] = sample(p0,1)
  X[i,3] = sample(f.surv,1)
  X[i,4]= sample(p.med,1)
  X[i,5]= sample(r,1)
  X[i,6]= sample(p.cons,1)
  
  X[i,7] <- X[i,1]*X[i,3] # calculate nC
  X[i,8] <- X[i,2]*X[i,4] # calculate pC
  X[i,9] <- (1-((1-X[i,5])^X[i,7]))*X[i,8] # calculate p.inf
  X[i,10] <-  X[i,6]*365*100000 #calculate A
  X[i,11] <- X[i,9]*X[i,10] # calculate infection risk per 100,000
  X[i,12] <- mean(X[1:i,1])
}


df <- as.data.frame(X) #export from matrix to a dataframe 
colnames(df) <- c('n0','p0','f.surv','p.med','r','p.cons','nC','pC','p.inf','a','inf') #label column names

# Summary statistics
print(summary(df$inf))
print(quantile(df$inf, probs=c(0.05,0.95)))

par(mfrow=c(1,2))
plot(X[,12], main='Probabilistic model',ylab='Rolling avg')

#===========================================================================
# Create sample data from the central tendancy estimates provided
# in the Deterministc worksheet

n0 = rnorm(n=100, 22, 1)
p0 = rnorm(n=100,mean=0.001,sd=0.0005)
f.surv = rnorm(n=100,mean=0.27,sd=0.05)
p.med = rnorm(n=100,mean=0.342,sd=0.11)
r = rnorm(n=100, 0.0094, 0.005)
p.cons = rnorm(n=100,mean=0.0141,sd=0.0065)

X = matrix(ncol = 12,nrow = 100000)
for(i in 1:100000){ 
  #First randomly sample the input parameters
  X[i,1] = sample(n0,1)
  X[i,2] = sample(p0,1)
  X[i,3] = sample(f.surv,1)
  X[i,4]= sample(p.med,1)
  X[i,5]= sample(r,1)
  X[i,6]= sample(p.cons,1)
  
  X[i,7] <- X[i,1]*X[i,3] # calculate nC
  X[i,8] <- X[i,2]*X[i,4] # calculate pC
  X[i,9] <- X[i,5]*X[i,7]*X[i,8] # calculate p.inf
  X[i,10] <-  X[i,6]*365*100000 #calculate A
  X[i,11] <- X[i,9]*X[i,10] # calculate infection risk per 100,000
  X[i,12] <- mean(X[1:i,1])
}

df <- as.data.frame(X) #export from matrix to a dataframe 
colnames(df) <- c('n0','p0','f.surv','p.med','r','p.cons','nC','pC','p.inf','a','inf') #label column names

# Summary statistics
print(summary(df$inf))
print(quantile(df$inf, probs=c(0.05,0.95)))

plot(X[,12], main='Probabilistic model \nwith mean parameters',ylab='Rolling avg')



# Sample histogram showing normal and log-normal distribution of n0
n1 <- rnorm(n=100, mean=33, sd=221)
par(mfrow=c(1,2))
hist(n1,main='Normal',col='lightblue',xlab='n0')
hist(n0, main='Log-normal',col='lightgreen',xlab='n0')


trueA <- 10
trueB <- 0
trueSd <- 8
sampleSize <- 31

x <- (-(sampleSize-1)/2):((sampleSize-1)/2)

y <- trueA*x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

plot(x,y)

likelihood <- function(param){
	a <- param[1]
	b <- param[2]
	sd <- param[3]

	pred <- a*x + b
	singlelikelihoods <- dnorm(y, mean = pred, sd = sd, log = T)
	sumll <- sum(singlelikelihoods)
	return(sumll)
}

slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
slopelikelihoods <- lapply(seq(2,18,by=.05), slopevalues)
plot(seq(2,18,by=.05),slopelikelihoods,type='l',xlab='values of slope parameter a')


prior <- function(param){
	a <- param[1]
	b <- param[2]
	sd <- param[3]

	aprior <- dunif(a,min=0,max=15,log=TRUE)
	bprior <- dnorm(b,sd=5,log=TRUE)
	sdprior <- dunif(sd,min=0,max=30,log=TRUE)
	return(aprior+bprior+sdprior)
}

posterior <- function(param){return(likelihood(param)+prior(param))}

proposalfunction <- function(param){
	return(rnorm(3,mean=param,sd=c(.1,.5,.3)))
}

run.Metropolis.MCMC <- function(startvalue,iterations){
	chain <- array(dim=c((iterations+1),3))
	chain[1,] <- startvalue
	
	for(i in 1:iterations){
		proposal <- proposalfunction(chain[i,])
		probab <- exp(posterior(proposal) - posterior(chain[i,]))
		if(runif(1) < probab){
			chain[i+1,] <- proposal
		} else {
			chain[i+1,] <- chain[i,]
		}
	}
	return(chain)
}

startvalue <- c(4,0,10)

chain <- run.Metropolis.MCMC(startvalue,20000)	
burnIn <- 5000
acceptance <- 1-mean(duplicated(chain[-(1:burnIn),]))

post <- chain[-(1:burnIn),]	

par(mfrow = c(2,3))
hist(post[,1],main = 'A',nclass=30)
abline(v=trueA,col='red')
hist(post[,2],main = 'B',nclass=30)
abline(v=trueB,col='red')
hist(post[,3],main = 'Sd',nclass=30)
abline(v=trueSd,col='red')
plot(post[,1],type='l')
abline(h=trueA,col='red')
plot(post[,2],type='l')
abline(h=trueB,col='red')
plot(post[,3],type='l')
abline(h=trueSd,col='red')

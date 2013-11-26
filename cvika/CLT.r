vals=c(-3,-1,3,20)
probs=c(3,7,8,13)
avg_rv <- function(n) as.real((t( rmultinom(1,size=n, prob=probs) ) %*% vals)/n)
mean_value=(probs/sum(probs)) %*% vals
variance=((vals-mean_value)^2) %*% (probs/sum(probs))


fn=ecdf(replicate(10000,(avg_rv(3000)-mean_value)/sqrt(variance)*sqrt(3000)))
n=1000
p=0.01
mu=p
sigma2=p*(1-p)/n

N=10000000
a=mean(rbinom(N, n , p)/n)
b=mean(rnorm(N, mu, sqrt(sigma2)))
b-a

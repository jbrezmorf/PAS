p=0.2
n=20
x=(0:n)
plot(x,dbinom(x,n,p), type="l", col="red")
lines(x,dnorm(x,n*p, sqrt(n*p*(1-p))) ,col="blue")
     
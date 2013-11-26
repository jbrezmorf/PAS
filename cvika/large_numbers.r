p_param=0.5
mu_par=-3.0
var_par=100000.0

avg_fn=function(n) ( rbinom(1,n,p_param))/n - p_param 

avg_norm=function(n) {
  vec=rnorm(n,mu_par,var_par)
  return( sum(vec)/n - mu_par )
} 

clt_fn=function(n)  {
  avg=sum(runif(n))/n
  sigma=sqrt(1/12)
  return (sqrt(n)*(avg - 0.5)/sigma)
}  

n_vec=round(1.1^(2:100))
n_vec=rep(4, 10000)
avg_vec=sapply(n_vec, clt_fn)
#hist(avg_vec, breaks=50,freq=FALSE)
x=seq(-4,4,0.01)
y=dnorm(x)
#lines(x,y, col="red")
qqnorm(avg_vec)


#plot(n_vec,avg_vec, type='l', log='x')




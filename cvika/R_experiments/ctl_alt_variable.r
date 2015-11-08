max_n=10000
p=0.5
n_values=100

sigma=p*(1-p)
fun=function(n) ( rbinom(1,n,p)/n -p)

#a=log(10)
#b=log(max_n)
#vec=round(exp( seq(a,b,length.out=n_values) ))
vec=round(1.1^(2:n_values))
#vec=rep(max_n, n_values)

avg=sapply(vec,fun)
clt=sqrt(vec)*avg
#sd(clt)
plot(vec,avg,type='l',log='x')
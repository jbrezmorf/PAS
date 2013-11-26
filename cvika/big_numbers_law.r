average=function(n) (as.real(rbinom(1,n,0.47))/n-0.47)
n_vec=round(1.01^(2:1000))
avg_vec=sapply( n_vec, average )
plot( n_vec, avg_vec , type='l', log="x")
N=10
lambda=0.014

breaks=c(0,25,50,75,100)
# probabilities for indivudual bins
# the last one is tail
bin_probs=c(pexp(tail(breaks, n=-1), lambda), 1.0) - pexp(breaks, lambda)
bin_mids=c( (head(breaks, n=-1) + tail(breaks, n=-1))/2, tail(breaks, n=1))

# max likely hood for tabled data:
# \d_{\lambda} \sum (X_i - (F(B_i) - F(A_i))*N )^2=
# \d_{\lambda} (X_i - (exp(-\lambda*A_i) - exp(-\lambda*B_i))*N)^2=
#  \sum 2*(X_i - (exp(-\lambda*A_i) - exp(-\lambda*B_i))*N)
#        *N*(A_i exp(-\lambda*A_i) - B_i exp(-\lambda*B_i)
#
#  0=\sum (X_i - (exp(-\lambda*A_i) - exp(-\lambda*B_i))*N)
#        *(A_i exp(-\lambda*A_i) - B_i exp(-\lambda*B_i)


# small positive bias, seems to be consistent
estimate_moc=function(data) {
  return ( sum(head(data,-1))/sum(data*bin_mids))
}

# large positive bias; not consistent
estimate_avg=function(data) {
  return ( sum(data)/sum(data*bin_mids))
}

# even larger positive bias; not consistent
estimate_avg1=function(data) {
  return ( sum(head(data,-1))/sum(head(data,-1)*head(bin_mids,-1)) )
}

estimate=estimate_avg1

data=rmultinom(1, 10, bin_probs)
estimate(data[,1])

data=rmultinom(1, 10000, bin_probs)
estimate(data[,1])

data=rmultinom(10000, 10, bin_probs)
mean(apply(data, 2, estimate))

gen=function(x) {
  vec=rmultinom(1,1,c(1,1,1))
  # random
  x=rbinom(1,1,0.5)
  i=((1:3)[vec==0])[x]
  vec[i]=2
  return(vec)
}
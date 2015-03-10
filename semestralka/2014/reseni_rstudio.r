par(ask = TRUE)
require(ggplot2)

#=======================================================================
#  1)
#=======================================================================

n=4
# Pravdepodobnost padnuti alespon jedne sestky v n hodech.
p=1-(5/6)^n
# n*log(5/6) == log(1-p)
p=0.9

log(1-p)/log(5.0/6.0)

# Alternativne lze pouzit geometricke rozdeleni (cekani na prvni sestku vcetne)

# In R quantiles gives number of failures
qgeom(p, 1/6) +1

# Second case
p=0.99
log(1-p)/log(5.0/6.0)
qgeom(p, 1/6) +1

# Final question
(5/6)^4*1/6
dgeom(4,1/6)

#=======================================================================
#  2)
#=======================================================================
data = c(932, 1130, 974, 924, 1234, 1246, 852, 841, 1097, 1056, 1006, 796, 1018,
         973,  1091, 769, 1022, 952, 832, 962, 1074, 1042, 1071, 1215, 904, 915,
         963,  1095, 1099, 1014, 878, 933, 1116, 1008, 945, 1166, 720, 1137, 970,
         857,  879, 1068, 1047, 978, 922, 372, 897, 1153, 1366, 1052)

summary(data)

data_mean=mean(data)

std_dev=sd(data)
std_dev

var(data)
quartiles = quantile(data, c(0.25, 0.75), names=FALSE )
iqr = quartiles[2] - quartiles[1]
iqr

index=1:length(data)
# Odlehla pozorovani by IQR
# two values
bool_iqr=data<quartiles[1]-1.5*iqr | data > quartiles[2]+1.5*iqr
index_iqr=index[bool_iqr]
data_iqr=data[!bool_iqr]
index_iqr

# by 3 sigma rule
# one value
bool_3sig=abs(((data-data_mean)/std_dev)) > 3
index_3sig=index[bool_3sig]
data_3sig=data[!bool_3sig]
index_3sig

# by MAD
# no value
1.4826*median(abs(data-median(data)))
mad(data)
bool_mad=(data-median(data))/(mad(data)) > 3
index_mad=index[bool_mad]
data_mad=data[!bool_mad]
index_mad

hist(data_iqr)
boxplot(data_iqr)
plot(ecdf(data_iqr))

qqnorm(data)
ecdf(data_iqr)(750)
pnorm(q=750,mean=mean(data_iqr), sd=sd(data_iqr))
pnorm(q=750,mean=mean(data), sd=sd(data))

#=======================================================================
#  3)
#=======================================================================
load=c( 100, 110, 120, 210, 220, 230, 310, 320, 330)
A= c(12.36, 13.60, 14.79, 25.92, 27.29, 28.19, 37.92, 39.95, 39.92)
B= c(12.19, 13.44, 14.60, 25.53, 26.36, 27.94, 37.75, 38.61, 40.60)
plot(abs(A-B)/load)
lines(abs(A-B))
t.test((A-B)/load, alternative="greater")
t.test(A-B)
t.test(A/load,B/load)

#=======================================================================
#  4)
#=======================================================================
n_spoj=c(0, 1,  2,  3, 4, 5, 6)
n_int=c(41,47, 36, 18, 5, 2, 1)
N=sum(n_int)

# recreate original values
values=rep(n_spoj, n_int)
values

# consider Poison distribution with
lambda=mean(values)
lambda

# Chi square
prob=dpois(n_spoj, lambda=lambda)
prob[7]=1-ppois(5, lambda=lambda)
prob
chisq.test(n_int, p=prob)

n_int
np_values=sum(n_int)*prob
np_values
T=sum((n_int-np_values)^2/np_values)
T
# p_val
1-pchisq(T, df=6)
1-pchisq(T, df=7-2)

# Kolmogorov?

# Expected frequ.
frequ.exp <- dpois(0:6, lambda=lambda)
# Construct numeric vector of data values (y = vFrequ for Kolmogorov-Smirnov Tests) 
vFrequ <- c()
for(i in 1:length(frequ.exp)) {
  vFrequ <- append(vFrequ, rep(i-1, times=N*frequ.exp[i]))
}


# First Kolmogorov-Smirnov Tests fit
ks.test(values, vFrequ)

# ks.test(values, "dpois", lambda=lambda)
# On sample test do not work probably due to violation of continuity assumption.
#=======================================================================
#  5)
#=======================================================================
N=7
mean_vec=c(47.167,15.667,31.5,14.833)
sd_vec=c(6.795,3.327,9.915,5.345)
n_group=length(mean_vec)

v_data=c()
for(i in 1:4) {
  v_data <- append(v_data, rnorm(N,mean_vec[i],sd_vec[i]))
}


v_fac=c(rep(1,7),rep(2,7),rep(3,7),rep(4,7))
df=data.frame(v_data, factor(v_fac))
names(df)=c("dat","fac")
model=aov(dat~fac,data=df)
summary(model)
TukeyHSD(model)

# ANOVA manual
SS_residual=sum(sd_vec^2*(N-1))
SS_residual
SS_group=N*sum((mean_vec-mean(mean_vec))^2)
SS_group
# total
SS_residual+SS_group


dof_residual=N*4-n_group
MS_residual=SS_residual/dof_residual
MS_residual

dof_group=n_group-1
MS_group=SS_group/dof_group
MS_group

F=MS_group/MS_residual
F

pp=pf(F, dof_group,dof_residual)
p_val=1-pp
p_val


for(i in 1:(n_group-1)) {
  for(j in (i+1):n_group) {
    U=abs(mean_vec[i]-mean_vec[j])/sqrt(MS_residual*2/N)
    p_val=ptukey(sqrt(2)*U, nmeans=n_group, df=dof_residual)
    p_val_tukey=1-p_val
    
    # Scheffe's test
    p_val=pf(U^2/dof_group, dof_group,dof_residual)
    p_val_scheffe=1-p_val
    
    # LSD T-test
    p_val=pt(U/dof_group, df=dof_residual)
    p_val_LSD=2*min(p_val,1-p_val)
    
    print(round(c(i,j, abs(mean_vec[i]-mean_vec[j]), p_val_tukey, p_val_scheffe, p_val_LSD), digits=7))
    
  }
}

insect=c()
group=c()
####### data reconstruction
for(i in 1:(n_group)) {
    rr=rnorm(N)    
    insect=c(insect,(rr-mean(rr))/sd(rr)*sd_vec[i]+mean_vec[i])
    group=c(group, rep(i,N))
}
group=factor(group)
df=data.frame(insect, group)

anova_model=aov(insect~group,data=df)
summary(anova_model)
TukeyHSD(anova_model)
par(ask = TRUE)
require(ggplot2)

#=======================================================================
#  1)
#=======================================================================



#=======================================================================
#  2)
#=======================================================================
auta = c(4, 6, 3, 3, 6, 4, 9, 7, 5, 4, 2, 7, 5, 3, 4, 4, 7, 4, 1, 6, 4, 5, 8, 3, 5,
7, 4, 8, 5, 7, 2, 7, 3, 4, 3, 3, 3, 3, 5, 8, 4, 2, 9, 5, 1, 10, 8, 0, 3, 4,
5, 4, 5, 4, 5, 3, 5, 6, 3, 2, 11, 10, 4, 5, 4, 5, 9, 3, 5, 7, 3, 3, 2, 7, 3,
7, 3, 4, 5, 5, 3, 1, 5, 2, 7, 4, 3, 7, 3, 6, 5, 3, 2, 4, 2, 4, 6, 3, 6, 3)

summary(auta)
sd(auta)
var(auta)
quartiles = quantile(auta, c(0.25, 0.75), names=FALSE )
iqr = quartiles[2] - quartiles[1]
iqr

table(auta)

 
hist(auta)


qqplot(qpois(ppoints(length(auta)), lambda=mean(auta)), auta)


# alternative hypothesis is median > 4
# P-value test
wilcox.test(auta, mu=4, alternative="greater")
#  we reject null hypothesis; alternative holds 
# however symmetry doesn't hold !!

n_minus=length(auta[auta<4])
n_minus                     
n_plus=length(auta[auta>4])
n_plus                       
pbinom(n_plus, n_plus+n_minus, 0.5)
pvalue=1-pbinom(n_plus, n_plus+n_minus, 0.5)
pvalue
# not assuming symmetry we can not reject

# =======================
# normalne ma wilcox vyssi silu:
#   
#> xx=rnorm(100)
#> wilcox.test(xx)
# V = 2432, p-value = 0.7505
#> n_plus=length(xx[xx>0])
#> 2*(1-pbinom(n_plus, 100, 0.5))
# [1] 1.382701
#> 2*(pbinom(n_plus, 100, 0.5))
# pvalue of sign test: [1] 0.6172994

#=======================================================================
#  3)
#=======================================================================
n1=1600; a1=3.288; s1=0.649; n2=900; a2=3.314; s2=0.592
sx=sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2)/ (n1+n2-2) )
T=(a2-a1)/sx/sqrt(1/n1+1/n2)
T
sx
pv=pt(T, df=n1+n2-2)
pv
# alternative is a2-a1>0
pvalue=1-pv
pvalue
# can not reject null hypothesis (neporkazali jsme ucinnost flourovani)
#=======================================================================
#  4)
#=======================================================================
freq=c(45, 26, 25, 27, 22, 32, 21, 26, 40, 34)
chisq.test(freq, p=rep(1/10,10))

# recreate original values
values=rep(1:10, freq)
values

mean(values)
var(values)

# chi square rucne

# expected frequency (same for all values)
expected=sum(freq)/10
chi=sum( (freq - expected)^2/expected)
chi
1-pchisq(chi, df=9)

# same as R-implementation
# we reject hypothesis about distribution

#=======================================================================
#  5)
#=======================================================================
v1=c(243, 251, 275, 291, 347, 354, 380, 392, 309)
v2=c(206, 210, 226, 249, 255, 273, 285, 295)
v3=c(241, 258, 270, 293, 328)

# create data frame
values=c(v1,v2,v3)
sizes=c(length(v1),length(v2),length(v3))
factors=factor( rep(1:3,sizes) )
dframe=data.frame( values, fac=factors )
#
# simplier
# dframe=stack(list(v1=v1,v2=v2,v3=v3))



# scatter plot
ggplot(dframe, aes(fac,values))+geom_point()
plot(as.list(dframe$fac), dframe$values)



# box plot
boxplot(values~fac,dframe)


# general linear model
model=aov(values~fac,dframe)
summary(model)
# Rejecting null hypothesis

# posthoc analysis using Tukey
TukeyHSD(model)

# significant difference in 1-2 pair


# posthoc analysis using pairwise T test with compensation
pairwise.t.test(dframe$val, dframe$fac, p.adjust.method="b")
pairwise.t.test(dframe$val, dframe$fac, p.adjust.method="holm")

# same result as Tukey, for 1-3 and 2-3 have larger error of second kind (since Tukey should be exact)




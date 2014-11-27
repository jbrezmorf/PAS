# Tenzometr
load=c(100, 110, 120, 210, 220, 230, 310, 320, 330)

new=round(0.123*load*rnorm(length(load), 1.0, 0.01), digits=2)
new
old=round(0.123*load*rnorm(length(load), 0.985, 0.01), digits=2)
old

t.test(new-old, alternative="greater")

#m=lm(old-new ~ 0+load)
#summary(m)

ratio=(new-old)/load
#plot(ratio)
plot(new-old)
t.test(ratio, alternative="greater")
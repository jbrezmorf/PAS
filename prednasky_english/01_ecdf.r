plot(c(-3,3), c(0,1), type="n", xlab="value", ylab="probability")
rn10=sort(rnorm(10))
lines(ecdf(rn10) )
lines(ecdf(rnorm(100)), col="orange")
lines(ecdf(rnorm(1000)), col="blue", lwd=3)
x=seq(-3,3,0.01)
y=pnorm(x)
lines(x, y, col="red", lwd=3)
text(rn10,1:10/10 - 0.02, format(rn10,digits=2))
legend(-2.5,0.8, 
       c("10", "100", "1000", "exact"),
       lty=1, lwd=4,
       col=c("black","orange","blue","red"))

op <- par(mfrow=c(2, 2), cex=0.8)
font=list(family="Courier New", face=1)
utils::str(hist(islands, col='gray', labels = TRUE))
legend("topright",
       "utils::str(hist(islands, col='gray', labels = TRUE))", bty="n")

##-- For non-equidistant breaks, counts should NOT be graphed unscaled:
hist(islands, breaks=c(12,20,36,80,200,1000,17000), freq = TRUE,
     main = 'WRONG histogram') # and warning
legend("topright",
"hist(islands, breaks=c(12,20,36,80,200,1000,17000),\
     freq = TRUE, main = 'WRONG histogram')", bty="n")


r<-hist(sqrt(islands), breaks = 12, col="lightblue", border="pink")
lines(r, lty = 1, lwd=3,border = 'red') # -> lines.histogram(*)
legend("topright",
"r<-hist(sqrt(islands), breaks = 12, col=\"lightblue\", border=\"pink\")\
lines(r, lty = 1, lwd=3,border = 'red')", bty="n")


r <- hist(sqrt(islands), breaks = c(4*0:5, 10*3:5, 70, 100, 140),
          col='blue1', labels=T)
text(r$mids, r$density, r$counts, adj=c(.5, -.5), col='blue3')
sapply(r[2:3], sum)
sum(r$density * diff(r$breaks)) # == 1
legend("topright", 
"r = hist(sqrt(islands), breaks = c(4*0:5, 10*3:5, 70, 100, 140),\
          col='blue1', labels=T)\
text(r$mids, r$density, r$counts, adj=c(.5, -.5), col='blue3')\
\
> sapply(r[2:3], sum)\
     counts intensities\ 
  48.000000    0.215625\
\
> sum(r$density * diff(r$breaks))\
[1] 1", bty="n")


par(op)

#require(utils) # for str
#str(hist(islands, breaks=12, plot= FALSE)) #-> 10 (~= 12) breaks
#str(hist(islands, breaks=c(12,20,36,80,200,1000,17000), plot = FALSE))



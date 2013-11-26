op <- par(mfrow=c(2, 2), xpd=TRUE)

utils::str(hist(islands, col='gray', labels = TRUE))
legend("topright",
"utils::str(hist(islands, col='gray', labels = TRUE))", bty='n')       
       

##-- For non-equidistant breaks, counts should NOT be graphed unscaled:
hist(islands, breaks=c(12,20,36,80,200,1000,17000), freq = TRUE, 
     main = 'WRONG histogram')
legend("topright",
"hist(islands, breaks=c(12,20,36,80,200,1000,17000),\
      freq = TRUE, main = 'WRONG histogram')"
       ,bty='n')

r=hist(sqrt(islands), breaks = 12, col='lightblue', border='red')
legend("topright",
"r = hist(sqrt(islands), breaks = 12,\
          col='lightblue', border='red')",bty='n')

r = hist(sqrt(islands), breaks = c(4*0:5, 10*3:5, 70, 100, 140),
          col='blue1')
legend(15,0.13,
"r = hist(sqrt(islands), col='blue1',
          breaks = c(4*0:5, 10*3:5, 70, 100, 140))\
text(r$mids, r$density, r$counts, adj=c(.5, -.5), col='blue3')
\
>sapply(r[2:3], sum)
     counts intensities \
  48.000000    0.215625 \
\
> sum(r$density * diff(r$breaks))\
[1] 1",bty='n')

text(r$mids, r$density, r$counts, adj=c(.5, -.5), col='blue3')
sapply(r[2:3], sum)
sum(r$density * diff(r$breaks)) # == 1
par(op)


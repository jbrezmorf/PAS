vec=rev(sort(colSums(unstack(InsectSprays))))
x=0:length(vec)
y=c(0,cumsum(vec))
plot(x,y, type='n',  ylab="insect frequency", xlab="type of spray", axes=FALSE)
barplot(vec,add=T, space=0, col='lightgreen', line=-1)
lines(x,y, type='o', pch=16, col='red', lwd=2)
rel_labels=paste(format(100*y/max(y),digits=2, trim=TRUE),"%")
rel_labels[1]=""
text(x,y,rel_labels, adj=c(1,-0.4), col='red')

ticks=seq(min(y), max(y), length=11)
labels=paste(format(100*ticks/max(ticks),digits=2, trim=TRUE),"%")
axis(4, at=ticks, labels=labels )

text(x[-length(x)],vec,format(vec,digits=2, trim=TRUE), adj=c(-0.6,-0.4), col='darkgreen')

######################
require(qcc)
pareto.chart(colSums(unstack(InsectSprays)), las=0)
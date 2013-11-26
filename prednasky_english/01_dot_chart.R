# Dotplot: Grouped Sorted and Colored
# Sort by mpg, group and color by cylinder
x <- mtcars
x$cyl <- factor(x$cyl) # it must be a factor

x["idx"]=1:nrow(x)
x8=x[x$cyl==8,]
x6=x[x$cyl==6,]
x4=x[x$cyl==4,]

xx=rbind(x8[order(x8$mpg),],x6[order(x6$mpg),], x4[order(x4$mpg),])
x=xx

x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
#mat=cbind( 1:nrow(x) , 1:nrow(x)+2)
#dotchart2(mat)
dotchart2(x$mpg/max(x$mpg) ,
          labels=row.names(x),cex=.7,groups= x$cyl, 
          main="Gas Milage for Car Models\ngrouped by cylinder",
          xaxis=FALSE, col=x$color, pch=19, sort=FALSE, dotsize=2,
          xlim=c(0,1))  

ticks=seq(min(x$mpg), max(x$mpg), length=11)
labels=format(ticks,digits=2, trim=TRUE)
ticks=ticks/max(x$mpg)
axis(1, at=ticks, labels=labels )

ticks=seq(min(x$hp), max(x$hp), length=11)
labels=format(ticks,digits=2, trim=TRUE)
ticks=ticks/max(x$hp)
axis(3, at=ticks, labels=labels )

dotchart2(x$hp/max(x$hp),add=TRUE,cex=.7,groups= x$cyl, pch=17, sort=FALSE, dotsize=2, col=x$color)  
#points(, row.names(x))

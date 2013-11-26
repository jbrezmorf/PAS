
dotplot(variety ~ yield | site, data = barley, groups = year,
                key = simpleKey(levels(barley$year), space = "right"),
               xlab = "Barley Yield (bushels/acre) ",
               aspect=0.5, layout = c(1,6), ylab=NULL)

par(xpd=TRUE)
bwplot(variety~yield,data=barley, aspect=1.5,
       scales=list(
         cex=1.2 ),
       xlab=list(label="Yield",cex=1.2) 
       )
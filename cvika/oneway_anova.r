boxplot(count ~ spray,data=InsectSprays)

# possibly different variance
oneway.test(count ~ spray,data=InsectSprays)
# equal variances
oneway.test(count ~ spray,data=InsectSprays, var.equal=TRUE)

aov2=aov(count ~ spray,data=InsectSprays)
summary(aov2)
TukeyHSD(aov2)
t.test(count ~ spray,data=InsectSprays, p.adjust=)


aov3=lm(count ~ spray,data=InsectSprays)
summary(aov3)

bonferroni
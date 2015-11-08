blood_1=c(243,
          251,
          275,
          291,
          347,
          354,
          380,
          392
)

blood_2=c(
  206,
  210,
  226,
  249,
  255,
  273,
  285,
  295,
  309
)

blood_3=c(
  241,
  258,
  270,
  293,
  328
)


factor=c(rep(0,length(blood_1)), rep(1,length(blood_2)), rep(2,length(blood_3)))

df=data.frame(c(blood_1, blood_2, blood_3), factor)
df.names[1]="blood"
#boxplot()

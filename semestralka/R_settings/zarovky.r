mean_life=1000
life_sd=127

data=rnorm(45, mean_life, life_sd)
data1=runif(5, mean_life-5*life_sd, mean_life+5*life_sd)

data=round(c(data, data1))
data
write.table(t(data), 
            file="/home/jb/Vyuka/PAS-Statistika/git/semestralka/R_settings/zarovky.data",
            row.names=FALSE,
            col.names=FALSE,
            sep=", ")

box=boxplot(data, notch=TRUE)
box

filter_data=data[ box$stats[1,1]<data & data<box$stats[5,1] ]
filter_data

sd(data)
mean(data)

sd(filter_data)
mean(filter_data)



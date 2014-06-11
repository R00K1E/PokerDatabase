rm(list=ls())

load("All_pokerhand_ids.RData")
games <- summary(factor(all.ids$table))
rm(all.ids)

expand <- function(x) data.frame(Var1=x[c(1,1,1,1,1,2,2,2,2,3,3,3,4,4,5)],
																 Var2=x[c(2,3,4,5,6,3,4,5,6,4,5,6,5,6,6)])

linear.tests <- function(mns, ses, games){
	pos <- c("Small Blind","Big Blind","Early","Middle","Cutoff","Button")
	data <- expand(pos)
	m1 <- expand(mns)[,1]
	m2 <- expand(mns)[,2]
	s1 <- expand(ses)[,1]
	s2 <- expand(ses)[,2]
	num1 <- games
	num2 <- games
	pooled <- ((num1-1) * s1 + (num2-1) * s2)/(num1+num2-2)
	z <- (m1-m2)/pooled
	p <- 2*pnorm(-abs(z))
	p <- p.adjust(p , method="bonferroni", n = length(m1))
	return(cbind(data.frame(data),m1,m2,z,p.value=ifelse(p < .001, "<.001",round(p,3))))
}


paired.tests <- function(mn, se, games){
	m1 <- mn[,1]
	m2 <- mn[,2]
	se1 <- se[,1]
	se2 <- se[,2]
	num1 <- games[1]
	num2 <- games[2]
	pooled <- ((num1-1) * se1 + (num2-1) * se2)/(num1+num2-2)
	z <- (m1-m2)/pooled
	p <- 2*pnorm(-abs(z))
	p <- p.adjust(p , method="bonferroni", n = nrow(mn))
	pos <- c("Small Blind","Big Blind","Early","Middle","Cutoff","Button")
	for(i in c(1:6)) cat(sprintf("%s mean1=%s, mean2=%s, z=%s, p<%s\n",pos[i],round(m1[i],4),round(m2[i],4), round(z[i],1), ifelse(p[i] < .001,".001",round(p[i],3))))
}


load(file.path("Limpers","boot_limpers_1000.RData"))

out$group <- c(rep("Big Blind",6),rep("Small Blind",6))
mn <- aggregate(cbind(ReturnRate,TotalContribution,ContPerGame,WinRate,Games,AllGames,PercentofAllGames) 
					~ Position + group, out, mean) 
se <- aggregate(cbind(ReturnRate,TotalContribution,ContPerGame,WinRate,Games,AllGames,PercentofAllGames) 
								~ Position + group, out, sd) 
head(out)

library(Hmisc)
#Percent of Games
mn1 <- mn[1:6,"PercentofAllGames"]
mn2 <- mn[7:12,"PercentofAllGames"]
se1 <- se[1:6,"PercentofAllGames"]
se2 <- se[7:12,"PercentofAllGames"]
head(out)

errbar(0:5, mn1, mn1+1.96*se1, mn1-1.96*se1, ylim=c(min(mn1-1.96*se1), max(mn2+1.96*se2)),
			 bty='l', xaxt='n',xlab="Position", ylab="Percent of Games %")
lines(0:5, mn1, lty=3)
errbar(0:5, mn2, mn2+1.96*se2, mn2-1.96*se2, add=T)
lines(0:5, mn2, lty=1)
legend("topright",c("Higher Stake Games ($5/10)","Lower Stake Games ($.5/$1)"), lwd=1, lty=2:1, bty='n')
axis(1,c(0,1,2,3,4,5),labels=c("Small Blind","Big Blind","Early","Middle","Cutoff","Button"))
title("Percent of Games")

linear.tests(cbind(mn2,mn2),cbind(se2,se2), games[2])
linear.tests(cbind(mn1,mn1),cbind(se1,se1), games[1])
paired.tests(cbind(mn2,mn1), cbind(se2,se1), games)


#Win Rate
mn1 <- mn[1:6,"WinRate"]
mn2 <- mn[7:12,"WinRate"]
se1 <- se[1:6,"WinRate"]
se2 <- se[7:12,"WinRate"]
head(out)

errbar(0:5, mn1, mn1+1.96*se1, mn1-1.96*se1, ylim=c(min(mn2-1.96*se2), max(mn1+1.96*se1)),
			 bty='l', xaxt='n',xlab="Position", ylab="Win Rate %")
lines(0:5, mn1, lty=3)
errbar(0:5, mn2, mn2+1.96*se2, mn2-1.96*se2, add=T)
lines(0:5, mn2, lty=1)
legend("topright",c("Higher Stake Games ($5/10)","Lower Stake Games ($.5/$1)"), lwd=1, lty=2:1, bty='n')
axis(1,c(0,1,2,3,4,5),labels=c("Small Blind","Big Blind","Early","Middle","Cutoff","Button"))
title("Win Rate")

linear.tests(cbind(mn2,mn2),cbind(se2,se2), games[2])
linear.tests(cbind(mn1,mn1),cbind(se1,se1), games[1])
paired.tests(cbind(mn2,mn1), cbind(se2,se1), games)



#Return Rate
mn1 <- mn[1:6,"ReturnRate"]
mn2 <- mn[7:12,"ReturnRate"]
se1 <- se[1:6,"ReturnRate"]
se2 <- se[7:12,"ReturnRate"]
head(out)

errbar(0:5, mn1, mn1+1.96*se1, mn1-1.96*se1, ylim=c(min(mn2-1.96*se2), max(mn1+1.96*se1)),
			 bty='l', xaxt='n',xlab="Position", ylab="Return Rate %")
lines(0:5, mn1, lty=3)
errbar(0:5, mn2, mn2+1.96*se2, mn2-1.96*se2, add=T)
lines(0:5, mn2, lty=1)
legend("topright",c("Higher Stake Games ($5/10)","Lower Stake Games ($.5/$1)"), lwd=1, lty=2:1, bty='n')
axis(1,c(0,1,2,3,4,5),labels=c("Small Blind","Big Blind","Early","Middle","Cutoff","Button"))
title("Return Rate")

linear.tests(cbind(mn2,mn2),cbind(se2,se2), games[2])
linear.tests(cbind(mn1,mn1),cbind(se1,se1), games[1])
paired.tests(cbind(mn2,mn1), cbind(se2,se1), games)





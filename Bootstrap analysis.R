rm(list=ls())
library(Hmisc)
load("boot_m_if_n_07.RData")

load("All_pokerhand_ids.RData")
games <- summary(factor(all.ids$table))


paired.tests <- function(mn, se, games, comparisons){
m1 <- mn[,1]
m2 <- mn[,2]
se1 <- se[,1]
se2 <- se[,2]
num1 <- games[1]
num2 <- games[2]
pooled <- ((num1-1) * se1 + (num2-1) * se2)/(num1+num2-2)
z <- (m1-m2)/pooled
p <- 2*pnorm(-abs(z))
p <- p.adjust(p , method="bonferroni", n = 4)
pos <- c("Early","Middle","Cutoff","Button")
for(i in c(1:4)) cat(sprintf("%s m1=%s,m2=%s, z=%s, p%s\n",pos[i],round(m1[i],4),round(m2[i],4), round(z[i],1), ifelse(p[i] < .001,"<.001",round(p,3))))
}

x <- pos
expand <- function(x) data.frame(Var1=x[c(1,1,1,2,2,3)],
																 Var2=x[c(2,3,4,3,4,4)])
linear.tests <- function(mns, ses, games){
	pos <- c("Early","Middle","Cutoff","Button")
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
	return(cbind(data.frame(data),m1,m2,z,p.value=ifelse(p < .001, "<.001",p)))
}


n = 19170981
m = n ^ .7

par(mfrow=c(1,1))
#fun <- sd
fun <- function(x) ((m/n)^.5) * sd(x)
mn <- with(out, tapply(cr, list(positiontype_id,group), mean))[3:6,]*100
se <- with(out, tapply(cr, list(positiontype_id,group),fun))[3:6,]*100
errbar(2:5, mn[,1], mn[,1] + 1.96 * se[,1], mn[,1] - 1.96 * se[,1], ylim=c(min(mn-2*se),max(mn+2*se)), 
			 pch='.', bty='l', xaxt='n', xlim=c(1.5,5.5), xlab="Position", ylab="Contribution Rate %")
lines(2:5, mn[,1], lty=2)
errbar(2:5, mn[,2], mn[,2] + 1.96 * se[,2], mn[,2] - 1.96 * se[,2], add=T, type='o', pch='.',  lty=1)
title("Contribution Rate")
legend("topleft",c("Higher Stake Games ($5/$10)","Lower Stake Games ($0.5/$1)"), lwd=1, lty=2:1, bty='n')
axis(1,c(2,3,4,5),labels=c("Early","Middle","Cutoff","Button"))
linear.tests(mn[,2],se[,2],games[2])
linear.tests(mn[,1],se[,1],games[1])
paired.tests(mn, se, games, 4)

mn <- with(out, tapply(wr, list(positiontype_id,group), mean))[3:6,]*100
se <- with(out, tapply(wr, list(positiontype_id,group),fun))[3:6,]*100
errbar(2:5, mn[,1], mn[,1] + 1.96 * se[,1], mn[,1] - 1.96 * se[,1], ylim=c(min(mn-2*se),max(mn+2*se)), 
			 pch='.', bty='l', xaxt='n', xlim=c(1.5,5.5), xlab="Position", ylab="Win Rate %")
lines(2:5,mn[,1], lty=2)
errbar(2:5, mn[,2], mn[,2] + 1.96 * se[,2], mn[,2] - 1.96 * se[,2], add=T, pch='.',  type='o')
title("Win Rate")
legend("topleft",c("Higher Stake Games ($5/$10)","Lower Stake Games ($0.5/$1)"), lwd=1, lty=2:1, bty='n')
axis(1,c(2,3,4,5),labels=c("Early","Middle","Cutoff","Button"))
linear.tests(mn[,2],se[,2],games[2])
linear.tests(mn[,1],se[,1],games[1])
paired.tests(mn, se, games, 4)

mn <- with(out, tapply(rr, list(positiontype_id,group), mean))[3:6,]*100
se <- with(out, tapply(rr, list(positiontype_id,group),fun))[3:6,]*100
errbar(2:5, mn[,1], mn[,1] + 1.96 * se[,1], mn[,1] - 1.96 * se[,1], ylim=c(min(mn-2*se),max(mn+2*se)), 
			 pch='.', bty='l', xaxt='n', xlim=c(1.5,5.5), xlab="Position", ylab="Return Rate %")
lines(2:5,mn[,1], lty=2)
errbar(2:5, mn[,2], mn[,2] + 1.96 * se[,2], mn[,2] - 1.96 * se[,2], add=T, pch='.',  type='o')
title("Return Rate")
legend("topleft",c("Higher Stake Games ($5/$10)","Lower Stake Games ($0.5/$1)"), lwd=1, lty=2:1, bty='n')
axis(1,c(2,3,4,5),labels=c("Early","Middle","Cutoff","Button"))
linear.tests(mn[,2],se[,2],games[2])
linear.tests(mn[,1],se[,1],games[1])
paired.tests(mn, se, games, 4)



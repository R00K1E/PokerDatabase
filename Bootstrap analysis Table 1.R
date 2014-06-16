rm(list=ls())
library(Hmisc)
load(file.path("Boostrap n of m","boot_m_of_n_07_1000.RData"))

load("All_pokerhand_ids.RData")
games <- summary(factor(all.ids$table))


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
	for(i in c(1:6)) cat(sprintf("%s mean1=%s, mean2=%s, pooled.se=%s, z=%s, p<%s\n",pos[i],round(m1[i],4),round(m2[i],4),round(pooled[i],4), round(z[i],1), ifelse(p[i] < .001,".001",round(p[i],3))))
}


run <- function(mn, se, variablename){
	errbar(1:6, mn[,1], mn[,1] + 1.96 * se[,1], mn[,1] - 1.96 * se[,1], ylim=c(min(mn-2*se),max(mn+2*se)), 
				 pch='.', bty='l', xaxt='n', xlim=c(0.5,6.5), xlab="Position", ylab=variablename)
	lines(1:6, mn[,1], lty=2)
	errbar(1:6, mn[,2], mn[,2] + 1.96 * se[,2], mn[,2] - 1.96 * se[,2], add=T, type='o', pch='.',  lty=1)
	title(variablename)
	legend("topright",c("Higher Stake Games ($5/$10)","Lower Stake Games ($0.5/$1)"), lwd=1, lty=2:1, bty='n')
	axis(1,1:6,labels=c("Small Blind","Big Blind","Early","Middle","Cutoff","Button"))
	tab <- data.frame(cbind(c("Small Blind","Big Blind","Early","Middle","Cutoff","Button"),round(mn,4),round(se,4)))
	names(tab) <- c("Position","BB mean","SB mean","BB se", "SB se")
	cat("\nMeans and Standard Errors\n")
	print(tab)
	cat("\nLower vs Higher Stake Game Comparisons (Bonferroni Adjusted z tests)\n")
	print(paired.tests(mn, se, games))
	cat("\nLower Stake Games Comparisons (Bonferroni Adjusted z tests)\n")
	print(cbind(data.frame(Stake=".5/1"),linear.tests(mn[,2],se[,2],games[2])))
	cat("\nHigher Stake Games Comparisons (Bonferroni Adjusted z tests)\n")
	print(cbind(data.frame(Stake="5/10"),linear.tests(mn[,1],se[,1],games[1])))
}

head(out)

n = 19170981
m = n ^ .7

par(mfrow=c(1,1))
#fun <- sd
fun <- function(x) ((m/n)^.5) * sd(x)

#Table 1 Results
#Win rate
mn <- with(out, tapply(wr, list(positiontype_id,group), mean)) * 100
se <- with(out, tapply(wr, list(positiontype_id,group),fun)) * 100
run(mn, se, "Win Rate %")

#Win ratio
mn <- with(out, tapply(wr_ratio, list(positiontype_id,group), mean))
se <- with(out, tapply(wr_ratio, list(positiontype_id,group),fun))
run(mn, se, "Win Rate Ratio")

#Contribution Rate
mn <- with(out, tapply(cr, list(positiontype_id,group), mean)) * 100
se <- with(out, tapply(cr, list(positiontype_id,group),fun)) * 100
run(mn, se, "Contribution Rate %")

#Contribution Ratio
mn <- with(out, tapply(cr_ratio, list(positiontype_id,group), mean))
se <- with(out, tapply(cr_ratio, list(positiontype_id,group),fun))
run(mn, se, "Contribution Rate Ratio")

#Contribution Rate Ratio - Win Rate Ratio
mn <- with(out, tapply(cr_wr_diff, list(positiontype_id,group), mean))
se <- with(out, tapply(cr_wr_diff, list(positiontype_id,group),fun))
run(mn, se, "Contribution Rate Ratio - Win Rate Ratio")

#Contribution Rate / Win Rate Difference
mn <- with(out, tapply(cr/wr, list(positiontype_id,group), mean))
se <- with(out, tapply(cr/wr, list(positiontype_id,group),fun))
run(mn, se, "Contribution Rate / Win Rate")





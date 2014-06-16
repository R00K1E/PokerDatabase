rm(list=ls())

data <- read.csv(file.path("Folds","Folds.csv"))
data

x <- aggregate(Games ~ Group + Stage, data, mean)
x$Order <- c(2,2,1,1,4,4,3,3)
	x
x <- x[order(x$Order),]
x <- x[order(x$Group),]
x

sb <- x[x$Group == "Small Blind",]
sb$Total <- sb$Games[sb$Stage == "Preflop"]


bb <- x[x$Group == "Big Blind",]
bb$Total <- bb$Games[bb$Stage == "Preflop"]

x <- rbind(sb,bb)
x$P <- x$Games / x$Total
x


for(group in unique(x$Group)){
	print(with(subset(x, Group == group), prop.test(Games, Total)))	
	print(with(subset(x, Group == group), pairwise.prop.test(Games, Total, p.adjust.method="bonferroni")))
}

out <- NULL

for(stage in unique(x$Stage)){
	tst <- with(subset(x, Stage == stage), prop.test(Games, Total))
	out <- rbind(out, data.frame(LSG=tst$estimate[1],
						 HSG=tst$estimate[2],
						 df=tst$parameter,
						 XSq=tst$statistic,
						 p.value=tst$p.value))
}

p.value(
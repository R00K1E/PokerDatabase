rm(list=ls())

data <- read.csv(file.path("Table 3","Table 3.csv"))

names(data)[4] <- "VPIP"

for(group in unique(data$Group)) {
	for(stage in unique(data$Stage)) {
 cat(sprintf("\n------------------\n", group, stage))
 cat(sprintf("\n%s in the %s stage\n", group, stage))
	cat("\n Proportions test overall all positions")
	print(with(subset(data, Group == group & Stage == stage), prop.test(VPIP, Games)))
 cat("\n1=Small Blind, 2=Big Blind, 3=Early, 4=Middle, 5=Cutoff, 6=Button")		
 print(with(subset(data, Group == group & Stage == stage), pairwise.prop.test(VPIP, Games, p.adjust.method="bonferroni")))
}}

for(group in unique(data$Group)) {
	for(position in unique(data$Position)) {
		cat(sprintf("\n------------------\n", group, position))
		cat(sprintf("\n%s across stages in the %s Position\n", group, position))
		cat("\n Proportions test overall all positions")
		print(with(subset(data, Group == group & Position == position), prop.test(VPIP, Games)))
		cat("1=Preflop, 2=Flop, 3=Turn, 4=River\n")		
		print(with(subset(data, Group == group & Position == position), pairwise.prop.test(VPIP, Games, p.adjust.method="bonferroni")))
	}}


out <- NULL
for(stage in unique(data$Stage)){
	for(position in unique(data$Position)){
		
x <- subset(data, Stage == stage & Position == position)
tst <- prop.test(x$VPIP, x$Games)
out <- rbind(out,data.frame(Stage=stage,
					 Position=position,
					 HSG=tst$estimate[1],
					 LSG=tst$estimate[2],
					 Xsquared=tst$statistic,
					 df=tst$parameter,
					 p.value=tst$p.value))
}}

out$bonf.p.value = p.adjust(out$p.value,method="bonferroni",nrow(out))
write.csv(out,"Between Group Comparisons.csv", row.names=F)

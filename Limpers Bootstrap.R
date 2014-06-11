rm(list=ls())
library(compiler)
setwd("J:/Management Research/UoS PFM/Docs/WORK/MANAGEMENT/STUDIES/Favls poker/LEIGHTON")
load(file.path("Limpers","big blind table.RData"))
load(file.path("Limpers","small blind table.RData"))

bb.limpers$group <- "Big Blind"
sb.limpers$group <- "Small Blind"
main <- rbind(bb.limpers, sb.limpers)
main$games <- 1
rm(bb.limpers,sb.limpers)

agg <- cmpfun(function(win, games, totalbet, net_return, positiontype_id, group){
	aggregate(cbind(win,games,totalbet,net_return) ~ positiontype_id + group, data, sum, na.rm=T)
})

boot.sample <- cmpfun(function(main){
	index <- 1:nrow(main)
	s <- sample(index,length(index),replace=T)
	return(main[s,])
})

total <- 1000
pb <- winProgressBar(title ="progress bar", min = 0, max = total, width = 300)

out <- NULL

for(i in 1:total){
try({
data <- boot.sample(main)

x <- agg(data$win, 
				 data$games, 
				 data$totalbet, 
				 data$net_return, 
				 data$positiontype_id, 
				 data$group)

if(nrow(x) == 12){
x$totalbet <- x$totalbet / 100
x$net_return <- x$net_return / 100

Games=c(6704433,6704567,6704545,6704925,6704502,6704112,
				12466372,12466473,12466533,12466509,12466469,12466468)

out <- rbind(out,data.frame(Sample=i,
					 Position=x$positiontype_id,
					 ReturnRate = x$net_return/x$totalbet*100,
					 TotalContribution=x$totalbet,
					 ContPerGame=x$totalbet/x$games,
					 WinRate=x$win/x$games*100,
					 Games=x$games,
					 AllGames=Games,
					 PercentofAllGames=x$games/Games*100))
					 
}	
if(i %% 100 == 0) save(out, file=file.path("Limpers",sprintf("boot_limpers_%s.RData",i)))
setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),"% complete"))
})
}


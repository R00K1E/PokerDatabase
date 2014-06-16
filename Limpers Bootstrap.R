rm(list=ls())
library(compiler)
library(data.table)

setwd("J:/Management Research/UoS PFM/Docs/WORK/MANAGEMENT/STUDIES/Favls poker/LEIGHTON")
load(file.path("Limpers","bigblinds limper table.RData"))
out$group <- "Big Blinds"
main <- out
rm(out)
load(file.path("Limpers","smallblinds limper table.RData"))
out$group <- "Small Blinds"
main <- rbind(main,out)
rm(out)

head(main)
main$games <- 1
main$group <- as.factor(main$group)
main$win <- as.numeric(main$win)
main$games <- as.numeric(main$games)
main$totalbet <- as.numeric(main$totalbet)
main$net_return <- as.numeric(main$net_return)


##run
main <- data.table(main)

boot.sample <- cmpfun(function(main){
	index <- 1:nrow(main)
	s <- sample(index,length(index),replace=T)
	return(main[s,])
})


total <- 1000
pb <- winProgressBar(title ="progress bar", min = 0, max = total, width = 300)

i=1
out <- NULL

for(i in 1:total){
	try({
		rm(data)
		data <- data.table(boot.sample(main))
		
		
		x <- data[,list(wins=sum(win), games=sum(games), totalbet=sum(totalbet),
										net_return=sum(net_return)), by = c('positiontype_id','group')]
		x <- x[order(c(x$positiontype_id)),]
		x <- x[order(c(x$group)),]
		
		if(nrow(x) == 12){
			x$totalbet <- x$totalbet / 100
			x$net_return <- x$net_return / 100
			
			Games=c(6704433,6704567,6704545,6704925,6704502,6704112,
							12466372,12466473,12466533,12466509,12466469,12466468)
			
			out <- rbind(out,data.frame(Sample=i,
																	Group=x$group,
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

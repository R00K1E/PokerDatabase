rm(list=ls())
setwd("J:/Management Research/UoS PFM/Docs/WORK/MANAGEMENT/STUDIES/Favls poker/LEIGHTON")
source("boot functions.R")


#import all.ids
load("All_pokerhand_ids.RData")

i = 1
out <- NULL
n = nrow(all.ids)
m = nrow(all.ids) ^ .7

total <- 1000
pb <- winProgressBar(title ="progress bar", min = 0, max = total, width = 300)

for(i in 1:total){
	
data <- returnBoot(all.ids, replace=F, size=m)

x <- agg(data$win, 
				 data$games, 
				 data$totalbet, 
				 data$net_return, 
				 data$positiontype_id, 
				 data$group)

y <- aggregate(list(total=x$totalbet), list(group=x$group), sum)
x <- merge(x,y)
x$wr <- x$win/x$games
x$cr <- x$totalbet/x$total
x$rr <- x$net_return/x$totalbet
x$ev <- x$net_return / x$games / 100
temp <- x[x$positiontype_id == 2,c("group","wr","cr","rr")]
names(temp) <- c("group", "wrearly", "crearly","rrearly")
k <- merge(x,temp)
x$wr_ratio <- k$wr/k$wrearly
x$cr_ratio <- k$cr/k$crearly
x$rr_ratio <- k$rr/k$rrearly
x$cr_wr_diff <- x$cr_ratio - x$wr_ratio
x$cr_rr_diff <- x$cr_ratio - x$rr_ratio
x$sample <- i
x
out <- rbind(out, x)
if(i %% 100 == 0) save(out, file=sprintf("boot_m_of_n_07_%s.RData",i))
setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),"% complete"))
}

close(pb)

save(out, file="boot_m_if_n_07.RData")


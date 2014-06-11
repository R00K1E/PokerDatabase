library(RPostgreSQL)
library(compiler)
drv <- dbDriver("PostgreSQL")

query <- cmpfun(function(database, querystring){
	drv <- dbDriver("PostgreSQL")
	try(dbDisconnect(con),silent=T)
	if(database == 'bigblinds') con <- dbConnect(drv, dbname="newpoker",host="localhost",port=5432, user="postgres")
	if(database == 'smallblinds') con <- dbConnect(drv, dbname="poker",host="localhost",port=5432, user="postgres")
	Table <- dbGetQuery(con,querystring)
	dbDisconnect(con)
	dbUnloadDriver(drv)
	return(Table)	
})


buildtable <- cmpfun(function(database, table, name){
	drv <- dbDriver("PostgreSQL")
	try(dbDisconnect(con),silent=T)
	if(database == 'bigblinds') con <- dbConnect(drv, dbname="newpoker",host="localhost",port=5432, user="postgres")
	if(database == 'smallblinds') con <- dbConnect(drv, dbname="poker",host="localhost",port=5432, user="postgres")
	if(dbExistsTable(con,name)) {dbRemoveTable(con,name)}
	postgresqlWriteTable(con, name, table, row.names = FALSE)
	dbDisconnect(con)
	dbUnloadDriver(drv)
})



returnBoot <- cmpfun(function(all.ids, replace=F, size = 100){
	
	smple <- all.ids[sample(nrow(all.ids), size, replace=replace),]
	
	bb <- subset(smple, table == "bigblinds")
	buildtable("bigblinds", bb, "boot")
	nothing <- query("bigblinds", "create index id_index on boot (pokerhand_id)")
	bb.boot <- query("bigblinds", "select b.pokerhand_id, playerhand_id, positiontype_id, net_return, win, totalbet from boot as s join bigtable as b on s.pokerhand_id = b.pokerhand_id")
	bb.boot$group <- "Big Blind"
	
	sb <- subset(smple, table == "smallblinds")
	buildtable("smallblinds", sb, "boot")
	nothing <- query("smallblinds", "create index id_index on boot (pokerhand_id)")
	sb.boot <- query("smallblinds", "select b.pokerhand_id, playerhand_id, positiontype_id, net_return, win, totalbet from boot as s join bigtable as b on s.pokerhand_id = b.pokerhand_id")
	sb.boot$group <- "Small Blind"
	
	data <- rbind(sb.boot, bb.boot)
	data$games <- 1
	return(data)})


agg <- cmpfun(function(win, games, totalbet, net_return, positiontype_id, group){
	aggregate(cbind(win,games,totalbet,net_return) ~ positiontype_id + group, data, sum)
})



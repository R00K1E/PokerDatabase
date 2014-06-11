SELECT

G.positiontype_id,

sum(case when G.preflopplayeractiontype_id != -1 then 1 else 0 end) as PreflopGames,
sum(case when G.flopplayeractiontype_id != -1 then 1 else 0 end) as FlopGames,
sum(case when G.turnplayeractiontype_id != -1 then 1  else 0 end) as TurnGames,
sum(case when G.riverplayeractiontype_id != -1 then 1 else 0 end) as RiverGames,

sum(case when G.preflopplayeractiontype_id in (select actiontype_id from actiontypes where trim(actionstring) like '%F%') then 1 else 0 end) as PreflopFold,
sum(case when G.flopplayeractiontype_id in (select actiontype_id from actiontypes where trim(actionstring) like '%F%') then 1 else 0 end) as FlopFold,
sum(case when G.turnplayeractiontype_id in (select actiontype_id from actiontypes where trim(actionstring) like '%F%') then 1 else 0 end) as TurnFold,
sum(case when G.riverplayeractiontype_id in (select actiontype_id from actiontypes where trim(actionstring) like '%F%') then 1 else 0 end) as RiverFold


FROM playerhandscashkeycolumns AS G
LEFT JOIN playerhandscashmisc AS M
ON G.playerhand_id=M.playerhand_id 
LEFT JOIN playerhandsflop AS F
ON G.playerhand_id=F.playerhand_id
LEFT JOIN playerhandsturn AS T
ON G.playerhand_id=T.playerhand_id
LEFT JOIN playerhandsriver AS R
ON G.playerhand_id=R.playerhand_id

where G.numberofplayers=6
group by G.positiontype_id

SELECT
G.positiontype_id,
sum(G.netamountwon) AS net_return,
sum(case when G.netamountwon > 0 then 1 else 0 end)  AS win,
sum(case when G.netamountwon < 0 then 1 else 0 end)  AS loss,
sum(case when G.netamountwon = 0 then 1 else 0 end)  AS even,
count(G.positiontype_id)  AS games,

sum(M.betamountpreflop+
M.callamountpreflop+
M.postamountpreflop+
COALESCE(F.betamount,0)+
COALESCE(F.callamount,0)+
COALESCE(T.betamount,0)+
COALESCE(T.callamount,0)+
COALESCE(R.betamount,0)+
COALESCE(R.callamount,0)) AS totalbet


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

and G.preflopplayeractiontype_id in 

(select actiontype_id from actiontypes
where 
trim(actionstring) not like '%F%'
and trim(actionstring) not like '%R%'
and trim(actionstring) not like '%B%'
and  trim(actionstring) like '%C%'
and  trim(actionstring) not like '')

group by
G.positiontype_id
order by positiontype_id





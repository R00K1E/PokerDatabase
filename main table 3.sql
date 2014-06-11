SELECT

G.pokerhand_id,
G.playerhand_id,
G.positiontype_id,
G.netamountwon AS net_return,
case when G.netamountwon > 0 then 1 else 0 end  AS win,
case when G.netamountwon < 0 then 1 else 0 end  AS loss,
case when G.netamountwon = 0 then 1 else 0 end  AS even,


case when G.netamountwon > 0 then G.netamountwon else 0 end AS returnwinninghand,

M.betamountpreflop+
M.callamountpreflop+
M.postamountpreflop+
COALESCE(F.betamount,0)+
COALESCE(F.callamount,0)+
COALESCE(T.betamount,0)+
COALESCE(T.callamount,0)+
COALESCE(R.betamount,0)+
COALESCE(R.callamount,0) AS totalbet,


M.postamountpreflop AS preflop_post,

M.betamountpreflop+
M.callamountpreflop AS preflop_cont,

COALESCE(F.betamount,0) + COALESCE(F.callamount,0) AS flop_cont,
COALESCE(T.betamount,0) + COALESCE(T.callamount,0) AS turn_cont,
COALESCE(R.betamount,0) + COALESCE(R.callamount,0) AS river_cont,

G.preflopaction_id As gamefacingplayerpreflop,
G.firstpreflopactiontype_id  As playersfirstaction,
G.preflopplayeractiontype_id,
G.flopplayeractiontype_id,
G.turnplayeractiontype_id,
G.riverplayeractiontype_id



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

limit 100
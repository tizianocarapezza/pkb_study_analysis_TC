#################################################TUG Code mit allen 43##########################################################
datdir="../data"
tug_scores = data.frame(tug_pre=NULL, tug_post=NULL)

read.tug <- function(sub, time)
{
  tug = read.csv(file.path(datdir
                           , sub
                           , paste( sub,"_TUG_ON_",time,".CSV"
                                    , sep=""))
                 , header=FALSE
                 , stringsAsFactors = FALSE)
}

for(sub in dir(datdir))
{
  tug_scores=rbind(tug_scores, data.frame(
    tug_pre = read.tug(sub, "pre")
    ,tug_post = read.tug(sub, "post")))
}
colnames(tug_scores) <- c("tug_pre", "tug_post")

#tug_scores <- tug_scores[-25, ]

tug_scores$diff = tug_scores$tug_pre - tug_scores$tug_post

tug_scores <- tug_scores[-25, ]
#Zeile 25 rausnehmen wegen dropout

mean_tug_pre <- mean(tug_scores$tug_pre)
sd_tug_pre <- sd(tug_scores$tug_pre)
mean_tug_post <- mean(tug_scores$tug_post)
sd_tug_post <- sd(tug_scores$tug_post)
mean_tug_Diff <- mean(tug_scores$diff)
sd_tug_Diff <- sd(tug_scores$diff)
#berechnen der TUG-Daten

MW_tug =c(mean_tug_pre, mean_tug_post, mean_tug_Diff)
StA_tug=c(sd_tug_pre, sd_tug_post, sd_tug_Diff)
TUG_Auswertung <- data.frame(MW_tug, StA_tug)
rownames(TUG_Auswertung)<-c("TUG pre", "TUG post", "TUG Diff")
colnames(TUG_Auswertung)<-c("MW", "StA")
##########################
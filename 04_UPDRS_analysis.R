datdir="../data"
updrs_scores = data.frame(updrs_pre=NULL, updrs_post=NULL)

read.updrs <- function(sub, time)
{
  updrs = sum(as.numeric(
    read.csv(file.path(datdir
                       , sub
                       , paste( sub,"_UPDRS_ON_",time,".CSV"
                                , sep=""))
             , header=FALSE
             , stringsAsFactors = FALSE)$V2[6:38]))
}

for(sub in dir(datdir))
{
  updrs_scores=rbind(updrs_scores, data.frame(
    ID = sub,
    updrs_pre   = read.updrs(sub, "pre")
    ,updrs_post = read.updrs(sub, "post")
  ))
}

updrs_scores$diff = updrs_scores$updrs_pre - updrs_scores$updrs_post

updrs_scores$diff_pct <- ifelse(
  updrs_scores$updrs_pre == 0,
  NA,
  updrs_scores$diff / updrs_scores$updrs_pre * 100
)

mean_updrs_pre <- mean(updrs_scores$updrs_pre)
sd_updrs_pre <- sd(updrs_scores$updrs_pre)
mean_updrs_post <- mean(updrs_scores$updrs_post)
sd_updrs_post <- sd(updrs_scores$updrs_post)
mean_updrs_Diff <- mean(updrs_scores$diff)
sd_updrs_Diff <- sd(updrs_scores$diff)
mean_updrs_diff_pct <- mean(updrs_scores$diff_pct)
sd_updrs_diff_pct <- sd(updrs_scores$diff_pct)

MW_updrs =c(mean_updrs_pre, mean_updrs_post, mean_updrs_Diff, mean_updrs_diff_pct)
StA_updrs=c(sd_updrs_pre, sd_updrs_post, sd_updrs_Diff, sd_updrs_diff_pct)
UPDRS_Auswertung <- data.frame(MW_updrs, StA_updrs)
rownames(UPDRS_Auswertung)<-c("UPDRS pre", "UPDRS post", "UPDRS Diff", "UPDRS PCT")
colnames(UPDRS_Auswertung)<-c("MW", "StA")
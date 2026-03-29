datdir="../data"
pdq39_scores = data.frame( Pat.ID=NULL
                           ,mobility_pre=NULL
                           ,mobility_post=NULL
                           ,activities_pre=NULL
                           ,activities_post=NULL
                           ,emotional_well_being_pre=NULL
                           ,emotional_well_being_post=NULL
                           ,stigma_pre=NULL
                           ,stigma_post=NULL
                           ,social_support_pre=NULL
                           ,social_support_post=NULL
                           ,cognitions_pre=NULL
                           ,cognitions_post=NULL
                           ,communication_pre=NULL
                           ,communication_post=NULL
                           ,bodily_discomfort_pre=NULL
                           ,bodily_discomfort_post=NULL
                           ,total_pre=NULL
                           ,total_post=NULL
)

read.pdq39 <- function(sub, time, z)
{
  pdq39 = sum(as.numeric(
    read.csv(file.path(datdir
                       , sub
                       , paste( sub,"_PDQ39_",time,".CSV", sep=""))
             , header=FALSE
             , stringsAsFactors = FALSE)$V1[z]))
}

# Berechnen der PDQ-39 subscores
for(sub in dir(datdir))
{
  pdq39_scores=rbind(pdq39_scores, data.frame(
    Pat.ID = as.integer(sub)
    ,mobility_pre              = read.pdq39(sub, "pre",1:10)  *100/40
    ,mobility_post             = read.pdq39(sub, "post",1:10) *100/40
    ,activities_pre            = read.pdq39(sub, "pre",11:16) *100/24
    ,activities_post           = read.pdq39(sub, "post",11:16)*100/24
    ,emotional_well_being_pre  = read.pdq39(sub, "pre",17:22) *100/24
    ,emotional_well_being_post = read.pdq39(sub, "post",17:22)*100/24
    ,stigma_pre                = read.pdq39(sub, "pre",23:26) *100/16
    ,stigma_post               = read.pdq39(sub, "post",23:26)*100/16
    ,social_support_pre        = read.pdq39(sub, "pre",27:29) *100/12
    ,social_support_post       = read.pdq39(sub, "post",27:29)*100/12
    ,cognitions_pre            = read.pdq39(sub, "pre",30:33) *100/16
    ,cognitions_post           = read.pdq39(sub, "post",30:33)*100/16
    ,communication_pre         = read.pdq39(sub, "pre",34:36) *100/12
    ,communication_post        = read.pdq39(sub, "post",34:36)*100/12
    ,bodily_discomfort_pre     = read.pdq39(sub, "pre",37:39) *100/12
    ,bodily_discomfort_post    = read.pdq39(sub, "post",37:39)*100/12
  ))
}

# Berechnen der PDQ39 Summenscores
pdq39_scores$PDQ39SI_pre = ( pdq39_scores$mobility_pre
                             +pdq39_scores$activities_pre
                             +pdq39_scores$emotional_well_being_pre
                             +pdq39_scores$stigma_pre
                             +pdq39_scores$social_support_pre
                             +pdq39_scores$cognitions_pre
                             +pdq39_scores$communication_pre
                             +pdq39_scores$bodily_discomfort_pre)/8
pdq39_scores$PDQ39SI_post = ( pdq39_scores$mobility_post
                              +pdq39_scores$activities_post
                              +pdq39_scores$emotional_well_being_post
                              +pdq39_scores$stigma_post
                              +pdq39_scores$social_support_post
                              +pdq39_scores$cognitions_post
                              +pdq39_scores$communication_post
                              +pdq39_scores$bodily_discomfort_post)/8

pdq39_scores$mobility_diff = pdq39_scores$mobility_pre-pdq39_scores$mobility_post
pdq39_scores$activities_diff = pdq39_scores$activities_pre-pdq39_scores$activities_post
pdq39_scores$emotional_well_being_diff = pdq39_scores$emotional_well_being_pre-pdq39_scores$emotional_well_being_post
pdq39_scores$stigma_diff = pdq39_scores$stigma_pre-pdq39_scores$stigma_post
pdq39_scores$social_support_diff = pdq39_scores$social_support_pre-pdq39_scores$social_support_post
pdq39_scores$cognitions_diff = pdq39_scores$cognitions_pre-pdq39_scores$cognitions_post
pdq39_scores$communication_diff = pdq39_scores$communication_pre-pdq39_scores$communication_post
pdq39_scores$bodily_discomfort_diff = pdq39_scores$bodily_discomfort_pre-pdq39_scores$bodily_discomfort_post
pdq39_scores$PDQ39SI_diff = pdq39_scores$PDQ39SI_pre- pdq39_scores$PDQ39SI_post

pdq39_scores <-pdq39_scores[c("Pat.ID","mobility_pre", "mobility_post", "mobility_diff",
                              "activities_pre", "activities_post", "activities_diff",
                              "emotional_well_being_pre", "emotional_well_being_post", "emotional_well_being_diff",
                              "stigma_pre", "stigma_post", "stigma_diff",
                              "social_support_pre", "social_support_post", "social_support_diff",
                              "cognitions_pre", "cognitions_post", "cognitions_diff",
                              "communication_pre", "communication_post", "communication_diff",
                              "bodily_discomfort_pre", "bodily_discomfort_post", "bodily_discomfort_diff",
                              "PDQ39SI_pre", "PDQ39SI_post", "PDQ39SI_diff")]

# Berechnen von MW und SD aller Unterkategorien und Summenscores
mean_Mobility_pre <- mean(pdq39_scores$mobility_pre)
sd_Mobility_pre   <- sd  (pdq39_scores$mobility_pre)
mean_Mobility_post<- mean(pdq39_scores$mobility_post)
sd_Mobility_post  <- sd  (pdq39_scores$mobility_post)
mean_Mobility_diff<- mean(pdq39_scores$mobility_diff)
sd_Mobility_diff  <- sd  (pdq39_scores$mobility_diff)

mean_activities_pre <- mean(pdq39_scores$activities_pre)
sd_activities_pre   <- sd  (pdq39_scores$activities_pre)
mean_activities_post<- mean(pdq39_scores$activities_post)
sd_activities_post  <- sd  (pdq39_scores$activities_post)
mean_activities_diff<- mean(pdq39_scores$activities_diff)
sd_activities_diff  <- sd  (pdq39_scores$activities_diff)

mean_emotional_well_being_pre <- mean(pdq39_scores$emotional_well_being_pre)
sd_emotional_well_being_pre   <- sd  (pdq39_scores$emotional_well_being_pre)
mean_emotional_well_being_post<- mean(pdq39_scores$emotional_well_being_post)
sd_emotional_well_being_post  <- sd  (pdq39_scores$emotional_well_being_post)
mean_emotional_well_being_diff<- mean(pdq39_scores$emotional_well_being_diff)
sd_emotional_well_being_diff  <- sd  (pdq39_scores$emotional_well_being_diff)

mean_stigma_pre <- mean(pdq39_scores$stigma_pre)
sd_stigma_pre   <- sd  (pdq39_scores$stigma_pre)
mean_stigma_post<- mean(pdq39_scores$stigma_post)
sd_stigma_post  <- sd  (pdq39_scores$stigma_post)
mean_stigma_diff<- mean(pdq39_scores$stigma_diff)
sd_stigma_diff  <- sd  (pdq39_scores$stigma_diff)

mean_social_support_pre <- mean(pdq39_scores$social_support_pre)
sd_social_support_pre   <- sd  (pdq39_scores$social_support_pre)
mean_social_support_post<- mean(pdq39_scores$social_support_post)
sd_social_support_post  <- sd  (pdq39_scores$social_support_post)
mean_social_support_diff<- mean(pdq39_scores$social_support_diff)
sd_social_support_diff  <- sd  (pdq39_scores$social_support_diff)

mean_cognitions_pre <- mean(pdq39_scores$cognitions_pre)
sd_cognitions_pre   <- sd  (pdq39_scores$cognitions_pre)
mean_cognitions_post<- mean(pdq39_scores$cognitions_post)
sd_cognitions_post  <- sd  (pdq39_scores$cognitions_post)
mean_cognitions_diff<- mean(pdq39_scores$cognitions_diff)
sd_cognitions_diff  <- sd  (pdq39_scores$cognitions_diff)

mean_communication_pre <- mean(pdq39_scores$communication_pre)
sd_communication_pre   <- sd  (pdq39_scores$communication_pre)
mean_communication_post<- mean(pdq39_scores$communication_post)
sd_communication_post  <- sd  (pdq39_scores$communication_post)
mean_communication_diff<- mean(pdq39_scores$communication_diff)
sd_communication_diff  <- sd  (pdq39_scores$communication_diff)

mean_bodily_discomfort_pre <- mean(pdq39_scores$bodily_discomfort_pre)
sd_bodily_discomfort_pre   <- sd  (pdq39_scores$bodily_discomfort_pre)
mean_bodily_discomfort_post<- mean(pdq39_scores$bodily_discomfort_post)
sd_bodily_discomfort_post  <- sd  (pdq39_scores$bodily_discomfort_post)
mean_bodily_discomfort_diff<- mean(pdq39_scores$bodily_discomfort_diff)
sd_bodily_discomfort_diff  <- sd  (pdq39_scores$bodily_discomfort_diff)

mean_PDQ39SI_pre <- mean(pdq39_scores$PDQ39SI_pre)
sd_PDQ39SI_pre   <- sd  (pdq39_scores$PDQ39SI_pre)
mean_PDQ39SI_post<- mean(pdq39_scores$PDQ39SI_post)
sd_PDQ39SI_post  <- sd  (pdq39_scores$PDQ39SI_post)
mean_PDQ39SI_diff<- mean(pdq39_scores$PDQ39SI_diff)
sd_PDQ39SI_diff  <- sd  (pdq39_scores$PDQ39SI_diff)


MW_pdq39=c(mean_Mobility_pre, mean_Mobility_post, mean_Mobility_diff,
           mean_activities_pre, mean_activities_post, mean_activities_diff,
           mean_emotional_well_being_pre, mean_emotional_well_being_post, mean_emotional_well_being_diff,
           mean_stigma_pre, mean_stigma_post, mean_stigma_diff,
           mean_social_support_pre, mean_social_support_post, mean_social_support_diff,
           mean_cognitions_pre, mean_cognitions_post, mean_cognitions_diff,
           mean_communication_pre, mean_communication_post, mean_communication_diff,
           mean_bodily_discomfort_pre, mean_bodily_discomfort_post, mean_bodily_discomfort_diff,
           mean_PDQ39SI_pre, mean_PDQ39SI_post, mean_PDQ39SI_diff)
StA_pdq39=c(sd_Mobility_pre, sd_Mobility_post, sd_Mobility_diff,
            sd_activities_pre, sd_activities_post, sd_activities_diff,
            sd_emotional_well_being_pre, sd_emotional_well_being_post, sd_emotional_well_being_diff,
            sd_stigma_pre, sd_stigma_post, sd_stigma_diff,
            sd_social_support_pre, sd_social_support_post, sd_social_support_diff,
            sd_cognitions_pre, sd_cognitions_post, sd_cognitions_diff,
            sd_communication_pre, sd_communication_post, sd_communication_diff,
            sd_bodily_discomfort_pre, sd_bodily_discomfort_post, sd_bodily_discomfort_diff,
            sd_PDQ39SI_pre, sd_PDQ39SI_post, sd_PDQ39SI_diff)

PDQ39_Auswertung <- data.frame(MW_pdq39, StA_pdq39)
rownames(PDQ39_Auswertung) <-c("mobility_pre", "mobility_post", "mobility_diff",
                               "activities_pre", "activities_post", "activities_diff",
                               "emotional_well_being_pre", "emotional_well_being_post", "emotional_well_being_diff",
                               "stigma_pre", "stigma_post", "stigma_diff",
                               "social_support_pre", "social_support_post", "social_support_diff",
                               "cognitions_pre", "cognitions_post", "cognitions_diff",
                               "communication_pre", "communication_post", "communication_diff",
                               "bodily_discomfort_pre", "bodily_discomfort_post", "bodily_discomfort_diff",
                               "PDQ39SI_pre", "PDQ39SI_post", "PDQ39SI_diff")
colnames(PDQ39_Auswertung)<-c("MW", "StA")
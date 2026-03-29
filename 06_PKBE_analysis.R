datdir="../data"
questionnaire_scores = data.frame(Zufriedenheit=NULL
                                  ,Alltag=NULL
                                  ,Gang=NULL
                                  ,Beweglicheit=NULL
                                  ,Tremor=NULL)

read.questionnaire <- function(sub)
{
  questionnaire_scores = read.csv(file.path(datdir
                                            , sub
                                            
                                            , paste( sub,"_questionnaire.CSV"
                                                     , sep=""))
                                  , header=FALSE
                                  , stringsAsFactors = FALSE)
  questionnaire_scores=t(questionnaire_scores)
}

for(sub in dir(datdir))
{
  questionnaire_scores = rbind(questionnaire_scores, data.frame(
    questionnaire = read.questionnaire(sub)
  ))
}
colnames(questionnaire_scores) <- c("Zufriedenheit"
                                    ,"Alltag"
                                    ,"Gang"
                                    ,"Beweglichkeit"
                                    ,"Tremor")


mean_Zufriedenheit <- mean(questionnaire_scores$Zufriedenheit)
sd_Zufriedenheit   <- sd  (questionnaire_scores$Zufriedenheit)
mean_Alltag        <- mean(questionnaire_scores$Alltag)
sd_Alltag          <- sd  (questionnaire_scores$Alltag)
mean_Gang          <- mean(questionnaire_scores$Gang)
sd_Gang            <- sd  (questionnaire_scores$Gang)
mean_Beweglichkeit <- mean(questionnaire_scores$Beweglichkeit)
sd_Beweglichkeit   <- sd  (questionnaire_scores$Beweglichkeit)
mean_Tremor        <- mean(questionnaire_scores$Tremor, na.rm=TRUE)
sd_Tremor          <- sd  (questionnaire_scores$Tremor, na.rm=TRUE)

MW_questionnaire= c(mean_Zufriedenheit, mean_Alltag, mean_Gang, mean_Beweglichkeit, mean_Tremor)
StA_questionnaire= c(sd_Zufriedenheit, sd_Alltag, sd_Gang, sd_Beweglichkeit, sd_Tremor)
questionnaire_Auswertung <- data.frame(MW_questionnaire,StA_questionnaire)
rownames(questionnaire_Auswertung)<-colnames(questionnaire_scores)
colnames(questionnaire_Auswertung)<-c("MW", "StA")

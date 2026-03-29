factors = read.csv("../docs/medsfile.csv", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)
datdir="../data"
LED_scores=data.frame(Pat.ID=NULL,LED_first=NULL, LED_last=NULL)

read.medication<-function(sub)
{
  meds = read.csv(file.path(datdir
                            , sub
                            , paste( sub("^0","",sub),"_medication.csv", sep=""))
                  , header=TRUE
                  , stringsAsFactors = FALSE)
}

for (sub in dir(datdir, pattern = "^[0-9]"))
{
  print(sub)
  meds=read.medication(sub)
  
  df = aggregate(dose~date+time+drug, data = meds, sum)
  out = list(); cnt = 0
  
  for(currdate in unique(df$date)){
    daymeds = df[df$date == currdate,]
    daymeds$posixtime = as.POSIXct(paste(daymeds$date, daymeds$time))
    
    if("levodopa" %in% daymeds$drug){
      dayldopa = sum(daymeds$dose[daymeds$drug == "levodopa"])}
    else{dayldopa = 0}
    
    if("levodopa_retard" %in% daymeds$drug){
      dayldopa = dayldopa + sum(daymeds$dose[daymeds$drug == "levodopa_retard"])*.75}
    
    LED = 0
    for(currtime in unique(daymeds$time)){
      timemeds = daymeds[daymeds$time == currtime,]
      
      if("levodopa" %in% timemeds$drug){
        ldopa = sum(timemeds$dose[timemeds$drug == "levodopa"])}
      else{ldopa = 0}
      
      if("levodopa_retard" %in% timemeds$drug){
        ldopa = ldopa + sum(timemeds$dose[timemeds$drug == "levodopa_retard"])*.75}
      
      for(currdrug in unique(timemeds$drug)){
        
        if(currdrug == "opicapon_retard"){ LED = LED + dayldopa * .5 ; next}
        if(currdrug == "opicapon")       { LED = LED + dayldopa * .5 ; next}
        if(currdrug == "entacapon")      { LED = LED +    ldopa * .33; next}
        if(currdrug == "tolcapon")       { LED = LED +    ldopa * .5 ; next}
        if(currdrug == "levodopa"|currdrug == "levodopa_retard"){
          # handles single levodopa doses without tol-/entacapone
          if( !any(c("entacapon","tolcapon") %in% timemeds$drug) ){
            ltime    = as.POSIXct(paste(currdate, currtime))
            
            enttime =  ltime - as.difftime(4, units = "hours")      
            relmeds = daymeds[daymeds$posixtime >= enttime & daymeds$posixtime <= ltime,]
            if( "entacapon" %in% relmeds$drug ){ LED = LED + ldopa * .33}
            
            toltime =  ltime - as.difftime(8, units = "hours")      
            relmeds = daymeds[daymeds$posixtime >= toltime & daymeds$posixtime <= ltime,]
            if( "tolcapon" %in% relmeds$drug ){ LED = LED + ldopa * .5}
          }}
        
        LED = LED + timemeds$dose[timemeds$drug == currdrug] *
          factors$factor[factors$drug.name == currdrug]
      }
    }
    cnt = cnt + 1
    out[[cnt]] = data.frame(Pat.ID = sub, date = currdate, LED = LED)
  }
  
  Pat.ID <- sub
  out <- do.call("rbind", out)
  out <- out[order(as.Date(out$date, format="%Y-%m-%d")),]
  LED_first <- out[1,3]
  LED_last  <- out[nrow(out),3]
  LED_scores=rbind(LED_scores, data.frame(Pat.ID,LED_first,LED_last))
}

LED_scores$diff = LED_scores$LED_first - LED_scores$LED_last
mean_LED_first <- mean(LED_scores$LED_first)
sd_LED_first   <- sd(LED_scores$LED_first)
mean_LED_last  <- mean(LED_scores$LED_last)
sd_LED_last    <- sd(LED_scores$LED_last)
mean_LED_diff  <- mean(LED_scores$diff)
sd_LED_diff    <- sd(LED_scores$diff)

Mw_LED = c(mean_LED_first, mean_LED_last, mean_LED_diff)
Sd_LED = c(sd_LED_first, sd_LED_last, sd_LED_diff)
LED_Auswertung <- data.frame(Mw_LED, Sd_LED)
rownames(LED_Auswertung) <- c("LED erster Tag","LED letzter Tag","Differenz")
colnames(LED_Auswertung) <- c("Mittelwert", "Standardabweichung")

# ---Baseline---

cnt=0
for(patnum in dir("../data"))
{ cnt = cnt+1
fname = paste ("../data/",patnum,"/",patnum,"_baseline.csv",
               sep="")
if(cnt==1) baseline = read.csv (fname)
if (cnt > 1) baseline = rbind (baseline, read.csv(fname))
}
#Zusammenführung aller baselines in einer baseline

mean_age <- mean(baseline$age)
sd_age <- sd(baseline$age)
mean_hy <- mean(baseline$hoehnyahr)
sd_hy <- sd(baseline$hoehnyahr)
mean_Erkrankungsd <- mean(2022- baseline$pd_onset)
sd_Erkrankungsd <- sd(2022- baseline$pd_onset)
Anzahl_m <- length(which(baseline$sex == "male"))
Anzahl_w <- length(which(baseline$sex == "female"))
Anzahl_dbs <- length(which(baseline$dbs == "yes"))
walking_aids <- length(which(baseline$walking_aids != "no"))
Anzahl_Rollator <- length(which(baseline$walking_aids == "rollator"))
Anzahl_Sticks <- length(which(baseline$walking_aids == "stick"))
Anzahl_Crutch <- length(which(baseline$walking_aids == "crutch"))
#Berechnungen der Baseline Daten

MW_baseline= c(mean_age, mean_Erkrankungsd, mean_hy,mean_LED_first
               ,mean_LED_last,Anzahl_m, Anzahl_w,Anzahl_dbs,walking_aids,
               Anzahl_Rollator,Anzahl_Sticks,Anzahl_Crutch)
StA_baseline=c(sd_age, sd_Erkrankungsd, sd_hy,sd_LED_first
               ,sd_LED_last,"-", "-","-","-","-","-","-")
klinische_Daten <- data.frame(MW_baseline,StA_baseline)
rownames(klinische_Daten)<-c("Alter","Erkrankungsdauer","Hoehn&Yahr",
                             "LED erster Tag","LED letzter Tag",
                             "Anzahl Maenner", "Anzahl Frauen","Anzahl DBS","Anzahl Ganghilfen",
                             "Anzahl Rollator","Anzahl Stoecke","Anzahl Kruecken")
colnames(klinische_Daten)<-c("MW","StA")
klinische_Daten$StA[1:5] <- formatC(as.numeric(klinische_Daten$StA[1:5]) 
                                    , digits = 2, width = NULL,
                                    format = "f", flag = "", mode = NULL,
                                    big.mark = "", big.interval = NULL,
                                    small.mark = "", small.interval = NULL,
                                    decimal.mark = ".", 
                                    preserve.width = "individual")
klinische_Daten$MW <- formatC(as.numeric(klinische_Daten$MW) 
                              , digits = 2, width = NULL,
                              format = "f", flag = "", mode = NULL,
                              big.mark = "", big.interval = NULL,
                              small.mark = "", small.interval = NULL,
                              decimal.mark = ".", 
                              preserve.width = "individual")
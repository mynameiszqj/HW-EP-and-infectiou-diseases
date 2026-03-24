
library(dplyr)
library(data.table)
library(lubridate)
library(MatchIt)
library(lme4)
library(cobalt)




#### 1. READ DATA ####
df_Case = fread("../Data_clean/diseases/timeseri_all.csv")


df_County = fread("../../Posted/Datasets/Derived/rain_50mm_events.csv")
df_County$Year = year(df_County$start_date)
df_County = df_County %>% subset(Year>=2018)
df_County = df_County %>% subset(PAC %in% df_Case$county)

df_County_time = fread("../../Posted/Datasets/Derived/Rainfall_50mm_2015to2024.csv")
df_County_time = df_County_time %>% subset(year(date)>=2018)
names(df_County_time)[3] = "HW925.3"
names(df_County_time)[1] = "PAC"
df_County_time = df_County_time %>% subset(HW925.3 == 1)
df_County_time = df_County_time %>% subset(PAC %in% df_Case$county)
df_County_time.all = fread("../../Posted/Datasets/Derived/Rainfall_50mm_2015to2024.csv") %>% subset(year(date)>=2018)
names(df_County_time.all)[3] = "HW925.3"
names(df_County_time.all)[1] = "PAC"
df_County_time.all = df_County_time.all %>% subset(PAC %in% df_Case$county)

windspeed <- fread("../Data_clean/covs/WindSpeed_daily_2015_2024.csv")
rh <- fread("../Data_clean/covs/RH_daily_2015_2024.csv")
sexratio <- fread("../Data_clean/covs/Population_SexRatio.csv")
popdensity <- fread("../Data_clean/covs/Population_Density.csv")
popageing <- fread("../Data_clean/covs/Population_AgingRate.csv")
pm25 <- fread("../Data_clean/covs/PM25_daily_2015_2024.csv")
hosbed <- fread("../Data_clean/covs/Hospital_Bed.csv")
gdp <- fread("../Data_clean/covs/GDP_2005_2024_prediction.csv"); gdp <- gdp[,-2]
education <- fread("../Data_clean/covs/Education.csv")


windspeed = windspeed %>% subset(County %in% unique(df_Case$county))
rh = rh %>% subset(County %in% unique(df_Case$county))
pm25 = pm25 %>% subset(County %in% unique(df_Case$county))

covs.time.day = windspeed %>% 
  merge(rh,by=c("County","date"),all.x = T) %>%
  merge(pm25,by=c("County","date"),all.x = T)

cov.time.year = sexratio %>% 
  merge(popdensity,by=c("County","Year"),all.x = T) %>% 
  merge(popageing,by=c("County","Year"),all.x = T) %>% 
  merge(hosbed,by=c("County","Year"),all.x = T) %>%
  merge(gdp,by=c("County","Year"),all.x = T)
cov.time.year = cov.time.year %>% subset(County %in% unique(df_Case$county))
cov.time.year$gdp_interp = cov.time.year$gdp_interp / cov.time.year$Population
cov.time.year$Bed.Num = cov.time.year$Bed.Num / cov.time.year$Population

cov.time.cont = education 
names(cov.time.cont) = c("County","Education")

covs.time.day = covs.time.day %>% subset(year(date) >= 2018)
cov.time.year = cov.time.year %>% subset(Year >= 2018)
cov.time.year = cov.time.year[,-c(4,5)]


### ..adjacency mat
Adjmat <- fread("../Data_clean/Adjacency_matrix_long.csv")
Adjmat <- Adjmat %>% subset(row %in% df_County$PAC)


### ..climate zone
zone <- openxlsx::read.xlsx("../../../Dataset/zone/county/county.xlsx")
zone <- zone %>% subset(County %in% hosdata$id_hosp)


#### 2.PSM DID ####
### .. 
### .. warning: [-6, -4] vs. [-3, -1]
### .. during: [-10, -4] vs.  [0, t-1]
### .. week1: [-10, -4] vs.  [t, (t+7)-1]
### .. week2: [-10, -4] vs.  [t+7, (t+14)-1]
### .. week3: [-10, -4] vs.  [t+14, (t+21)-1]
library(cli)
cli_progress_bar('Processing O3 dataset', total=nrow(df_County), type='tasks',
                 format="{cli::pb_bar} {pb_percent} @ {cli::pb_current}:{cli::pb_total} ETA: {cli::pb_eta}")


##### 2.1 P1 #####
save = data.frame()
myfun = function(i){
  
  CaseCounty = df_County$PAC[i]
  EventSatrt = df_County$start_date[i]
  EventEnd = df_County$end_date[i]
  EventDays = df_County$duration[i]
  EventYear = year(EventSatrt)
  
  
  if(length(which(df_Case$county==CaseCounty & df_Case$date==EventSatrt)) == 0) return(NULL)
  
 
  pre10 = as.IDate((EventSatrt-10):(EventSatrt-1))
  ss = sum(df_County_time$HW925.3[which(df_County_time$PAC == CaseCounty & df_County_time$date %in% pre10)])
  if(ss == 0) {
    
    ## ..GO
    
    
    noControl = df_County_time[date >= pre10[1] & date <= EventSatrt,
                               .(once = any(HW925.3 == 1)),
                               by = PAC]
    noControl = noControl[once == T, PAC]
    noControl = noControl[which(noControl != CaseCounty)]

    
    ## ..
    AdjmatCounty = Adjmat$col[which(Adjmat$adj==1 & Adjmat$row == CaseCounty)]
    cz = zone$zone[which(zone$County == CaseCounty)]
    thezone = zone$County[which(zone$zone == cz)]
    MICounty = MI0.5[which(MI0.5 != CaseCounty)]
    
    
    ### ..psm 1:4
    PSMdata = df_County_time.all[date == EventSatrt]
    PSMdata$Tag = 0
    PSMdata$Tag = ifelse(PSMdata$HW925.3==1,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% noControl,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% AdjmatCounty,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% thezone,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% MICounty,1,PSMdata$Tag)
    
    PSMdata = PSMdata[,c(1,4)]  #############
    names(PSMdata)[1] = "County"
    PSMdata = left_join(PSMdata,covs.time.day %>% subset(date==EventSatrt),by = "County")
    PSMdata = left_join(PSMdata,cov.time.year %>% subset(Year==EventYear),by = "County")
    PSMdata = left_join(PSMdata,cov.time.cont,by = "County")
    
    therow <- PSMdata[County == CaseCounty]
    PSMdata.temp <- PSMdata[County != CaseCounty]
    PSMdata <- rbind(therow, PSMdata)
    PSMdata$Education[is.na(PSMdata$Education)] = median(PSMdata$Education,na.rm = T)
    
    tryCatch({
      m <- matchit(Tag ~ Wind + RH + SexRatio + Pop_Density + AgingRate +
                     PM25 + Education + gdp_interp + Bed.Num,
                   data=PSMdata,
                   distance='logit',
                   method='nearest',
                   replace=TRUE,
                   ratio=4)
      s.out <- summary(m, standardize = TRUE)
      out <- list()
      out[[1]] <- s.out[["sum.all"]]
      out[[2]] <- s.out[["sum.matched"]]
      openxlsx::write.xlsx(out,paste0("../Text_and_figures/Supp/Rainfall-PSM-NEW2-P1/",i,".xlsx"),rowNames=T)
      
      
      Group <- c("1", m$match.matrix[1,]) %>% as.numeric
      Group <- PSMdata$County[Group]      
      
      ### .. psm-did
      pre_date <- seq.Date(EventSatrt-10, EventSatrt-4, by = "day")
      post_date <- seq.Date(EventSatrt-3, EventSatrt-1, by = "day")
      
      PSMDID <- df_Case[date %in% c(pre_date, post_date) & (county %in% Group)]
      PSMDID$caseCounty <- ifelse(PSMDID$county == CaseCounty, 1, 0)
      PSMDID$caseTime <- ifelse(PSMDID$date %in% post_date, 1, 0)
      PSMDID$Loop = i
      return(PSMDID)
    },
    error = function(e) return(NULL)
    )
    
  }
  #cli_progress_update()
}
result = lapply(1:nrow(df_County), myfun)
rr = do.call(rbind,result)
rr$Year = year(rr$date)
names(rr)[1] = "County"
pp = popdensity %>% subset(select = c("Year","County","Population"))
rr1 = left_join(rr,pp,by = c("County","Year"))
fwrite(rr1,"../Data_clean/diseases/DID-rainfall-NEW2/PSM-DID-P1.csv")




##### 2.2 P2 #####
save = data.frame()
myfun = function(i){
  
  CaseCounty = df_County$PAC[i]
  EventSatrt = df_County$start_date[i]
  EventEnd = df_County$end_date[i]
  EventDays = df_County$duration[i]
  EventYear = year(EventSatrt)
  
  
  if(length(which(df_Case$county==CaseCounty & df_Case$date==EventSatrt)) == 0) return(NULL)
  

  pre10 = as.IDate((EventSatrt-10):(EventSatrt-1))
  ss = sum(df_County_time$HW925.3[which(df_County_time$PAC == CaseCounty & df_County_time$date %in% pre10)])
  if(ss == 0) {
    
    ## ..GO

    
    noControl = df_County_time[date >= pre10[1] & date <= as.IDate(EventSatrt+EventDays-1),
                               .(once = any(HW925.3 == 1)),
                               by = PAC]
    noControl = noControl[once == T, PAC]
    noControl = noControl[which(noControl != CaseCounty)]
 
    ## ..
    AdjmatCounty = Adjmat$col[which(Adjmat$adj==1 & Adjmat$row == CaseCounty)]
    cz = zone$zone[which(zone$County == CaseCounty)]
    thezone = zone$County[which(zone$zone == cz)]
    MICounty = MI0.5[which(MI0.5 != CaseCounty)]
    
    
    ### ..psm 1:4
    PSMdata = df_County_time.all[date == EventSatrt]
    PSMdata$Tag = 0
    PSMdata$Tag = ifelse(PSMdata$HW925.3==1,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% noControl,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% AdjmatCounty,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% thezone,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% MICounty,1,PSMdata$Tag)
    PSMdata = PSMdata[,c(1,4)] 
    names(PSMdata)[1] = "County"
    PSMdata = left_join(PSMdata,covs.time.day %>% subset(date==EventSatrt),by = "County")
    PSMdata = left_join(PSMdata,cov.time.year %>% subset(Year==EventYear),by = "County")
    PSMdata = left_join(PSMdata,cov.time.cont,by = "County")
    
    therow <- PSMdata[County == CaseCounty]
    PSMdata.temp <- PSMdata[County != CaseCounty]
    PSMdata <- rbind(therow, PSMdata)
    PSMdata$Education[is.na(PSMdata$Education)] = median(PSMdata$Education,na.rm = T)
    
    tryCatch({
      m <- matchit(Tag ~ Wind + RH + SexRatio + Pop_Density + AgingRate +
                     PM25 + Education + gdp_interp + Bed.Num,
                   data=PSMdata,
                   distance='logit',
                   method='nearest',
                   replace=TRUE,
                   ratio=4)
      s.out <- summary(m, standardize = TRUE)
      out <- list()
      out[[1]] <- s.out[["sum.all"]]
      out[[2]] <- s.out[["sum.matched"]]
      openxlsx::write.xlsx(out,paste0("../Text_and_figures/Supp/Rainfall-PSM-NEW2-P2/",i,".xlsx"),rowNames=T)
      
      
      Group <- c("1", m$match.matrix[1,]) %>% as.numeric
      Group <- PSMdata$County[Group]
      
      ### .. 
      pre_date <- seq.Date(EventSatrt-10, EventSatrt-4, by = "day")
      post_date <- seq.Date(EventSatrt, EventSatrt+EventDays-1, by = "day")
      
      PSMDID <- df_Case[date %in% c(pre_date, post_date) & (county %in% Group)]
      PSMDID$caseCounty <- ifelse(PSMDID$county == CaseCounty, 1, 0)
      PSMDID$caseTime <- ifelse(PSMDID$date %in% post_date, 1, 0)
      PSMDID$Loop = i
      return(PSMDID)
    },
    error = function(e) return(NULL)
    )
    
  }
  #cli_progress_update()
}
result = lapply(1:nrow(df_County), myfun)
rr = do.call(rbind,result)
rr$Year = year(rr$date)
names(rr)[1] = "County"
pp = popdensity %>% subset(select = c("Year","County","Population"))
rr2 = left_join(rr,pp,by = c("County","Year"))
fwrite(rr2,"../Data_clean/diseases/DID-rainfall-NEW2/PSM-DID-P2.csv")




##### 2.3 P3 #####
save = data.frame()
myfun = function(i){
  
  CaseCounty = df_County$PAC[i]
  EventSatrt = df_County$start_date[i]
  EventEnd = df_County$end_date[i]
  EventDays = df_County$duration[i]
  EventYear = year(EventSatrt)
  

  if(length(which(df_Case$county==CaseCounty & df_Case$date==EventSatrt)) == 0) return(NULL)
  
  pre10 = as.IDate((EventSatrt-10):(EventSatrt-1))
  ss = sum(df_County_time$HW925.3[which(df_County_time$PAC == CaseCounty & df_County_time$date %in% pre10)])
  if(ss == 0) {
    
    ## ..GO
    
    noControl = df_County_time[date >= pre10[1] & date <= as.IDate(EventSatrt+EventDays+7-1),
                               .(once = any(HW925.3 == 1)),
                               by = PAC]
    noControl = noControl[once == T, PAC]
    noControl = noControl[which(noControl != CaseCounty)]
 
    ## ..
    AdjmatCounty = Adjmat$col[which(Adjmat$adj==1 & Adjmat$row == CaseCounty)]
    cz = zone$zone[which(zone$County == CaseCounty)]
    thezone = zone$County[which(zone$zone == cz)]
    MICounty = MI0.5[which(MI0.5 != CaseCounty)]
    
    
    ### ..psm 1:4
    PSMdata = df_County_time.all[date == EventSatrt]
    PSMdata$Tag = 0
    PSMdata$Tag = ifelse(PSMdata$HW925.3==1,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% noControl,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% AdjmatCounty,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% thezone,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% MICounty,1,PSMdata$Tag)
    PSMdata = PSMdata[,c(1,4)] 
    names(PSMdata)[1] = "County"
    PSMdata = left_join(PSMdata,covs.time.day %>% subset(date==EventSatrt),by = "County")
    PSMdata = left_join(PSMdata,cov.time.year %>% subset(Year==EventYear),by = "County")
    PSMdata = left_join(PSMdata,cov.time.cont,by = "County")
    
    therow <- PSMdata[County == CaseCounty]
    PSMdata.temp <- PSMdata[County != CaseCounty]
    PSMdata <- rbind(therow, PSMdata)
    PSMdata$Education[is.na(PSMdata$Education)] = median(PSMdata$Education,na.rm = T)
    
    tryCatch({
      m <- matchit(Tag ~ Wind + RH + SexRatio + Pop_Density + AgingRate +
                     PM25 + Education + gdp_interp + Bed.Num,
                   data=PSMdata,
                   distance='logit',
                   method='nearest',
                   replace=TRUE,
                   ratio=4)
      s.out <- summary(m, standardize = TRUE)
      out <- list()
      out[[1]] <- s.out[["sum.all"]]
      out[[2]] <- s.out[["sum.matched"]]
      openxlsx::write.xlsx(out,paste0("../Text_and_figures/Supp/Rainfall-PSM-NEW2-P3/",i,".xlsx"),rowNames=T)
      
      
      Group <- c("1", m$match.matrix[1,]) %>% as.numeric
      Group <- PSMdata$County[Group]  
      
      ### .. psm-did
      pre_date <- seq.Date(EventSatrt-10, EventSatrt-4, by = "day")
      post_date <- seq.Date(EventSatrt+EventDays, EventSatrt+EventDays+7-1, by = "day")
      
      PSMDID <- df_Case[date %in% c(pre_date, post_date) & (county %in% Group)]
      PSMDID$caseCounty <- ifelse(PSMDID$county == CaseCounty, 1, 0)
      PSMDID$caseTime <- ifelse(PSMDID$date %in% post_date, 1, 0)
      PSMDID$Loop = i
      return(PSMDID)
    },
    error = function(e) return(NULL)
    )
    
  }
  #cli_progress_update()
}
result = lapply(1:nrow(df_County), myfun)
rr = do.call(rbind,result)
rr$Year = year(rr$date)
names(rr)[1] = "County"
pp = popdensity %>% subset(select = c("Year","County","Population"))
rr3 = left_join(rr,pp,by = c("County","Year"))
fwrite(rr3,"../Data_clean/diseases/DID-rainfall-NEW2/PSM-DID-P3.csv")


##### 2.3 P4 #####
save = data.frame()
myfun = function(i){
  
  CaseCounty = df_County$PAC[i]
  EventSatrt = df_County$start_date[i]
  EventEnd = df_County$end_date[i]
  EventDays = df_County$duration[i]
  EventYear = year(EventSatrt)
  

  if(length(which(df_Case$county==CaseCounty & df_Case$date==EventSatrt)) == 0) return(NULL)
  
  pre10 = as.IDate((EventSatrt-10):(EventSatrt-1))
  ss = sum(df_County_time$HW925.3[which(df_County_time$PAC == CaseCounty & df_County_time$date %in% pre10)])
  if(ss == 0) {
    
    ## ..GO
    
    
    noControl = df_County_time[date >= pre10[1] & date <= as.IDate(EventSatrt+EventDays+7+7-1),
                               .(once = any(HW925.3 == 1)),
                               by = PAC]
    noControl = noControl[once == T, PAC]
    noControl = noControl[which(noControl != CaseCounty)]
    

    ## ..
    AdjmatCounty = Adjmat$col[which(Adjmat$adj==1 & Adjmat$row == CaseCounty)]
    cz = zone$zone[which(zone$County == CaseCounty)]
    thezone = zone$County[which(zone$zone == cz)]
    MICounty = MI0.5[which(MI0.5 != CaseCounty)]
    
    
    ### ..psm 1:4
    PSMdata = df_County_time.all[date == EventSatrt]
    PSMdata$Tag = 0
    PSMdata$Tag = ifelse(PSMdata$HW925.3==1,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% noControl,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% AdjmatCounty,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% thezone,1,PSMdata$Tag)
    PSMdata$Tag = ifelse(PSMdata$PAC %in% MICounty,1,PSMdata$Tag)
    PSMdata = PSMdata[,c(1,4)] 
    names(PSMdata)[1] = "County"
    PSMdata = left_join(PSMdata,covs.time.day %>% subset(date==EventSatrt),by = "County")
    PSMdata = left_join(PSMdata,cov.time.year %>% subset(Year==EventYear),by = "County")
    PSMdata = left_join(PSMdata,cov.time.cont,by = "County")
    
    therow <- PSMdata[County == CaseCounty]
    PSMdata.temp <- PSMdata[County != CaseCounty]
    PSMdata <- rbind(therow, PSMdata)
    PSMdata$Education[is.na(PSMdata$Education)] = median(PSMdata$Education,na.rm = T)
    
    tryCatch({
      m <- matchit(Tag ~ Wind + RH + SexRatio + Pop_Density + AgingRate +
                     PM25 + Education + gdp_interp + Bed.Num,
                   data=PSMdata,
                   distance='logit',
                   method='nearest',
                   replace=TRUE,
                   ratio=4)
      s.out <- summary(m, standardize = TRUE)
      out <- list()
      out[[1]] <- s.out[["sum.all"]]
      out[[2]] <- s.out[["sum.matched"]]
      openxlsx::write.xlsx(out,paste0("../Text_and_figures/Supp/Rainfall-PSM-NEW2-P4/",i,".xlsx"),rowNames=T)
      
      
      Group <- c("1", m$match.matrix[1,]) %>% as.numeric
      Group <- PSMdata$County[Group] 
      
      ### .. psm-did
      pre_date <- seq.Date(EventSatrt-10, EventSatrt-4, by = "day")
      post_date <- seq.Date(EventSatrt+EventDays+7, EventSatrt+EventDays+7+7-1, by = "day")
      
      PSMDID <- df_Case[date %in% c(pre_date, post_date) & (county %in% Group)]
      PSMDID$caseCounty <- ifelse(PSMDID$county == CaseCounty, 1, 0)
      PSMDID$caseTime <- ifelse(PSMDID$date %in% post_date, 1, 0)
      PSMDID$Loop = i
      return(PSMDID)
    },
    error = function(e) return(NULL)
    )
    
  }
  #cli_progress_update()
}
result = lapply(1:nrow(df_County), myfun)
rr = do.call(rbind,result)
rr$Year = year(rr$date)
names(rr)[1] = "County"
pp = popdensity %>% subset(select = c("Year","County","Population"))
rr4 = left_join(rr,pp,by = c("County","Year"))
fwrite(rr4,"../Data_clean/diseases/DID-rainfall-NEW2/PSM-DID-P4.csv")








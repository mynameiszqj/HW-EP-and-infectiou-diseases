library(data.table)
library(dplyr)
library(glmmTMB)
library(fixest)
library(openxlsx)



#### 0.base data ####

### ...
case.all = fread("../Data_clean/diseases/newclass2/timeseri_all.csv")
case.19 = fread("../Data_clean/diseases/newclass2/timeseri_age_19.csv")
case.20_44 = fread("../Data_clean/diseases/newclass2/timeseri_age_20_44.csv")
case.45_64 = fread("../Data_clean/diseases/newclass2/timeseri_age_45_64.csv")
case.65 = fread("../Data_clean/diseases/newclass2/timeseri_age_65.csv")
case.female = fread("../Data_clean/diseases/newclass2/timeseri_femlae.csv")
case.male = fread("../Data_clean/diseases/newclass2/timeseri_male.csv")
case.out = fread("../Data_clean/diseases/newclass2/timeseri_outpatient.csv")
case.ed = fread("../Data_clean/diseases/newclass2/timeseri_ED.csv")
case.other = fread("../Data_clean/diseases/newclass2/timeseri_tranother.csv")

case = case.all %>% 
  merge(case.19, by = c("county","date")) %>% 
  merge(case.20_44, by = c("county","date")) %>% 
  merge(case.45_64, by = c("county","date")) %>% 
  merge(case.65, by = c("county","date")) %>% 
  merge(case.female, by = c("county","date")) %>% 
  merge(case.male, by = c("county","date")) %>% 
  merge(case.out, by = c("county","date")) %>% 
  merge(case.ed, by = c("county","date")) %>% 
  merge(case.other, by = c("county","date"))

case$Year = lubridate::year(case$date)

### ..
population = fread("../Data_clean/covs/Population_both_2015_2024.csv")
population.male = fread("../Data_clean/covs/Population_male_2015_2024.csv")

population.19 =  population %>% 
  subset(Agegroup %in% c("0 to 12 months","1 to 4 years","5 to 9 years","10 to 14 years","15 to 19 years")) %>% 
  group_by(Year,County) %>% summarise(population19 = sum(Population))
population.20.44 =  population %>% 
  subset(Agegroup %in% c("20 to 24 years","25 to 29 years","30 to 34 years","35 to 39 years","40 to 44 years" )) %>% 
  group_by(Year,County) %>% summarise(population20_44 = sum(Population))
population.45.64 =  population %>% 
  subset(Agegroup %in% c("45 to 49 years","50 to 54 years","55 to 59 years","60 to 64 years")) %>% 
  group_by(Year,County) %>% summarise(population45_64 = sum(Population))
population = population %>% group_by(Year,County) %>% summarise(allpopulation = sum(Population))
population.male = population.male %>% group_by(Year,County) %>% summarise(malepopulation = sum(Population))
population = merge(population,population.male,by = c("Year","County"))
population$femalepopulation = population$allpopulation - population$malepopulation
population = merge(population,population.19,by = c("Year","County"))
population = merge(population,population.20.44,by = c("Year","County"))
population = merge(population,population.45.64,by = c("Year","County"))
population$population65 = population$allpopulation - population$population19 - population$population20_44 - population$population45_64
names(population)[2] = "county"

### ..linking pop and diseases
case = merge(case,population,by=c("Year","county"), all.x = T)

names(population)[2] = "County"
case.all = case[,c(1:19)]
names(case.all)[2] = "County"
case.all = case.all[,-1]

#### 1.PSM DID Data ####
rr1 = fread("../Data_clean/diseases/DID-rainfall-NEW2/PSM-DID-P1.csv")[,c(1,2,9:12)]
rr2 = fread("../Data_clean/diseases/DID-rainfall-NEW2/PSM-DID-P2.csv")[,c(1,2,9:12)]
rr3 = fread("../Data_clean/diseases/DID-rainfall-NEW2/PSM-DID-P3.csv")[,c(1,2,9:12)]
rr4 = fread("../Data_clean/diseases/DID-rainfall-NEW2/PSM-DID-P4.csv")[,c(1,2,9:12)]

rr1 = rr1 %>% merge(case.all,by = c("County","date")) %>% merge(population,by = c("County","Year"))
rr2 = rr2 %>% merge(case.all,by = c("County","date")) %>% merge(population,by = c("County","Year"))
rr3 = rr3 %>% merge(case.all,by = c("County","date")) %>% merge(population,by = c("County","Year"))
rr4 = rr4 %>% merge(case.all,by = c("County","date")) %>% merge(population,by = c("County","Year"))


estPrint = function(data, g, period){
  lst = list()
  
  if(g %in% c("All","inte","resp","vect_zoon","skin_mucous","Others")) p = "allpopulation"
  if(g %in% c("Male")) p = "malepopulation"
  if(g %in% c("Female")) p = "femalepopulation"
  if(g %in% c("age_19")) p = "population19"
  if(g %in% c("age_20_44")) p = "population20_44"
  if(g %in% c("age_45_64")) p = "population45_64"
  if(g %in% c("age_65")) p = "population65"
  
  eval(parse(text = paste("M = glmmTMB(",g," ~ caseCounty*caseTime + (1|date) + (1|County) + offset(log(",p,")), data = data, ziformula = ~ 1, family = nbinom2)", sep = "")))


  ### extrac
  summ.M = summary(M)
  summ.M = summ.M[["coefficients"]]$cond %>% as.data.frame
 

  summ.M$out = paste0(round(exp(summ.M$Estimate),2)," (",round(exp(summ.M$Estimate - 1.96*summ.M$`Std. Error`),2),", ",round(exp(summ.M$Estimate + 1.96*summ.M$`Std. Error`),2),")")
  summ.M$out2 = paste0(round(100*(exp(summ.M$Estimate)-1),2)," (",round(100*(exp(summ.M$Estimate - 1.96*summ.M$`Std. Error`)-1),2),", ",round(100*(exp(summ.M$Estimate + 1.96*summ.M$`Std. Error`)-1),2),")")
  summ.M$period = period

  
  lst[["M"]]=summ.M
  return(lst)
}

### ..loop
group = names(rr1)[7:18]

for (i in 1:length(group)) {
  subgroup = group[i]
  savelst = list()
  
  for (j in 1:4) {
    print(paste0(subgroup,"...P",j))
    if(j==1) {df = rr1; period = "P1"; resutlst.p1 = estPrint(df,subgroup,period)}
    if(j==2) {df = rr2; period = "P2"; resutlst.p2 = estPrint(df,subgroup,period)}
    if(j==3) {df = rr3; period = "P3"; resutlst.p3 = estPrint(df,subgroup,period)}
    if(j==4) {df = rr4; period = "P4"; resutlst.p4 = estPrint(df,subgroup,period)}
  }
  ll = list()
  
  ll[[1]] = rbind(resutlst.p1[[1]],resutlst.p2[[1]],resutlst.p3[[1]],resutlst.p4[[1]])

  
  savelst[["M"]] = ll[[1]]

  write.xlsx(savelst,paste0("../Text_and_figures/Main/new2/1.Association/Rainfall/",subgroup,".xlsx"),rowNames  = T,overwrite = T)
}


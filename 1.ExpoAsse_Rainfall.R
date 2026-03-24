library(openxlsx)
library(dplyr)
library(lubridate)
library(data.table)

#### 99.5 per ####
df <- rbind(
  fread("/mnt/nas2/Dataset/CMA/tp/county/1996.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/1997.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/1998.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/1999.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2000.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2001.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2002.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2003.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2004.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2005.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2006.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2007.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2008.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2009.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2010.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2011.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2012.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2013.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2014.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2015.csv"))

df <- na.omit(df)
setDT(df)

df <- df[,list(p995 = quantile(value, 0.995)), by = list(county.code)]

#### definiton ####

### precipitation
all <- rbind(
  fread("/mnt/nas2/Dataset/CMA/tp/county/2015.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2016.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2017.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2018.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2019.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2020.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2021.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2022.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2023.csv"),
  fread("/mnt/nas2/Dataset/CMA/tp/county/2024.csv")
)
all <- na.omit(all)


### snow
snow <- rbind(
  fread("/mnt/nas2/Dataset/China_snow/county/2015.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2016.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2017.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2018.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2019.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2020.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2021.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2022.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2023.csv"),
  fread("/mnt/nas2/Dataset/China_snow/county/2024.csv")
)
snow <- na.omit(snow)
names(snow)[3] <- "snow"


### temperature
temp <- rbind(fread("/mnt/nas2/Dataset/CMA/tm/county/2015.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2016.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2017.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2018.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2019.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2020.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2021.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2022.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2023.csv"),
              fread("/mnt/nas2/Dataset/CMA/tm/county/2024.csv"))
temp <- na.omit(temp)
names(temp)[3] <- "temp"




#### 99.5 definition ####
all <- left_join(all, df, by = "county.code")
all$rainfall <- ifelse(all$value>all$p995,1,0)
all <- all %>% subset(county.code %in% c(210803,460300,810000,820000) == F)
temp <- temp %>% subset(county.code %in% c(210803,460300) == F)

temp2 <- temp %>% subset(county.code == 710024)
temp2 <- temp2 %>% group_by(county.code, date) %>% summarise(temp = mean(temp))
temp <- temp %>% subset(county.code %in% 710024 == F)
temp <- rbind(temp, temp2)

all.2 <- merge(all, temp, by = c("county.code","date"),all.x = T)

snow2 <- snow %>% subset(county.code == 710024)
snow2 <- snow2 %>% group_by(county.code, date) %>% summarise(snow = mean(snow))
snow <- snow %>% subset(county.code %in% 710024 == F)
snow <- rbind(snow, snow2)

all.3 <- merge(all.2, snow, by = c("county.code","date"),all.x = T)

head(all.3)
table(all.3$rainfall)
all.3$rainfall <- ifelse(all.3$snow>0 & all.3$temp<0, 0, all.3$rainfall)
all.4 <- all.3[,-c(3,4,6,7)]

fwrite(all.4,"../../Posted/Datasets/Derived/Rainfall_995_2015to2024.csv")


#### 50mm definition ####
all$rainfall <- ifelse(all$value>50,1,0)
all.2 <- merge(all, temp, by = c("county.code","date"),all.x = T)
all.3 <- merge(all.2, snow, by = c("county.code","date"),all.x = T)

head(all.3)
table(all.3$rainfall)
all.3$rainfall <- ifelse(all.3$snow>0 & all.3$temp<0, 0, all.3$rainfall)
all.4 <- all.3[,-c(3,4,6,7)]

fwrite(all.4,"../../Posted/Datasets/Derived/Rainfall_50mm_2015to2024.csv")
aa
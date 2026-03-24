
library(openxlsx)
library(dplyr)
library(lubridate)
library(data.table)

### ..HeatWave definition


## ..From:futureheatwaves::IDHeatwavesAlternative

findhw <- function (threshold, data, numDays) 
{
  days <- numDays
  datafr <- data.frame(data[,c("date", "tm")])
  tempsExceedingthreshold <- as.numeric(datafr[, 2] >= threshold)
  tempsExceedingthreshold <- c(tempsExceedingthreshold, 0)
  heatwaveForm <- rep(1, days)
  counter <- 1
  hwBound <- days - 1
  hwInfo <- c()
  ii <- 1
  while (ii <= length(datafr[, 2])) {
    if (identical(tempsExceedingthreshold[ii:(ii + hwBound)], 
                  heatwaveForm)) {
      size <- match(0, tempsExceedingthreshold[-(1:ii)])
      hwInfo <- c(hwInfo, rep(1, size))
      counter <- counter + 1
      ii <- ii + size
    }
    else {
      hwInfo <- c(hwInfo, 0)
      ii <- ii + 1
    }
  }
  return(hwInfo)
}




#### 0.read data ####
## ..Data source: CMA
data_climate <- rbind(fread("/mnt/nas2/Dataset/CMA/tmmax/county/1996.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/1997.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/1998.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/1999.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2000.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2001.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2002.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2003.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2004.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2005.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2006.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2007.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2008.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2009.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2010.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2011.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2012.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2013.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2014.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2015.csv"))
head(data_climate)
# county.code       date     value
#         <int>   <IDat>    <num>
# 1:      110101 1996-01-01  5.01
# 2:      110102 1996-01-01  5.39
# 3:      110105 1996-01-01  5.00
# 4:      110106 1996-01-01  5.25
# 5:      110107 1996-01-01  5.54
# 6:      110108 1996-01-01  5.53

data_climate <- na.omit(data_climate)

data_climate$date %>% summary
data_climate$county.code %>% unique %>% length                                  
names(data_climate)[3] <- "tmax"



#### 1.threshold definition ####

setDT(data_climate)
data <- data_climate[ ,list(tmax925=quantile(tmax,0.925)),by = list(county.code)]
names(data)[1] <- "PAC"

#### 2.read 2015-2024 ####
data_climate <- rbind(fread("/mnt/nas2/Dataset/CMA/tmmax/county/2015.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2016.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2017.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2018.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2019.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2020.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2021.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2022.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2023.csv"),
                      fread("/mnt/nas2/Dataset/CMA/tmmax/county/2024.csv"))
data_climate <- na.omit(data_climate)



#### 3.HW definition ####
code <- unique(data_climate$county.code)


### ..Relative 1.threshold

HWdata <- list()
for (i in 1:length(code)) {
  print(paste("Definding HW in ",i,"-",code[i]," (",i,"/",length(code),")",sep = ""))
  sub <- subset(data_climate,county.code==code[i])
  names(sub) <- c("PAC","date","tm")
  thr <- subset(data,PAC==code[i])
  
  #order
  sub <- sub[order(sub$date),]
  
  #defind HW
  # sub$HW925.2 <- findhw(data = sub,threshold = thr$tmax925[1],numDays = 2)  ## 92.5百分位数为阈值,持续>=2days
  sub$HW925.3 <- findhw(data = sub,threshold = 35,numDays = 3)  ## 35摄氏度为绝对阈值,持续>=3days
  # sub$HW925.4 <- findhw(data = sub,threshold = thr$tmax925[1],numDays = 4)
  
  #combind
  HWdata[[i]] <- sub
}

HWdata <- do.call("rbind",HWdata)
names(HWdata)[3] <- "tmax"
fwrite(HWdata,"../../Posted/Datasets/Derived/Heatwave_35degree_2015to2024.csv")


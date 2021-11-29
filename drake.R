
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(tidyr)
library(dplyr)   # needs to come after MASS above so select() isn't masked
library(lubridate)
library(kableExtra)
library(piecewiseSEM)
library(drake)  # 6.1.0
library(ggmap)
library(maps)
library(mapdata)
# install.packages("read_excel")
library(readxl)

OUTPUT_DIR		<- "outputs"
DATA_DIR <- 'data'

#*****************************************************************************************************************
# functions 
#*****************************************************************************************************************
'%!in%' <- function(x,y)!('%in%'(x,y))
# input csv data
read_file <- function(x) read.csv(file.path(DATA_DIR, x), comment.char = "#", stringsAsFactors = FALSE)
# input xlsx data
read_xlsx <- function(x, n_sheet, n_skip) read_excel(file.path(DATA_DIR, x), sheet = n_sheet, skip = n_skip)
# write csv 
writ_file <- function(input, output) write.csv(input, file.path(OUTPUT_DIR, output), row.names = FALSE)
# clean SEDB
clean_sedb <- function() {
  # sdata <- read_xlsx('SoilErosionDB_v2.xlsx', n_sheet = 1, n_skip = 0)
  sdata <- read.csv("data/SoilErosionDB_v2-2.csv")
  sdata$Study_midyear <- ifelse(!is.na(sdata$Study_midyear), sdata$Study_midyear, sdata$Paper_year - 12)
  sdata$Study_midyear <- floor(sdata$Study_midyear)
  return(sdata)
}

# get tm and pm from the global climate data (del)
# i = 1
# sdata = test_data
get_del_climate <- function(sdata) {
  tm_location <- "F:/My Drive/PNNL/data/bigdata/UDel/Global2011T_XLS"
  pm_location <- "F:/My Drive/PNNL/data/bigdata/UDel/Global2011P_XLS"
  for (i in 1:nrow(sdata)) {
    # for (i in 91:150) {
    target_year <- sdata$Study_midyear[i]
    target_lat <- sdata$Latitude[i]
    target_lon <- sdata$Longitude[i]
    
    if(is.na(target_year) | is.na(target_lat) | is.na(target_lon)) {next}
    else{
      # find target years temperature data
      tm_name <- paste0(tm_location, "/", "air_temp.", target_year, ".txt")
      del_tm <- read.table(tm_name)
      colnames(del_tm)[1:14] <- c("lon", "lat", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12")
      # precipitation
      pm_name <- paste0(pm_location, "/", "precip.", target_year, ".txt")
      del_pm <- read.table(pm_name)
      colnames(del_pm)[1:14] <- c("lon", "lat", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12")
      # calculate annual mean temperature
      del_tm %>% 
        mutate(MAT = (M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12)/12) ->
        del_tm
      del_pm %>% 
        mutate(MAP = (M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12)) ->
        del_pm
      # find data for the target
      ilat <- del_tm[which.min(abs(del_tm$lat - target_lat)), ]$lat
      ilon <- del_tm[which.min(abs(del_tm$lon - target_lon)), ]$lon
      
      del_tm %>% 
        filter(lat == ilat & lon == ilon) %>% 
        dplyr::select(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, MAT) ->
        del_mat
      # precipitation
      del_pm %>% 
        filter(lat == ilat & lon == ilon) %>% 
        dplyr::select(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, MAP) ->
        del_map
      
      # get tm data from UDel for SEDB
      if(nrow(del_mat) == 0) {next}
      else{
        sdata[i, "Tannual_del"] <- del_mat$MAT # get annual temperature
      }
      
      # get pm data from UDel for SEDB
      if(nrow(del_map) == 0) {next}
      else{
        sdata[i, "Pannual_del"] <- del_map$MAP # get annual precip
      }
      if (i %in% seq(0, 6000, 100)) 
        {print(paste0("***** ", sdata$Study_number[i],'*****', i))}
    }
  }
  return(sdata)
}

#*****************************************************************************************************************
# make a drake plan 
#*****************************************************************************************************************
plan = drake_plan(
  # load data
  wos_summary = read_xlsx('Number_Studies_byYear.xlsx', n_sheet = 2, n_skip = 1),
  SEDB = clean_sedb(),
  counties = map_data("world", region = ".", exact = FALSE),
  GlobalMATMAP = read.csv('F:/My Drive/PNNL/SRDBV5/data/summarized_climate.csv', comment.char = "#"),
  IGBP_MODIS = read.csv('F:/My Drive/PNNL/SRPartitioning/Data/IGBP_Koppen_MODIS.csv') %>% dplyr::select(-warner_rs),
  
  # link with external data
  SEDB_del = get_del_climate(SEDB)
)

make(plan)
# clean(plan)

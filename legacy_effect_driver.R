#driver script
#load packages
library(dplyr)
library(spdep)
library(splitstackshape)
library(raster)
library(gstat)

#load initial dataframe

test_wd<-"G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Processing NPP Data/NPP Data processing"
rangeland_npp_covariates<-readRDS(file.path(test_wd, "npp_climate_rangelands_final.rds")) #loads file and name it annualSWA_OctDec I guess
as.data.frame(rangeland_npp_covariates)
summary(rangeland_npp_covariates)


#small touch up to get rid of the dash in the 'semi-arid_steppe'
rangeland_npp_covariates$region<-gsub('-', '_', rangeland_npp_covariates$region)
unique(rangeland_npp_covariates$region)

#get mean annual precip
mean_mm_site<-aggregate(mm~x+y+region,mean,data=rangeland_npp_covariates)
head(mean_mm_site)
summary(mean_mm_site)

#make MAP groupings for each vegetation type in preparation for stratification
#hotdeserts
hot_deserts_1 <-subset(mean_mm_site,region=="hot_deserts")
summary(hot_deserts_1)
hot_deserts_below <-hot_deserts_1 %>% dplyr::filter(mm < 286.14)
hot_deserts_below$map <- 'below'
hot_deserts_above  <-hot_deserts_1 %>%  dplyr::filter(mm > 286.14)
hot_deserts_above$map <- 'above'
hot_deserts_above_below <- rbind(hot_deserts_above,hot_deserts_below)
#hot_deserts_above_below_2<-hot_deserts_above_below[-3]
#head(hot_deserts_above_below_2)
#summary(hot_deserts_above_below)

#cold_deserts
cold_deserts_1 <-subset(mean_mm_site,region=="cold_deserts")
summary(cold_deserts_1)
cold_deserts_below <-cold_deserts_1 %>% dplyr::filter(mm < 288.1)
cold_deserts_below$map <- 'below'
cold_deserts_above  <-cold_deserts_1 %>%  dplyr::filter(mm > 288.1 )
cold_deserts_above$map <- 'above'
cold_deserts_above_below <- rbind(cold_deserts_above,cold_deserts_below)

#california_annuals
california_annuals_1 <-subset(mean_mm_site,region=="california_annuals")
summary(california_annuals_1)
california_annuals_below <-california_annuals_1 %>% dplyr::filter(mm < 403.9)
california_annuals_below$map <- 'below'
california_annuals_above  <-california_annuals_1 %>%  dplyr::filter(mm > 403.9)
california_annuals_above$map <- 'above'
california_annuals_above_below <- rbind(california_annuals_above,california_annuals_below)

#shortgrass steppe
semiarid_steppe_1 <-subset(mean_mm_site,region=="semi_arid_steppe")
summary(semiarid_steppe_1)
semiarid_steppe_below <-semiarid_steppe_1 %>% dplyr::filter(mm < 417.1)
semiarid_steppe_below$map <- 'below'
semiarid_steppe_above  <-semiarid_steppe_1 %>%  dplyr::filter(mm > 417.1)
semiarid_steppe_above$map <- 'above'
semiarid_steppe_above_below <- rbind(semiarid_steppe_above,semiarid_steppe_below)

#northern mixed
northern_mixed_prairies_1 <-subset(mean_mm_site,region=="northern_mixed_prairies")
summary(northern_mixed_prairies_1)
northern_mixed_prairies_below <-northern_mixed_prairies_1 %>% dplyr::filter(mm < 404.3)
northern_mixed_prairies_below$map <- 'below'
northern_mixed_prairies_above  <-northern_mixed_prairies_1 %>%  dplyr::filter(mm > 404.3)
northern_mixed_prairies_above$map <- 'above'
northern_mixed_prairies_above_below <- rbind(northern_mixed_prairies_above,northern_mixed_prairies_below)

#making colomn for per-pixel mean precip and npp
rangeland_mean_npp<-aggregate(npp ~ x + y + region,mean,data=rangeland_npp_covariates)
rangeland_mean_mm<-aggregate(mm ~ x + y + region,mean,data=rangeland_npp_covariates)
mm_production_mean<-merge(rangeland_mean_npp,rangeland_mean_mm,by=c('x','y','region'))
head(mm_production_mean)
summary(mm_production_mean)

#merge initial dataframe with means
rangeland_npp_covariates_deviations_1<-merge(rangeland_npp_covariates,mm_production_mean,by=c('x','y'))
head(rangeland_npp_covariates_deviations_1)

#add percent mm and percent npp deviations to relatavize
rangeland_npp_covariates_deviations_1$npp.dev<-rangeland_npp_covariates_deviations_1$npp.x - rangeland_npp_covariates_deviations_1$npp.y
rangeland_npp_covariates_deviations_1$mm.dev<-rangeland_npp_covariates_deviations_1$mm.x - rangeland_npp_covariates_deviations_1$mm.y
summary(rangeland_npp_covariates_deviations_1)
head(rangeland_npp_covariates_deviations_1)

#add a lag term
lag_conus <- rangeland_npp_covariates_deviations_1 %>% group_by(x, y) %>%
  dplyr::mutate(prev = lag(npp.x))

head(lag_conus)

head(sensitivity_conus)
lag_conus_2<- lag_conus[ -c(7:11) ] #isolate coefficient so only slope is graphed
head(lag_conus_2)


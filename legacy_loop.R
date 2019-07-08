#mean npp-map spatial model across veg types
mean_production<-aggregate(npp.x~ x + y,mean,data=rangeland_npp_covariates_deviations_1)
head(mean_production)
list.coefficients.mean<-list()
for(i in 1:1000)
{
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  #test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  #test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  #test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  #test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  #test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    #test.strat.semiarid_steppe, test.strat.hot_deserts)
  
  stratified_final_mean<-merge(test.strat.northern_mixed, lag_conus_2,by=c('x','y'))
  #print(stratified_final)
  head(stratified_final_mean)
  stratified_final_lm_mean_lag<-lm(npp.x~mm.x*prev
                               ,stratified_final_mean)
  AIC(stratified_final_lm_mean_lag)
  summary(stratified_final_lm_mean_lag)
  stratified_final_lm_mean<-lm(npp.x~mm.x
                                   ,stratified_final_mean)
  AIC(stratified_final_lm_mean)
  summary(stratified_final_lm_mean)
  
  newcoef1 <- stratified_final_lm_mean$coefficients 
  df.mean<-data.frame(newcoef1)
  df.mean$id = i
  list.coefficients.mean[[i]] <- data.frame(df.mean)
  
  
}

summary(stratified_final_lm_mean)
df.coefficients.mean <- do.call("rbind", list.coefficients.mean)
head(df.coefficients.mean)
df.coefficients.mean.2 <- cbind(rownames(df.coefficients.mean), data.frame(df.coefficients.mean, row.names=NULL))

colnames(df.coefficients.mean.2)  <- c("predictor","coefficient","run.id")

df.coefficients.mean.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.mean.2$predictor)
df.coefficients.mean.2$predictor<-gsub(':', '_', df.coefficients.mean.2$predictor)
df.coefficients.mean.2$predictor<-gsub('-', '_', df.coefficients.mean.2$predictor)

df2_mean<-reshape(df.coefficients.mean.2, idvar = "run.id", timevar = "predictor", direction = "wide")
head(df2_mean)
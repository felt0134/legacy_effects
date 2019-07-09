#mean npp-map spatial model across veg types

list.aic.lag<-list()
list.aic.nolag<-list()

for(i in 1:1000)
{
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    test.strat.semiarid_steppe, test.strat.hot_deserts)
  stratified_final_mean<-merge(test.strat, lag_conus,by=c('x','y'))
  
  #no lag
  df.nolag<- stratified_final_mean %>% group_by(region) %>%
    dplyr::do(test.lm = lm(npp.std ~ mm.std, data = .)) %>%
    dplyr::mutate(AIC=AIC(test.lm))
    df.nolag_2<-df.nolag[-2]
    list.aic.nolag[[i]]<-data.frame(df.nolag_2)
    
    #lag
    df.lag<- stratified_final_mean %>% group_by(region) %>%
      dplyr::do(test.lm.lag = lm(npp.std ~ mm.std*prev, data = .)) %>%
      dplyr::mutate(AIC=AIC(test.lm.lag)) %>%
      dplyr::mutate(mm.coef=coef(test.lm.lag)[2]) %>%
      dplyr::mutate(prev.coef=coef(test.lm.lag)[3]) %>%
      dplyr::mutate(interaction.coef=coef(test.lm.lag)[4])
      df.lag_2<-df.lag[-2]
      list.aic.lag[[i]]<-data.frame(df.lag_2)

}

  
coef_nolag <- do.call("rbind", list.aic.nolag)
head(coef_nolag)
coef_nolag.2 <- cbind(rownames(coef_nolag), data.frame(coef_nolag, row.names=NULL))
head(coef_nolag.2)
colnames(coef_nolag.2) <- c("id","region","AIC.nolag")
head(coef_nolag.2)

coef_lag <- do.call("rbind", list.aic.lag)
head(coef_lag)
coef_lag.2 <- cbind(rownames(coef_lag), data.frame(coef_lag, row.names=NULL))
head(coef_lag.2)
colnames(coef_lag.2) <- c("id","region","AIC.lag",'mm.coef','prev.year.coef','interaction')
head(coef_lag.2)

merge.lags<-merge(coef_nolag.2,coef_lag.2,by=c('id','region'))
head(merge.lags)

merge.lags$aic.diff <- merge.lags$AIC.lag - merge.lags$AIC.nolag 
hist(merge.lags$prev.year.coef)

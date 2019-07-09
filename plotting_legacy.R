#plotting

veg_names <- c(
  `hot_deserts` = "Hot deserts",
  `cold_deserts` = "Cold deserts",
  'northern_mixed_prairies' = "Northern mixed prairies",
  `semi_arid_steppe` = "Shortgrass steppe",
  `california_annuals` = "California annuals"
)

#previous versus current
ggplot(lag_conus,aes(x=npp.std.prev,y=npp.std,na.rm=TRUE)) +
  geom_point(pch=19,size=0.1) +
  facet_wrap(~region.x,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  stat_smooth(method='lm',color='red') +
  ylab(bquote('Standardized NPP')) +
  labs(x=expression('Standardized NPP'["-1"])) +
  coord_fixed(ratio=.75) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    legend.position = c(0.82,0.95),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#####histograms#########

  ggplot(merge.lags,aes(x=interaction)) +
    geom_histogram(binwidth = .00005,color='black',fill='white') +
    facet_wrap(~region,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
    #scale_fill_manual(values=c('temporal_sensitivity'='red','Spatial'='lightblue'),
                      #labels=c('temporal_sensitivity'='Temporal','Spatial'='Spatial')) +
    #labs(x=expression('Coefficient of standardized NPP'["-1"])) +
    xlab('Interaction of current-year PPT and previous-year NPP') +
    geom_vline(xintercept=0,color='red') +
    ylab('') +
    theme(
      axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
      axis.text.y = element_text(color='black',size=12),
      axis.title = element_text(color='black',size=15),
      axis.ticks = element_line(color='black'),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=17),
      legend.position = c(0.82,0.95),
      strip.background =element_rect(fill="white"),
      strip.text = element_text(size=15),
      panel.background = element_rect(fill=NA),
      panel.border = element_blank(), #make the borders clear in prep for just have two axes
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"))
  
  #map of legacy effects

library(colorspace)
library(latticeExtra)
library(sp)
library(raster)

#shapefile referecne for state outlines
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c('California','New Mexico','Arizona','Utah',
                                        'Arizona','Colorado','Washington','Wyoming',
                                        'Idaho','Oregon','Idaho','Montana','Texas',
                                        'North Dakota','South Dakota','Nebraska',
                                        'Oklahoma','Kansas'),]
  
  legacy_conus <- lag_conus %>% group_by(x, y) %>%
    dplyr::do(model = lm(npp.std~npp.std.prev, data = .)) %>%
    dplyr::mutate(coef=coef(model)[2])
  
  head(lag_conus)
  legacy_conus_coef_only<- legacy_conus[ -c(3) ] #isolate coefficient so only slope is graphed
  head(legacy_conus_coef_only)
  
  
  legacy_raster<-rasterFromXYZ(legacy_conus_coef_only)
  plot(legacy_raster)
  break_legacy<-quantile(legacy_conus_coef_only$coef,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
  
  spplot(legacy_raster,#scales = list(draw = TRUE),
         at=break_legacy,
         asp=1,
         col.regions =
           rev(heat_hcl(length(break_legacy)-1)),
         main="Legacy effects") +
    latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))
  
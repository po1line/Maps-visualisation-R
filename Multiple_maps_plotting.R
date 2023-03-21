#install packages 
install.packages('ggplot2')
install.packages('ggpubr')
install.packages('data.table')
install.packages('dplyr')
install.packages('raster')
install.packages('sp')
library(ggplot2)
library(ggpubr)
library(data.table)
library(dplyr)
library(raster)
library(sp)

# read the raster data of future predictions
cnrm_126 <- raster('fut_7km_CNRM-CM6-1_126.tif')
can_126 <- raster('fut_7km_CanESM5_126.tif')
bcc_126 <- raster('fut_7km_BCC-CSM2-MR_126.tif')


cnrm_585 <- raster('fut_7km_CNRM-CM6-1_585.tif')
can_585 <- raster('fut_7km_CanESM5_585.tif')
bcc_585 <- raster('fut_7km_BCC-CSM2-MR_585.tif')

#function to transform the raster data to dataframe
to_df <- function(sp_data){
  as.data.frame(sp_data, xy = TRUE)
}

# transform the raster data to dataframes
cnrm_126_df <- to_df(cnrm_126)
can_126_df <- to_df(can_126)
bcc_126_df <- to_df(bcc_126)

cnrm_585_df <- to_df(cnrm_585)
can_585_df <- to_df(can_585)
bcc_585_df <- to_df(bcc_585)

#adding prediction values to dataframes and removing string column
cnrm_126_df$values <- getValues(cnrm_126)
cnrm_126_df <- cnrm_126_df[,-3]

can_126_df$values <- getValues(can_126)
can_126_df <- can_126_df[,-3]

bcc_126_df$values <- getValues(bcc_126)
bcc_126_df <- bcc_126_df[,-3]

cnrm_585_df$values <- getValues(cnrm_585)
cnrm_585_df <- cnrm_585_df[,-3]

can_585_df$values <- getValues(can_585)
can_585_df <- can_585_df[,-3]

bcc_585_df$values <- getValues(bcc_585)
bcc_585_df <- bcc_585_df[,-3]

#creation of combined dataset
list_future_maps <- list(cnrm_126_df=cnrm_126_df, 
                         can_126_df=can_126_df, 
                         bcc_126_df=bcc_126_df, 
                         cnrm_585_df=cnrm_585_df,
                         can_585_df=can_585_df, 
                         bcc_585_df=bcc_585_df)


ID <- names(list_future_maps) #setting the ID for data from each dataset
ID_future <- c(cnrm_126_df='CNRM-CM6-1-SSP126', 
               can_126_df='CanESM5-SSP126', 
               bcc_126_df='BCC-CSM2-MR-SSP126', 
               cnrm_585_df='CNRM-CM6-1-SSP585', 
               can_585_df='CanESM5-SSP585', 
               bcc_585_df='BCC-CSM2-MR-SSP585') 
list_future_maps <- mapply(cbind, list_future_maps, "dataset"=ID, SIMPLIFY=F)
future_maps_df <- as.data.frame(rbindlist(l = list_future_maps)) #one dataset
table(future_maps_df$dataset)

#setting color palettes for maps
my_colors <-  c("#002624", "#366272", "#6b9dc0", "#b5c2d9","#ffe6f1" )
my_colors_1 <-c("#e9d7b8", "#e8c9b3", "#c59ca4", "#a48d9e", "#606d94")
my_colors_2 <-c( "#75efdd", "#549188","#333333", "#995e5e","#ff8888" )
my_colors_3 <- c( "#008fbf", "#47b0df",
                  "#dae695", '#e6ae95', 
                  "#df7b7b", "#a00000" )

#plotting maps in one image
ggplot() +
  geom_raster(data = future_maps_df , aes(x = x, y = y, fill = values)) +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "white") +
  scale_fill_gradientn (colours = my_colors_3, na.value = "white", limits = c(0,1))+
  coord_quickmap()+
  theme_classic(base_size = 12, base_family = "Georgia")+theme(legend.position = "bottom")+
  facet_wrap(~dataset,  labeller=labeller(dataset=ID_future))

#plotting density functions
ggplot(future_maps_df, aes(x=values, fill=dataset)) + 
  geom_density(alpha = .4)+
  facet_wrap(~dataset, labeller=labeller(dataset=ID_future))+
  theme_classic(base_size = 14, base_family = "Georgia")+ 
  labs(#fill='Distance', 
    title='Density plots of the values of future predictions', alpha=NULL)+
  theme(legend.position = "none")+
  ylab('Density')+
  xlab('Future prediction value')+
  geom_vline(data=mean_prediction_value_future, 
             aes(xintercept=mean_value, color=dataset),
             linetype="dashed")+
  scale_fill_manual('Model',labels = c('cnrm_126_df'='CNRM-CM6-1-SSP126', 
                                       'can_126_df'='CanESM5-SSP126', 
                                       'bcc_126_df'='BCC-CSM2-MR-SSP126', 
                                       'cnrm_585_df'='CNRM-CM6-1-SSP585', 
                                       'can_585_df'='CanESM5-SSP585', 
                                       'bcc_585_df'='BCC-CSM2-MR-SSP585') ,
                    values = c('cnrm_126_df'="#003f5c", 
                               'can_126_df'="#58508d",
                               'bcc_126_df'="#bc5090", 
                               'cnrm_585_df'="cyan3", 
                               'can_585_df'="#ff6361",  
                               'bcc_585_df'='#ffa600'))+
  scale_color_manual('Model',labels = c('cnrm_126_df'='CNRM-CM6-1-SSP126', 
                                        'can_126_df'='CanESM5-SSP126', 
                                        'bcc_126_df'='BCC-CSM2-MR-SSP126', 
                                        'cnrm_585_df'='CNRM-CM6-1-SSP585', 
                                        'can_585_df'='CanESM5-SSP585', 
                                        'bcc_585_df'='BCC-CSM2-MR-SSP585') ,
                     values = c('cnrm_126_df'="#003f5c", 
                                'can_126_df'="#58508d",
                                'bcc_126_df'="#bc5090", 
                                'cnrm_585_df'="cyan3", 
                                'can_585_df'="#ff6361",  
                                'bcc_585_df'='#ffa600'))+ 
  theme(plot.title = element_text(family='Arial'))


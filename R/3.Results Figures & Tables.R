#script for generating plots and tables to present results 
pacman::p_load(tidyverse,tidycensus,sf,tmap,tigris,spdep,spatialreg,rnaturalearth,stargazer,ggthemes,dotwhisker)
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
library(ggmap)


#start with percent poverty map and other demographic maps
pdx_parks_tracts <- pdx_parks_tracts %>% mutate(pop_density=ifelse(pop_density==0.1,NA,pop_density),
                                    med_income=ifelse(med_income==0.1,NA,med_income),
                                    park_acres=ifelse(park_acres==0.1,NA,park_acres),
                                    pct_poverty=ifelse(pct_poverty==0,NA,pct_poverty),
                                    pct_bachelors_plus=ifelse(pct_bachelors_plus==0,NA,pct_bachelors_plus),
                                    total_pop=ifelse(total_pop==0,NA,total_pop))


coords<- cbind(pdx_street_trees,st_coordinates(st_point_on_surface(pdx_street_trees$geometry)))

percent_pov_map<-
  ggplot() +
   #geom_sf(data = neighborhoods, fill = "grey65", color = "white") +  
   geom_sf(data=test,aes(fill=pct_poverty),alpha=.85,color="white") +
  labs(x="Longitude",y="Latitude",fill="% Poverty") +
  #scale_fill_viridis_b(direction = 1)+
  scale_fill_fermenter(palette = "Purples",n.breaks=5,direction = 1) +
  coord_sf() +
  theme_few()
pop_density_map  
  ?scale_fill_viridis_b
intro_grid<-gridExtra::grid.arrange(street_trees_map,hh_income_map,percent_pov_map,percent_bachelors_map)  
ggsave(intro_grid,filename = "/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Figures/intro_grid.png",width = 10,height = 6.5,dpi = 300)

pop_grid <- gridExtra::grid.arrange(total_pop_map,pop_density_map)
ggsave(pop_grid,filename = "/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Figures/pop_grid.png",height = 8,width = 7,dpi = 300)

#density map 
tree_density_map<-
  base_tract_map +
 #geom_point(data=coords,aes(x=X,y=Y),color="yellow") +
  stat_density2d(
    aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
    size = 0.1, bins = 40, data = coords,
    geom = "polygon"
  ) +
  geom_density2d(data = coords,
                 aes(x = X, y = Y), size = 0.3,
                 color="#74c476") +
  scale_fill_fermenter(palette = "Greens",direction = 1) +
    labs(fill="Tree Density")+
  guides(alpha="none")+
 # scale_color_manual(values = "#74c476") +
  # scale_fill_viridis_c(option = "A",alpha = .89) +
  theme_few()
trees_grid <- gridExtra::grid.arrange(street_trees_map,tree_density_map)

ggsave(trees_grid,filename = "/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Figures/tree_density_grid.png",width=8,height = 7,dpi=300)

ggplot() +
  geom_sf(data = pdx_parks_tracts, fill = "grey65",alpha=.5, color = "white") +  
  geom_sf(data=st_centroid(pdx_parks_tracts))

#reporting regression results 
stargazer(tree_tracts.ols_v2, trees_tracts.slm_v2, trees_tracts.sem,type="latex", 
          title="Model Results",out = "reg_table_1.tex")


#residuals mapped  
pdx_parks_tracts_fil <- pdx_parks_tracts %>% filter(GEOID!=41051980000)
pdx_parks_tracts_fil$ols_pred <- predict(tree_tracts.ols_v2)
pdx_parks_tracts_fil$slm_pred <- predict(trees_tracts.slm_v2)
pdx_parks_tracts_fil$sem_pred <- predict(trees_tracts.sem)


ols_pred_map<-
  ggplot() +
  #geom_sf(data = neighborhoods, fill = "grey65", color = "white") +  
  geom_sf(data=pdx_parks_tracts_fil,aes(fill=ols_pred),alpha=.85,color="white") +
  labs(x="Longitude",y="Latitude",fill="OLS\nPredicted") +
  #scale_fill_viridis_b(direction = 1)+
  scale_fill_fermenter(palette = "RdBu",n.breaks=5,direction = -1) +
  coord_sf() +
  theme_few()

resid_maps <- gridExtra::grid.arrange(ols_resid_map,slm_resid_map,sem_resid_map)
pred_maps <- gridExtra::grid.arrange(ols_pred_map,slm_pred_map,sem_pred_map)

ggsave(pred_maps,filename="/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Figures/pred_maps.png",width = 8,height = 8,dpi=300)

#script for spatial analysis and modeling 
#load required packages
pacman::p_load(tidyverse,tidycensus,sf,tmap,tigris,spdep,spatialreg,rnaturalearth,stargazer,spatialRF,Imap)

#assumes carrying over of data from 1.data_processing script

#map the variables of interest 
pdx_parks_tracts <- pdx_parks_tracts %>% st_as_sf()
tmap_mode("plot")
tm_shape(pdx_parks_tracts) +
  tm_fill("pct_unemployed",palette="PRGn",style="quantile",
          title = "",alpha = .85,n=3) +
  tm_borders(alpha=.7)

plot((pdx_parks_tracts_2$med_income),log(pdx_parks_tracts_2$street_trees_per_mile))

plot((pdx_parks_tracts$city_center_dist),log(pdx_parks_tracts$street_trees_per_mile))
plot(lm(log(street_trees_per_mile)~med_income*city_center_dist+pct_poverty*city_center_dist,data=pdx_parks_tracts))
#assess spatial autocorrelation -----
#first examine queen and rook criteria 
tracts_nb_queen <- poly2nb(pdx_parks_tracts,queen = T)
tracts_nb_queen <- poly2nb(pdx_parks_tracts_2,queen = T)

tracts_nb_rook <- poly2nb(pdx_parks_tracts,queen = F)
#examine knn criteria 
tracts_knn <- knearneigh(pdx_parks_tracts_points,k=5)
tracts_nb_knn <- knn2nb(tracts_knn)

#look at spatial neighbors structure by mapping
plot(pdx_parks_tracts$geometry,border="grey")
plot.nb(tracts_nb_queen,coords=st_geometry(pdx_parks_tracts), add=TRUE, col='green')
plot.nb(tracts_nb_rook,coords=st_geometry(pdx_parks_tracts), add=TRUE, col='purple')
legend("bottomleft",legend=c("Queen","Rook"),
       fill=c("green","purple"),bty="n")
#convert to listw object 
listw <- nb2listw(tracts_nb_queen)
knn_listw <- nb2listw(tracts_nb_knn)

#run global moran test and plot 
moran.test(pdx_parks_tracts$street_trees_per_mile,knn_listw) #0.411 

moran <- moran.plot(pdx_parks_tracts$n_street_trees, listw = nb2listw(tracts_nb_queen, style = "W"))

moran_mc <- moran.mc(pdx_parks_tracts_2$street_trees_per_mile,listw,nsim = 999,alternative = "two.sided")
plot(moran_mc)

#run local moran test and map 
local_m <- localmoran(x = pdx_parks_tracts$street_trees_per_mile,
                      listw = knn_listw)

maran_map <- cbind(pdx_parks_tracts,local_m)

tm_shape(maran_map) + tm_fill(col = "Ii", style = "quantile",
                              title = "Local Moran Statistic",alpha=0.8)

quadrant <- vector(mode="numeric",length=nrow(local_m))
# centers the variable of interest around its mean
m_qualification <- pdx_parks_tracts$street_trees_per_mile - mean(pdx_parks_tracts$street_trees_per_mile)
# centers the local Moran's around the mean
m_local <- local_m[,1] - mean(local_m[,1])
# significance threshold
signif <- 0.1
# builds a data quadrant
quadrant[m_qualification >0 & m_local>0] <- 4
quadrant[m_qualification <0 & m_local<0] <- 1
quadrant[m_qualification <0 & m_local>0] <- 2
quadrant[m_qualification >0 & m_local<0] <- 3
view(quadrant)
# plot
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(pdx_parks_tracts$geometry,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
legend("bottomleft",legend=c("Insignificant","Low-Low","Low-High","High-Low","High-High"),
       fill=colors,bty="n")

#choose best model for data ----

#start with OLS 
pdx_parks_tracts <- pdx_parks_tracts %>% mutate(pop_density=ifelse(pop_density==0,0.1,pop_density),
                                                med_income=ifelse(med_income==0,0.1,med_income),
                                                park_acres=ifelse(park_acres==0,0.1,park_acres))

plot(log(pdx_parks_tracts$n_street_trees),log(pdx_parks_tracts$pop_density))
tree_tracts.ols <- lm(log(street_trees_per_mile)~pop_density+med_income+Sextant+n_parks+
                        med_age+pct_white+pct_black+pct_asian+pct_hispanic+pct_poverty+pct_less_than_hs+pct_HS_grad+
                        pct_bachelors_plus,data = pdx_parks_tracts)

tree_tracts.ols_v2 <- lm(log(street_trees_per_mile)~pop_density*log(med_income)*log(city_center_dist)+Sextant+n_parks+
                           med_age*log(city_center_dist)+pct_white+pct_black+pct_hispanic+pct_poverty+
                           pct_bachelors_plus+pct_unemployed,data = pdx_parks_tracts,na.action = na.omit)
summary(tree_tracts.ols_v2)

tree_tracts.ols_v3 <- lm(log(street_trees_per_mile)~pop_density*log(med_income)*log(city_center_dist)+Sextant+n_parks+
                           med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
                           pct_bachelors_plus+pct_unemployed+LENTS,data = pdx_parks_tracts_2,na.action = na.omit)
summary(tree_tracts.ols_v3)

AIC(tree_tracts.ols_v3)

tree_tracts.ols_v4 <- lm(log(street_trees_per_mile)~pop_density*city_center_dist*med_income+city_center_dist*pct_bachelors_plus+Sextant+n_parks+
                           med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
                           pct_unemployed+LENTS,data = pdx_parks_tracts_2,na.action = na.omit)
summary(tree_tracts.ols_v4)

pdx_parks_tracts_2_fil <- pdx_parks_tracts_2 %>% filter(GEOID!=41051980000) #removed due to unpopulated tract
tracts_queen_fil <- poly2nb(pdx_parks_tracts_2_fil,queen = T)
listw_fil <- nb2listw(tracts_queen_fil)

mc<-moran.mc(tree_tracts.ols_v3$residuals,listw_fil,nsim = 999,alternative = "two.sided")
mc
plot(mc)

regions_parks <- regions_parks %>% 
  mutate(pop_density=ifelse(pop_density==0,0.1,pop_density),
         med_income=ifelse(med_income==0,0.1,med_income),
         park_acres=ifelse(park_acres==0,0.1,park_acres))


cor.test(pdx_parks_tracts$street_trees_per_mile,log(pdx_parks_tracts$veg_area),na.action=na.exclude)

veg_area.ols <- lm(log(veg_area_per_mile)~pop_density*log(med_income)*log(city_center_dist)+Sextant+n_parks+
                     med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
                     pct_bachelors_plus+pct_unemployed+LENTS,data = pdx_parks_tracts_2)
summary(veg_area.ols)

#Use spatial Random Forest to optimize variable selection and further assess spatial autocorrelation ----

#start with function to create a distance matrix for all census tract centroid points
ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}
GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}
trees_coords<- cbind(pdx_parks_tracts_points_fil2,st_coordinates(st_point_on_surface(pdx_parks_tracts_points_fil2$geometry)))

tracts_coords <- trees_coords %>% select("lat"=Y,"lon"=X,"name"=GEOID)
trees_coords <- trees_coords %>% select("lon"=X,"lat"=Y,"name"=OBJECTID)

distance_tract_mat <- GeoDistanceInMetresMatrix(tracts_coords)
distance.thresholds <- c(0, 1000, 2000, 4000, 8000,10000)
random.seed <- 1
pdx_parks_tracts_points_fil2 <- na.omit(pdx_parks_tracts_points)
pdx_parks_tracts_points_fil2 <- pdx_parks_tracts_points_fil2 %>% mutate(log_street_trees_per_mile = log(street_trees_per_mile))
dependent_var <- "log_street_trees_per_mile"
predictors <- c("pop_density","med_income","city_center_dist","n_parks","med_age",
                  "pct_white","pct_black","pct_hispanic","pct_poverty",
                  "pct_bachelors_plus","pct_unemployed")
# pop_density*log(med_income)*log(city_center_dist)+Sextant+n_parks+
#   med_age*log(city_center_dist)+pct_white+pct_black+pct_hispanic+pct_poverty+
#   pct_bachelors_plus+pct_unemployed
xy<-tracts_coords[,c("lat","lon")] %>% st_drop_geometry() %>% 
  select("x"=lon,"y"=lat)

spatialRF::plot_training_df(
  data = pdx_parks_tracts_points_fil2,
  dependent.variable.name = dependent_var,
  predictor.variable.names = predictors,
  ncol = 3,
  point.color = viridis::viridis(100, option = "F"),
  line.color = "gray30"
)

spatialRF::plot_training_df_moran(
  data = pdx_parks_tracts_points_fil2,
  dependent.variable.name = dependent_var,
  predictor.variable.names = predictors,
  distance.matrix = distance_tract_mat,
  distance.thresholds = distance.thresholds,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1
  ),
  point.color = "gray40"
) +
  ylab("Model Variables")
ggsave("variable_morans_index.png",height=7,width = 7,dpi=300)

preference.order <- c(
  "pct_poverty",
  "med_income",
  "city_center_dist",
  "med_income",
  "med_age",
  "pop_density"
)

predictor.variable.names <- spatialRF::auto_cor(
  x = pdx_parks_tracts_points_fil2[, predictors],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>%
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )
predictor.variable.names$vif

interactions <- spatialRF::the_feature_engineer(
  data = pdx_parks_tracts_points_fil2,
  dependent.variable.name = dependent_var,
  predictor.variable.names = predictors,
  xy = xy,
  importance.threshold = 0.50, #uses 50% best predictors
  cor.threshold = 0.60, #max corr between interactions and predictors
  seed = random.seed,
  repetitions = 100,
  verbose = TRUE
)
pdx_tracts_points_interactions <- interactions$data
predictor.variable.names <- interactions$predictor.variable.names
model.non.spatial <- spatialRF::rf(
  data = pdx_tracts_points_interactions,
  dependent.variable.name = dependent_var,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_tract_mat,
  distance.thresholds = distance.thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random.seed,
  verbose = FALSE
)

spatialRF::plot_residuals_diagnostics(
  model.non.spatial,
  verbose = FALSE
)

spatialRF::plot_importance(
  model.non.spatial,
  verbose = FALSE
)

model.non.spatial <- spatialRF::rf_importance(
  model = model.non.spatial
)

model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random.seed
)
spatialRF::plot_moran(
  model.spatial,
  verbose = FALSE
)

#run lagrange multiplier test ----
lm.LMtests(tree_tracts.ols_v2, listw, test="all", zero.policy = T) # spatial lag is best in this case 

#sensitivity analysis ----

# empty vector for storage of Moran's I values
moran_I <- c()
trees.nb <- spdep::dnearneigh(as.matrix(pdx_parks_tracts[,c(n_street_trees, pop_density)]), d1 = 0, d2 = 200)

coords <- st_centroid(st_geometry(pdx_parks_tracts), of_largest_polygon=TRUE)

k1 <- knn2nb(knearneigh(coords))
rn <- row.names(pdx_parks_tracts)
all.linked <- max(unlist(nbdists(tracts_nb_knn, coords)))
trees.nb.0.all <- dnearneigh(as.matrix(st_centroid(pdx_parks_tracts$geometry)), d1=0,d2=200)

for (k in seq(1, 10, 1)) {
  tracts_knn <- knearneigh(pdx_parks_tracts_points_fil,k=k)
  tracts_nb_knn <- knn2nb(tracts_knn)
  knn_listw <- nb2listw(tracts_nb_knn)
  moran <- moran.mc(tree_tracts.ols_v2$residuals, knn_listw, nsim = 999, zero.policy = TRUE)
  moran_I <- c(moran_I, moran$statistic)
}

pdx_parks_tracts_points_fil <- pdx_parks_tracts_points %>% filter(GEOID!=41051980000)
tracts_knn_fil <- knearneigh(pdx_parks_tracts_points_fil,k=5)
tracts_nb_knn_fil <- knn2nb(tracts_knn_fil)
plot(moran_mc)
moran_I <- data.frame(moran = moran_I, 
                      k = seq(1, 10, 1))

ggplot(moran_I, aes(x = k, y = moran)) + 
  geom_point() +
  geom_line()

moran.mc(trees_tracts.sem$residuals,knn_listw,nsim = 999,alternative = "greater")


#spatial lag model ----
trees_tracts.slm <- lagsarlm(log(n_street_trees)~log(pop_density)+tract_area+n_parks+med_age+log(med_income)+pct_white+pct_black+
                               pct_asian+pct_hispanic+pct_poverty+pct_less_than_hs+pct_HS_grad+
                               pct_bachelors_plus,data = pdx_parks_tracts,listw = listw,zero.policy = T)

veg_tracts.slm <- lagsarlm(log(veg_area_per_mile)~pop_density*log(med_income)+log(city_center_dist)+Sextant+n_parks+
                             med_age+pct_white+pct_black+pct_hispanic+pct_poverty+log(street_trees_per_mile)+
                             pct_bachelors_plus+pct_unemployed+LENTS,data=pdx_parks_tracts_2,listw = listw)
summary(veg_tracts.slm)
# log(veg_area)~pop_density*log(med_income)*log(city_center_dist)+Sextant+n_parks+
#   med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
#   pct_bachelors_plus+pct_unemployed+LENTS
#median income not stat signif for street tree coverage but is for green space area 
trees_tracts.slm_v2 <- lagsarlm(log(street_trees_per_mile)~pop_density*log(med_income)*log(city_center_dist)+Sextant+n_parks+
                                  med_age*log(city_center_dist)+pct_white+pct_black+pct_hispanic+pct_poverty+
                                  pct_bachelors_plus+pct_unemployed,data = pdx_parks_tracts,listw = listw,zero.policy = T)

trees_tracts.slm_v3 <- lagsarlm(log(street_trees_per_mile)~pop_density*log(med_income)*log(city_center_dist)+Sextant+n_parks+
                                  med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
                                  pct_bachelors_plus+pct_unemployed+LENTS,data = pdx_parks_tracts_2,listw = listw,zero.policy = T)

trees_tracts.slm_v4 <- lagsarlm(log(street_trees_per_mile)~log(pop_density)*city_center_dist*log(med_income)+city_center_dist*pct_bachelors_plus+Sextant+n_parks+
                                  med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
                                  pct_unemployed+LENTS,data = pdx_parks_tracts_2,listw = listw,zero.policy = T)

trees_tracts_gstsls <- gstsls(log(street_trees_per_mile)~pop_density*city_center_dist+med_income*city_center_dist+Sextant+n_parks+med_age+pct_white+pct_black+
                                pct_asian+pct_hispanic+pct_poverty+pct_less_than_hs+pct_HS_grad+
                                pct_bachelors_plus,data = pdx_parks_tracts,listw = knn_listw,zero.policy = T)

summary(trees_tracts.slm_v3)
W <- as(knn_listw,"CsparseMatrix")
trMC <- trW(W,type = "MC")
impacts <- impacts(trees_tracts.slm,tr=trMC,R=100)
sums <- summary(impacts,zstats=T)
data.frame(sums$pzmat)


veg_tracts.sem <- errorsarlm(log(veg_area)~log(pop_density)+log(med_income)+city_center_dist+Sextant+
                               med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
                               pct_bachelors_plus+pct_unemployed+LENTS,data=pdx_parks_tracts_2,listw = listw,zero.policy = T)

trees_tracts.sem <- errorsarlm(log(street_trees_per_mile)~log(pop_density)*city_center_dist*log(med_income)+city_center_dist*pct_bachelors_plus+Sextant+n_parks+
                                 med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
                                 pct_unemployed+LENTS,data = pdx_parks_tracts_2,listw = listw,zero.policy = T)

trees_tracts.sem_v2 <- errorsarlm(log(street_trees_per_mile)~pop_density*log(med_income)*city_center_dist+forcats::fct_shuffle(Sextant)+n_parks+
                                    med_age+pct_white+pct_black+pct_hispanic+pct_poverty+
                                    pct_bachelors_plus+pct_unemployed+LENTS,data = pdx_parks_tracts_2,listw = listw,zero.policy = T)
summary(trees_tracts.sem_v2)

plot(trees_tracts.slm$residuals)

moran.mc(trees_tracts.slm_v3$residuals,listw_fil,nsim = 999,alternative = "two.sided")
#model comparison ----

moran_I_lm <- c()
moran_I_slm <- c()
moran_I_sem <- c()

# loop through a distance vector d ranging from 50 to 2000 m
for (k in seq(1, 10, 1)) {
  tracts_knn <- knearneigh(pdx_parks_tracts_points_fil,k=k)
  tracts_nb_knn <- knn2nb(tracts_knn)
  knn_listw <- nb2listw(tracts_nb_knn)
  moran_lm <- moran.mc(tree_tracts.ols_v2$residuals, knn_listw, nsim = 999, zero.policy = TRUE)
  moran_I_lm <- c(moran_I, moran$statistic)
  
  moran_slm <- moran.mc(trees_tracts.slm_v2$residuals, knn_listw, nsim = 999, zero.policy = TRUE)
  moran_I_slm <- c(moran_I_slm, moran_slm$statistic)
  
  moran_sem <- moran.mc(trees_tracts.sem$residuals, knn_listw, nsim = 999, zero.policy = TRUE)
  moran_I_sem <- c(moran_I_sem, moran_sem$statistic)
}

moran_I_lm  <- data.frame(moran_I = moran_I_lm, 
                          k = seq(1, 10, 1), 
                          model="Simple linear model")

moran_I_slm <- data.frame(moran_I = moran_I_slm, 
                          k = seq(1, 10, 1), 
                          model="Spatial lag model")

moran_I_sem <- data.frame(moran_I = moran_I_sem, 
                          k = seq(1, 10, 1), 
                          model="Spatial error model")

moran_I_lm <- moran_I_lm %>% select("moran_I"=moran_I.moran,-moran_I.k,-moran_I.statistic,k,model)
moran.df <- rbind(moran_I_lm, moran_I_slm, moran_I_sem)

ggplot(moran.df, aes(x = k, y = moran_I, col=model)) + 
  geom_point() +
  geom_line()



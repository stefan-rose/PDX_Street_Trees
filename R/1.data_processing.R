#script for data cleaning and exploratory data analysis 
pacman::p_load(tidyverse,tidycensus,sf,tmap,tigris)

#load data sources ----

#main data files included are city boundary, city regions, city neighborhoods, street trees file, and metro area parks and vegetation areas 
#these files can be downloaded here: https://gis-pdx.opendata.arcgis.com
pdx <- st_read("/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Data/City_Boundaries")
pdx_regions <- st_read("/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Data/Portland_Administrative_Sextants")
pdx_street_trees <- st_read("/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Data/Street_Trees")
neighborhoods <- st_read("/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Data/Neighborhoods_(Regions)")
orcas <- st_read("/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Data/ORCA_Sites")
OR_tracts <- tracts(state="OR",cb=T)

#Portland redlines can be downloaded here: https://dsl.richmond.edu/panorama/redlining/#loc=5/39.1/-94.58&text=downloads
redlines <- st_read("/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Data/ORPortland1937")


v19 <- load_variables("2019",dataset = "acs5")
#median hh income = B19013_001, total population = B01003_001, median age = B01002_001, race - total pop = B02001_001, 
#white alone = B02001_002, black alone = B02001_003, asian alone = B02001_005, two or more races = B02001_008, hispanic population = B03003_003,
#poverty status total = B17020_001, total education attainment = B23006_001, less than high school grad = B23006_002, high school grad = B23006_009,
#some college = B23006_016, bachelor's or higher = B23006_023
pdx_acs <- get_acs(geography = "tract",
                   year=2019,
                   state = "OR",
                   variables = c(total_pop = "B01003_001",med_income = "B19013_001",med_age = "B01002_001",
                                 race_pop = "B02001_001",white_pop = "B02001_002",black_pop="B02001_003",asian_pop="B02001_005",
                                 multiracial_pop = "B02001_008",hispanic_pop = "B03003_003",poverty_pop = "B17020_001",poverty="B17020_002",
                                 total_edu_pop = "B23006_001", less_than_HS = "B23006_002",HS_grad = "B23006_009",
                                 some_college="B23006_016",bachelors_plus = "B23006_023",employment_total="B23025_002",unemployed="B23025_005"))

#get acs data and convert to percent where possible 
pdx_acs <- pdx_acs %>% select(-moe) %>% pivot_wider(names_from = "variable",values_from = "estimate") %>% 
  mutate(pct_white = white_pop/race_pop,
         pct_black = black_pop/race_pop,
         pct_asian = asian_pop/race_pop,
         pct_multiracial=multiracial_pop/race_pop,
         pct_hispanic = hispanic_pop/race_pop,
         pct_poverty = poverty/poverty_pop,
         pct_less_than_hs = less_than_HS/total_edu_pop,
         pct_HS_grad = HS_grad/total_edu_pop,
         pct_some_college = some_college/total_edu_pop,
         pct_bachelors_plus = bachelors_plus/total_edu_pop)

pdx_employment <- get_acs(geography = "tract",
                          year=2019,
                          state = "OR",
                          variables = c(employment_total="B23025_002",unemployed="B23025_005")) #%>% 
pdx_employment<- pdx_employment %>%   select(-moe) %>% 
  pivot_wider(names_from = "variable",values_from = "estimate") %>% 
  mutate(pct_unemployed = unemployed/employment_total) 


orcas <- st_make_valid(orcas)
orcas <- st_transform(orcas,crs=4326)
orcas <- orcas %>% filter(DISSOLVEID!=2303&DISSOLVEID!=1099&DISSOLVEID!=1236&DISSOLVEID!=2094) #omit non-parks and some parks out of bounds

pdx <- st_transform(pdx,crs=4326)

pdx <- pdx %>% st_buffer(dist=0)
pdx <- st_make_valid(pdx)

pdx_center_pt <- st_centroid(pdx) %>% st_make_valid()

OR_tracts <- st_transform(OR_tracts,crs=4326)

#exclude tracts on the very edge of city boundary and crop ---- 
OR_tracts <- OR_tracts %>% filter(TRACTCE!="007100"&TRACTCE!="031508"&TRACTCE!="030102"&TRACTCE!="030101"&TRACTCE!="030300"&TRACTCE!="030402"&
                                    TRACTCE!="030501"&TRACTCE!="030502"&TRACTCE!="030600"&TRACTCE!="030700"&TRACTCE!="020304"&TRACTCE!="020303"&
                                    TRACTCE!="020100"&TRACTCE!="020800"&TRACTCE!="020900"&TRACTCE!="021000"&TRACTCE!="021601"&TRACTCE!="022201"&
                                    TRACTCE!="022206"&TRACTCE!="022208"&TRACTCE!="009903"&TRACTCE!="009803"&TRACTCE!="009606"&TRACTCE!="009605"&
                                    TRACTCE!="010200")

#load city vegetation area nad convert units 
pdx_veg <- st_read("/Users/rose/Desktop/Projects/Personal/Portland Parks Access/Data/Vegetation")
pdx_veg <- pdx_veg %>% mutate(vegetation_area = st_area(geometry),
                              vegetation_area = vegetation_area*0.00000038610215855,
                              vegetation_area=str_sub(vegetation_area,end = -4),
                              vegetation_area=as.numeric(vegetation_area))

#crop parks areas to Portland City Boundary
orcas_pdx <- orcas[lengths(st_intersects(orcas,pdx))==1,] 
tmap_mode("view")
tm_shape(pdx_center_pt) +
  tm_dots() +
  tm_shape(pdx_tracts) +
  tm_borders(lwd=2)

#crop tracts to Portland city boundaries 
pdx_tracts <- OR_tracts[lengths(st_intersects(OR_tracts,pdx))==1,]

#join acs data to portland tracts ----
pdx_tracts <- pdx_tracts %>% left_join(pdx_acs,by="GEOID") %>% st_as_sf()

#join parks as centroid points to census tracts 
orcas_pdx_points <- orcas_pdx %>% st_centroid() %>% st_make_valid()

pdx_street_trees <- pdx_street_trees %>% st_transform(crs = 4326)

redline_points <- redlines %>% filter(holc_grade=="C"|holc_grade=="D") %>% 
  st_centroid() %>% st_make_valid() %>% st_transform(crs=4326)

neighborhoods_points <- neighborhoods %>% filter(NAME=="LENTS") %>% 
  st_centroid() %>% st_make_valid() %>% st_transform(crs=4326)

pdx_parks_tracts <- pdx_tracts %>% st_join(orcas_pdx_points,left = T) %>% 
#  filter(GEOID != 41051980000) %>%  #removed because unpopulated census tract
  group_by(GEOID) %>% 
  summarise(n_parks=n_distinct(SITENAME),
            park_acres = sum(ACREAGE,na.rm = T),
            total_pop = first(total_pop),
            med_age = first(med_age),
            med_income=first(med_income),
            pct_white=first(pct_white)*100,
            pct_black=first(pct_black)*100,
            pct_asian=first(pct_asian)*100,
            pct_multiracial=first(pct_multiracial)*100,
            pct_hispanic=first(pct_hispanic)*100,
            pct_poverty=first(pct_poverty)*100,
            pct_less_than_hs=first(pct_less_than_hs)*100,
            pct_HS_grad=first(pct_HS_grad)*100,
            pct_some_college=first(pct_some_college)*100,
            pct_bachelors_plus=first(pct_bachelors_plus)*100) %>% 
  ungroup() %>% 
  mutate(tract_area = st_area(.),
         tract_area = tract_area*0.00000038610215855) %>% 
  mutate(tract_area=str_sub(tract_area,end = -4)) %>% 
  mutate(tract_area = as.numeric(tract_area),
         pop_density = total_pop/tract_area)



pdx_parks_tracts <- pdx_parks_tracts %>% st_join(pdx_street_trees,left=T) %>% 
  filter(Condition != "Dead") %>%  #remove trees that are dead
  group_by(GEOID) %>% 
  summarise(n_street_trees=n_distinct(OBJECTID),
            n_parks=first(n_parks),
            park_acres = first(park_acres),
            total_pop = first(total_pop),
            pop_density = first(pop_density),
            tract_area = first(tract_area),
            med_age = first(med_age),
            med_income=first(med_income),
            pct_white=first(pct_white),
            pct_black=first(pct_black),
            pct_asian=first(pct_asian),
            pct_multiracial=first(pct_multiracial),
            pct_hispanic=first(pct_hispanic),
            pct_poverty=first(pct_poverty),
            pct_less_than_hs=first(pct_less_than_hs),
            pct_HS_grad=first(pct_HS_grad),
            pct_some_college=first(pct_some_college),
            pct_bachelors_plus=first(pct_bachelors_plus)) %>% 
  ungroup()

pdx_parks_tracts <- pdx_parks_tracts %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0.1)) %>% 
  mutate_if(is.numeric, ~replace(., is.nan(.), 0.1))

pdx_veg_points <- pdx_veg %>% st_centroid(pdx_veg) %>% 
  st_transform(crs=4326)
pdx_parks_tracts <- pdx_parks_tracts %>% st_join(pdx_veg_points,left = T) %>% 
  group_by(GEOID) %>% 
  summarise(n_street_trees=first(n_street_trees),
            n_parks=first(n_parks),
            park_acres = first(park_acres),
            veg_area = sum(vegetation_area),
            total_pop = first(total_pop),
            pop_density = first(pop_density),
            tract_area = first(tract_area),
            med_age = first(med_age),
            med_income=first(med_income),
            pct_white=first(pct_white),
            pct_black=first(pct_black),
            pct_asian=first(pct_asian),
            pct_multiracial=first(pct_multiracial),
            pct_hispanic=first(pct_hispanic),
            pct_poverty=first(pct_poverty),
            pct_less_than_hs=first(pct_less_than_hs),
            pct_HS_grad=first(pct_HS_grad),
            pct_some_college=first(pct_some_college),
            pct_bachelors_plus=first(pct_bachelors_plus)) %>% 
  ungroup() %>% 
  mutate(street_trees_per_mile = n_street_trees/tract_area,
         veg_area_per_mile = veg_area/tract_area,
         n_parks_per_mile = n_parks/tract_area)
#take tracts and join centroid points to neighborhoods and regions ----
pdx_parks_tracts_points <- pdx_parks_tracts %>% st_centroid()

neighborhood_parks <- neighborhoods %>% 
  st_transform(crs = 4326) %>% 
  st_join(pdx_parks_tracts_points,left=FALSE) %>% 
  st_make_valid() #%>% 
   # group_by(NAME) %>% 
   # summarise(n_street_trees=sum(n_street_trees),
   #           n_parks = sum(n_parks),
   #           park_acrs=sum(park_acres),
   #           total_pop = sum(total_pop),
   #           tract_area=sum(tract_area),
   #           pop_density=median(pop_density),
   #           med_age = median(med_age),
   #           med_income=median(med_income),
   #           pct_white=median(pct_white),
   #           pct_black=median(pct_black),
   #           pct_asian=median(pct_asian),
   #           pct_multiracial=median(pct_multiracial),
   #           pct_hispanic=median(pct_hispanic),
   #           pct_poverty=median(pct_poverty),
   #           pct_less_than_hs=median(pct_less_than_hs),
   #           pct_HS_grad=median(pct_HS_grad),
   #           pct_some_college=median(pct_some_college),
   #           pct_bachelors_plus=median(pct_bachelors_plus))

regions_parks <- pdx_regions %>% 
  st_transform(crs=4326) %>% 
  st_join(pdx_parks_tracts_points,left=FALSE) %>% 
  st_make_valid()

pdx_parks_tracts <- regions_parks %>% 
  st_drop_geometry() %>% 
  select(GEOID,Sextant) %>% 
  right_join(pdx_parks_tracts,by="GEOID")

pdx_parks_tracts_points <- pdx_parks_tracts_points %>% 
  mutate(city_center_dist = st_distance(pdx_center_pt,pdx_parks_tracts_points,by_element = T),
         city_center_dist = city_center_dist*0.000621371,
         city_center_dist=str_sub(city_center_dist,end = -4),
         city_center_dist=as.numeric(city_center_dist))

pdx_parks_tracts <- pdx_parks_tracts_points %>% 
  st_drop_geometry() %>% 
  select(GEOID,city_center_dist) %>% 
  right_join(pdx_parks_tracts,by="GEOID") %>% 
  st_as_sf()

pdx_parks_tracts <- pdx_employment %>% 
  select(GEOID,pct_unemployed) %>% 
  mutate(pct_unemployed=pct_unemployed*100) %>% 
  right_join(pdx_parks_tracts,by="GEOID")

pdx_parks_tracts <- pdx_parks_tracts %>% select(GEOID,"pct_unemployed"=pct_unemployed.x,everything()) %>% 
  select(-pct_unemployed.y)

pdx_parks_tracts_2 <- pdx_parks_tracts %>% 
  st_join(redline_points,left=T) %>% 
  st_make_valid() %>% 
  st_join(neighborhoods_points) %>% 
  st_make_valid() %>% 
  select(everything(),-OBJECTID,-COMMPLAN:-Shape_Area,"LENTS"="NAME") %>% 
  mutate(LENTS = if_else(LENTS=="LENTS",1,0)) %>% 
  mutate(LENTS=as.numeric(ifelse(is.na(LENTS),0,LENTS)))

pdx_parks_tracts <- pdx_parks_tracts %>% mutate(pct_unemployed=ifelse(is.nan(pct_unemployed),NA,pct_unemployed))
pdx_parks_tracts_points <- pdx_parks_tracts %>% st_centroid()

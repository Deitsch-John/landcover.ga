# functions for the landcover shiny app
# setwd("C:/Users/jfdei/OneDrive/Desktop/qGIS/raster.library")
# NLCD <- rast("nlcd_2019_land_cover_l48_20210604.IMG")
# 
# NLCD.ga <- mask(crop(NLCD, 
#                      vect(st_union(GeorgiaCounties))), vect(st_union(GeorgiaCounties)))
# 
# NLCD.ga.agg <- terra::aggregate(NLCD.ga, fact = 10, fun = modal)
# writeRaster(NLCD.ga.agg, filename = "test.IMG", datatype = "HFA")

landcover_map <- function(county){
  county.shape <- GeorgiaCounties %>%
    filter(COUNTYNAME==county)
  
  county.landcover <- mask(crop(NLCD, 
                                vect(county.shape)), vect(county.shape))
  
  county.points <- as.points(county.landcover)%>%
    st_as_sf()%>%
    mutate(Class = case_when(
      `NLCD Land Cover Class`%in% c("21", "22", "23", "24")~"Developed",
      `NLCD Land Cover Class`%in% c("41", "42", "43")~"Forest",
      `NLCD Land Cover Class`%in% c("81","82")~"Pasture/Crops",
      `NLCD Land Cover Class`%in% c("52", "90", "95")~"Shrubs and Wetlands",
      `NLCD Land Cover Class`=="11"~"Water"
    ))%>%
    filter(!is.na(Class))
  
  rast.temp <- terra::rast(ext(county.shape), nrows = 300, ncols = 300,
                           crs = "EPSG:26917")
  rast <- rasterize(vect(county.points), rast.temp, field = "Class")
  
  county.points %>%
    ggplot()+
    geom_sf(aes(color = Class))+
    geom_sf(data = county.shape, fill = NA,
            size = 0.8)+
    scale_fill_brewer(type = "qual", palette = "Set1", 
                      direction = -1, aesthetics = "color")+
    theme_void()+
    labs(color = "Land Cover Class")+
    guides(color = guide_legend(
      override.aes = list(shape = 15, size = 4)
    ))+
    theme(legend.position = "top")
  
}
landcover_table <- function(county){
  county.shape <- GeorgiaCounties %>%
    filter(COUNTYNAME==county)
  
  county.landcover <- mask(crop(NLCD, 
                                vect(county.shape)), vect(county.shape))
  
  county.points <- as.points(county.landcover)%>%
    st_as_sf()%>%
    st_drop_geometry()%>%
    mutate(Class = case_when(
      `NLCD Land Cover Class`%in% c("21", "22", "23", "24")~"Developed",
      `NLCD Land Cover Class`==41~"Deciduous Forest",
      `NLCD Land Cover Class`==42~"Evergreen Forest",
      `NLCD Land Cover Class`==43~"Mixed Forest",
      `NLCD Land Cover Class`%in% c("81","82")~"Pasture/Crops",
      `NLCD Land Cover Class`%in% c("52", "90", "95")~"Shrubs and Wetlands",
      `NLCD Land Cover Class`=="11"~"Water"
    ))%>%
    filter(!is.na(Class))
  
  total.len <- nrow(county.points)
  
  county.points %>%
    group_by(Class)%>%
    summarise(number.points = n())%>%
    mutate(`% Land Cover` = (number.points/total.len)*100)%>%
    dplyr::select(Class, `% Land Cover`)%>%
    arrange(-`% Land Cover`)%>%
    gt()%>%
    fmt_number(columns = c("% Land Cover"), decimals = 2)%>%
    tab_header(title = str_c(county, " County", sep = ""))
  
}
landcover_map_class <- function(county, classx){
  county.shape <- GeorgiaCounties %>%
    filter(COUNTYNAME==county)
  
  county.landcover <- mask(crop(NLCD, 
                                vect(county.shape)), vect(county.shape))
  
  county.points <- as.points(county.landcover)%>%
    st_as_sf()%>%
    mutate(Class = case_when(
      `NLCD Land Cover Class`%in% c("21", "22", "23", "24")~"Developed",
      `NLCD Land Cover Class`==41~"Deciduous Forest",
      `NLCD Land Cover Class`==42~"Evergreen Forest",
      `NLCD Land Cover Class`==43~"Mixed Forest",
      `NLCD Land Cover Class`%in% c("81","82")~"Pasture/Crops",
      `NLCD Land Cover Class`%in% c("52", "90", "95")~"Shrubs and Wetlands",
      `NLCD Land Cover Class`=="11"~"Water"
    ))%>%
    filter(!is.na(Class))%>%
    filter(Class == classx)
  
  county.points %>%
    ggplot()+
    geom_sf(color = "seagreen")+
    geom_sf(data = county.shape, fill = NA,
            size = 0.8)+
    theme_void()
}
county_location_map <- function(county){
  county.shape <- GeorgiaCounties %>%
    filter(COUNTYNAME==county)
  
  ggplot()+
    geom_sf(data = GeorgiaCounties, 
            fill = NA, 
            size = 0.7,
            color = "gray68")+
    geom_sf(data = st_union(GeorgiaCounties), 
            fill = NA, 
            size = 1.4,
            color = "Black")+
    geom_sf(data = county.shape, 
            fill = "firebrick2",
            color = NA)+
    theme_void()
}